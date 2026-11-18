;;;; mesh.lisp -- a node of the mesh: what comes in over links, what goes out, who is about, what was said.
;;;;
;;;; The radio is not here.  A LINK is whatever the app calls a connection (a number, say); the app gives
;;;; MESH-RECEIVE each frame a link delivers and the mesh hands frames to the app's SEND function, for one link
;;;; or every link but one.  What this node is told -- peers seen and gone, messages said -- MESH-RECEIVE
;;;; returns as events, (:peer PEER) (:message CHAT) (:leave PEER).
;;;;
;;;; As bitchat does: a public message and an announce are signed, and a message is shown only when its
;;;; signature checks against the key its sender announced (a sender's keys pinned on first sight); every
;;;; packet not ours and not for us goes on to the other links with its TTL one less (7 at most to start),
;;;; until it would reach 0; a packet seen before (sender, time, type, payload) is dropped; fragments (type
;;;; 0x20) are put back together, and what they made read as if it had come whole.

(in-package #:chirp)

(defstruct peer
  id                   ; hex of the 8-byte peer id
  (nickname "") noise signing (seen 0) (links '()))

(defstruct chat sender nickname text timestamp mine)

(defstruct (mesh (:constructor %make-mesh))
  identity
  (send (lambda (frame &key link except) (declare (ignore frame link except))))
  (clock #'unix-ms)
  (peers (make-hash-table :test 'equal))          ; hex id -> peer
  (messages '())                                  ; newest first
  (seen (make-hash-table :test 'equal))           ; dedup key -> when (ms)
  (assemblies (make-hash-table :test 'equal))     ; sender+fragment id -> (when total type . chunks)
  (links '())
  (announced 0)                                   ; when this node last announced (ms)
  (max-frame 512)                                 ; frames longer go as fragments
  (dropped 0))                                    ; packets not shown: unverifiable, malformed

(defun make-mesh (&key identity send (clock #'unix-ms) (max-frame 512))
  (%make-mesh :identity identity :send (or send (lambda (f &key link except) (declare (ignore f link except))))
              :clock clock :max-frame max-frame))

(defun unix-ms ()
  "Milliseconds since 1970 (seconds from the clock, the part of a second from the internal timer)."
  (+ (* 1000 (- (get-universal-time) 2208988800))
     (mod (floor (* 1000 (get-internal-real-time)) internal-time-units-per-second) 1000)))

(defparameter +ttl+ 7 "A packet's TTL as it starts.")
(defparameter +announce-every+ 30000 "How often (ms) this node announces itself again.")
(defparameter +seen-for+ 300000 "How long (ms) a packet is remembered, so it is not taken or relayed twice.")
(defparameter +assembly-for+ 30000 "How long (ms) a part-arrived set of fragments waits for the rest.")

(defun %my-id (mesh) (identity-peer-id (mesh-identity mesh)))

(defun %emit (mesh frame &key link except)
  (funcall (mesh-send mesh) frame :link link :except except))

(defun %seen-key (p)
  (format nil "~A-~D-~D-~A" (hex (packet-sender p)) (packet-timestamp p) (packet-type p)
          (hex (subseq (n:sha256 (packet-payload p)) 0 8))))

(defun %send-packet (mesh p &key link except)
  "P out -- whole, or as fragments when its frame is too long -- and remembered as seen (its echo dropped)."
  (setf (gethash (%seen-key p) (mesh-seen mesh)) (funcall (mesh-clock mesh)))
  (let ((frame (encode-packet p)))
    (if (<= (length frame) (mesh-max-frame mesh))
        (%emit mesh frame :link link :except except)
        (dolist (f (fragment-frames mesh p frame)) (%emit mesh f :link link :except except)))))

;;; ---- what this node says ----

(defun %new-packet (mesh type payload)
  (make-packet :type type :ttl +ttl+ :timestamp (funcall (mesh-clock mesh)) :sender (%my-id mesh) :payload payload))

(defun mesh-announce-frame (mesh)
  "This node's announce, signed (a packet)."
  (sign-packet (%new-packet mesh +announce+ (announce-payload (mesh-identity mesh)))
               (identity-signing-secret (mesh-identity mesh))))

(defun mesh-announce (mesh &key link)
  "This node announced -- on LINK, or every link."
  (setf (mesh-announced mesh) (funcall (mesh-clock mesh)))
  (%send-packet mesh (mesh-announce-frame mesh) :link link))

(defun mesh-say (mesh text)
  "TEXT said to everyone in range: signed, sent on every link, and kept as this node's own message (a CHAT,
   returned)."
  (let ((p (sign-packet (%new-packet mesh +message+ (utf8 text)) (identity-signing-secret (mesh-identity mesh))))
        (id (mesh-identity mesh)))
    (%send-packet mesh p)
    (let ((c (make-chat :sender (hex (%my-id mesh)) :nickname (identity-nickname id) :text text
                        :timestamp (packet-timestamp p) :mine t)))
      (push c (mesh-messages mesh))
      c)))

(defun mesh-leave-frame (mesh)
  "This node's leave, signed (a packet): sent as it goes."
  (sign-packet (%new-packet mesh +leave+ (u8v 0)) (identity-signing-secret (mesh-identity mesh))))

;;; ---- links ----

(defun mesh-link-up (mesh link)
  "LINK is connected: this node announced on it."
  (pushnew link (mesh-links mesh))
  (mesh-announce mesh :link link))

(defun mesh-link-down (mesh link)
  "LINK is gone.  Returns the peers it was the last link to, as (:leave PEER) events."
  (setf (mesh-links mesh) (remove link (mesh-links mesh)))
  (let ((gone '()))
    (maphash (lambda (k p)
               (when (member link (peer-links p))
                 (setf (peer-links p) (remove link (peer-links p)))
                 (when (null (peer-links p)) (push (list :leave p) gone) (remhash k (mesh-peers mesh)))))
             (mesh-peers mesh))
    gone))

(defun mesh-tick (mesh)
  "Time passing: an announce now and then, and what is remembered pruned."
  (let ((now (funcall (mesh-clock mesh))))
    (when (and (mesh-links mesh) (>= (- now (mesh-announced mesh)) +announce-every+))
      (mesh-announce mesh))
    (maphash (lambda (k at) (when (> (- now at) +seen-for+) (remhash k (mesh-seen mesh)))) (mesh-seen mesh))
    (maphash (lambda (k a) (when (> (- now (first a)) +assembly-for+) (remhash k (mesh-assemblies mesh))))
             (mesh-assemblies mesh))))

;;; ---- what comes in ----

(defun mesh-receive (mesh link frame)
  "FRAME, as LINK delivered it: taken (and relayed) if new.  Returns its events."
  (let ((p (handler-case (decode-packet frame) (bad-packet () (incf (mesh-dropped mesh)) nil))))
    (when p (mesh-take mesh link p frame))))

(defun mesh-take (mesh link p frame)
  (let ((key (%seen-key p)) (me (%my-id mesh)))
    (when (or (gethash key (mesh-seen mesh)) (equalp (packet-sender p) me))
      (return-from mesh-take nil))
    (setf (gethash key (mesh-seen mesh)) (funcall (mesh-clock mesh)))
    (let ((for-me (and (packet-recipient p) (equalp (packet-recipient p) me))))
      ;; on to the rest of the mesh, a hop less -- not what was for this node, nor what has run out
      (let ((ttl (min (packet-ttl p) +ttl+)))
        (when (and (not for-me) (> ttl 1) frame)
          (let ((copy (copy-seq frame)))
            (setf (aref copy 2) (1- ttl))
            (%emit mesh copy :except link))))
      (case (packet-type p)
        (#.+announce+ (take-announce mesh link p))
        (#.+message+ (take-message mesh p))
        (#.+leave+ (take-leave mesh p))
        (#.+fragment+ (take-fragment mesh link p))
        (t nil)))))                     ; private traffic and the rest: not yet

(defun take-announce (mesh link p)
  (multiple-value-bind (nick noise signing) (read-announce (packet-payload p))
    (let ((id (hex (packet-sender p))))
      (cond ((null nick) (incf (mesh-dropped mesh)) nil)
            ;; its id is its key's, and it signed what it said with the key it names
            ((or (not (equalp (packet-sender p) (peer-id-of noise))) (not (packet-signed-by-p p signing)))
             (incf (mesh-dropped mesh)) nil)
            (t (let ((known (gethash id (mesh-peers mesh))))
                 (if (and known (peer-signing known) (not (equalp (peer-signing known) signing)))
                     (progn (incf (mesh-dropped mesh)) nil)    ; another key for a known id: refused
                     (let ((peer (or known (setf (gethash id (mesh-peers mesh)) (make-peer :id id)))))
                       (setf (peer-nickname peer) nick (peer-noise peer) noise (peer-signing peer) signing
                             (peer-seen peer) (funcall (mesh-clock mesh)))
                       ;; heard straight from it (its TTL untouched): this link reaches it
                       (when (and link (>= (packet-ttl p) +ttl+)) (pushnew link (peer-links peer)))
                       (list (list :peer peer))))))))))

(defun take-message (mesh p)
  (let ((peer (gethash (hex (packet-sender p)) (mesh-peers mesh))))
    (if (and peer (null (packet-recipient p)) (packet-signed-by-p p (peer-signing peer)))
        (let ((c (make-chat :sender (peer-id peer) :nickname (peer-nickname peer)
                            :text (from-utf8 (packet-payload p)) :timestamp (packet-timestamp p))))
          (setf (peer-seen peer) (funcall (mesh-clock mesh)))
          (push c (mesh-messages mesh))
          (list (list :message c)))
        (progn (incf (mesh-dropped mesh)) nil))))

(defun take-leave (mesh p)
  (let* ((id (hex (packet-sender p))) (peer (gethash id (mesh-peers mesh))))
    (when (and peer (packet-signed-by-p p (peer-signing peer)))
      (remhash id (mesh-peers mesh))
      (list (list :leave peer)))))

;;; ---- fragments: 8-byte id, index and total (2 bytes each), the original's type, then the part ----

(defun fragment-frames (mesh p frame)
  "FRAME (P's) cut into fragment packets' frames, each within the mesh's frame size."
  (let* ((chunk (max 64 (- (mesh-max-frame mesh) 14 8 (if (packet-recipient p) 8 0) 13)))
         (total (ceiling (length frame) chunk))
         (fid (n:random-bytes 8)))
    (loop for i below total
          for start = (* i chunk)
          collect (let ((body (u8v (+ 13 (- (min (length frame) (+ start chunk)) start)))))
                    (replace body fid)
                    (%put body 8 i 2) (%put body 10 total 2)
                    (setf (aref body 12) (packet-type p))
                    (replace body frame :start1 13 :start2 start :end2 (min (length frame) (+ start chunk)))
                    (encode-packet (make-packet :type +fragment+ :ttl (packet-ttl p) :timestamp (packet-timestamp p)
                                                :sender (packet-sender p) :recipient (packet-recipient p)
                                                :payload body))))))

(defun take-fragment (mesh link p)
  (let ((body (packet-payload p)))
    (when (< (length body) 13) (incf (mesh-dropped mesh)) (return-from take-fragment nil))
    (let* ((key (concatenate 'string (hex (packet-sender p)) "/" (hex (subseq body 0 8))))
           (index (%get body 8 2)) (total (%get body 10 2))
           (a (or (gethash key (mesh-assemblies mesh))
                  (setf (gethash key (mesh-assemblies mesh))
                        (list* (funcall (mesh-clock mesh)) total (aref body 12) (make-array total :initial-element nil))))))
      (when (or (zerop total) (> total 256) (>= index total) (/= total (second a)))
        (incf (mesh-dropped mesh)) (return-from take-fragment nil))
      (let ((parts (cdddr a)))
        (setf (aref parts index) (subseq body 13))
        (when (every #'identity parts)
          (remhash key (mesh-assemblies mesh))
          (let* ((whole (apply #'concatenate 'u8v (coerce parts 'list)))
                 (q (handler-case (decode-packet whole) (bad-packet () nil))))
            (if q
                ;; the whole is what was relayed (as its fragments): read, not relayed again
                (let ((key (%seen-key q)))
                  (unless (gethash key (mesh-seen mesh))
                    (setf (gethash key (mesh-seen mesh)) (funcall (mesh-clock mesh)))
                    (case (packet-type q)
                      (#.+announce+ (take-announce mesh link q))
                      (#.+message+ (take-message mesh q))
                      (#.+leave+ (take-leave mesh q)))))
                (progn (incf (mesh-dropped mesh)) nil))))))))
