;;;; app.lisp -- the chat, on the phone: bitchat's public channel over Bluetooth LE, in native screens shown over
;;;; whatever app opens it (the modus desk's "Bitchat").
;;;;
;;;; The radio is Bluetooth.swift's (kiln-native's BLE-*): links come and go, frames arrive on them.  The mesh
;;;; (mesh.lisp) makes sense of the frames; this shows what it says, and sends what is typed.

(defpackage #:chirp-app
  (:use #:cl #:chirp)
  (:local-nicknames (#:k #:kiln-native))
  (:export #:open-chat))

(in-package #:chirp-app)

(defvar *mesh* nil)
(defvar *nav* nil)
(defvar *root* nil)
(defvar *table* nil)
(defvar *rows* '() "What the chat shows, oldest first: (:chat CHAT) or (:note TEXT TIME).")
(defvar *known* (make-hash-table :test 'equal) "Peers seen this session (hex id -> nickname): who is new.")
(defvar *links* (make-hash-table) "Open links -> the most one frame on it may carry.")
(defvar *partial* (make-hash-table) "Link -> bytes of a frame that came in pieces, so far.")
(defvar *log* '() "The radio's log, newest first.")
(defvar *utc-offset* 0)
(defvar *last-tick* 0)

;;; ---- where this phone's identity is kept ----
;;; The two secret keys in the system keychain (gossamer's KEYCHAIN-KEEP), read back straight into fixed memory;
;;; the nickname and the public keys in a file in the app's Library (not Documents, which the Files app shows).
;;; Where there is no keychain, the secrets go in the file too.

(defun library-file ()
  (let* ((docs (k:app-documents))
         (base (subseq docs 0 (or (position #\/ docs :from-end t :end (max 0 (1- (length docs)))) 0))))
    (concatenate 'string base "/Library/chirp/identity.sexp")))

(defparameter +noise-secret+ "chirp-noise-secret")
(defparameter +signing-secret+ "chirp-signing-secret")

(defun keychain-p () (gossamer:keychain-available-p))

(defun load-identity ()
  (let* ((path (library-file))
         (plist (ignore-errors (with-open-file (in path) (let ((*read-eval* nil)) (read in))))))
    (cond
      ((null plist)
       (let ((id (new-identity (format nil "anon~4,'0D" (random 10000 (make-random-state t))))))
         (save-identity id :new t)
         id))
      ;; the secrets in the file (kept before the keychain, or with none): into the keychain now, out of the file
      ((getf plist :noise-secret)
       (let ((id (identity-from-plist plist)))
         (if (keychain-p)
             (progn (save-identity id :new t)
                    (load-identity))              ; read back from the keychain: the keys in fixed memory
             id)))
      (t (let ((noise (gossamer:keychain-recall +noise-secret+))
               (signing (gossamer:keychain-recall +signing-secret+)))
           (if (and noise signing)
               (make-identity :nickname (getf plist :nickname "")
                              :noise-secret noise :noise-public (chirp::unhex (getf plist :noise-public))
                              :signing-secret signing :signing-public (chirp::unhex (getf plist :signing-public)))
               ;; the keychain lost them (a restore without it): a new identity, the nickname kept
               (let ((id (new-identity (getf plist :nickname "anon"))))
                 (note "the keychain had no keys: a new identity")
                 (save-identity id :new t)
                 id)))))))

(defun save-identity (id &key new)
  "ID kept: its NEW secret keys into the keychain (when there is one), its nickname and public keys in the file."
  (let ((path (library-file))
        ;; with a keychain the file holds no secret, not even on its way there (hex of a key is a copy of it)
        (plist (if (keychain-p)
                   (list :nickname (identity-nickname id) :noise-public (hex (identity-noise-public id))
                         :signing-public (hex (identity-signing-public id)))
                   (identity-plist id))))
    (when (and new (keychain-p))
      (unless (and (gossamer:keychain-keep +noise-secret+ (identity-noise-secret id))
                   (gossamer:keychain-keep +signing-secret+ (identity-signing-secret id)))
        (error "chirp: the keychain refused the identity's keys")))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (with-standard-io-syntax (let ((*print-readably* nil)) (prin1 plist out))))))

;;; ---- the radio ----

(defvar *log-file* nil "Documents/chirp.log: the radio's log, frames and all, to read off the phone.")

(defun note (fmt &rest args)
  (let ((line (apply #'format nil fmt args)))
    (push line *log*)
    (when (> (length *log*) 300) (setf *log* (subseq *log* 0 200)))
    (ignore-errors
     (with-open-file (out (or *log-file* (setf *log-file* (concatenate 'string (k:app-documents) "/chirp.log")))
                          :direction :output :if-exists :append :if-does-not-exist :create)
       (format out "~D ~A~%" (chirp::unix-ms) line)))))

(defun send-frame (frame &key link except)
  "FRAME out: on LINK, or on every link but EXCEPT."
  (note "out ~:[all~;~:*~D~]~@[ but ~D~] ~A" link except (hex frame))
  (if link
      (k:ble-send link frame)
      (loop for l being the hash-keys of *links*
            unless (eql l except) do (k:ble-send l frame))))

(defun fit-frames ()
  "The mesh's frames no longer than the smallest link carries (fragments, past that)."
  (let ((smallest (loop for m being the hash-values of *links* minimize m)))
    (setf (chirp::mesh-max-frame *mesh*) (max 64 (min 512 (if (plusp (hash-table-count *links*)) smallest 512))))))

(defun on-radio (value)
  (destructuring-bind (kind &rest args) value
    (case kind
      (:state (note "radio ~:[off~;on~]" (eql (first args) 1)) (show-title))
      (:log (note "~A" (first args)))
      (:link (destructuring-bind (link up max) args
               (if (eql up 1)
                   (progn (setf (gethash link *links*) max)
                          (note "link ~D up (~D bytes)" link max)
                          (fit-frames)
                          (mesh-link-up *mesh* link))
                   (progn (remhash link *links*) (remhash link *partial*)
                          (note "link ~D down" link)
                          (fit-frames)
                          (take-events (mesh-link-down *mesh* link))))
               (show-title)))
      (:data (destructuring-bind (link bytes) args (take-frame link bytes))))))

(defun take-frame (link bytes)
  "BYTES from LINK: a frame -- or, if it is short of one, the start of a frame whose rest is to come."
  (note "in ~D ~A" link (hex (coerce bytes '(simple-array (unsigned-byte 8) (*)))))
  (let* ((so-far (gethash link *partial*))
         (frame (if so-far (concatenate '(simple-array (unsigned-byte 8) (*)) so-far bytes)
                    (coerce bytes '(simple-array (unsigned-byte 8) (*))))))
    (handler-case
        (let ((p (decode-packet frame)))
          (remhash link *partial*)
          (let ((before (mesh-dropped *mesh*)))
            (take-events (chirp::mesh-take *mesh* link p frame))
            (when (> (mesh-dropped *mesh*) before)
              (note "dropped type ~D from ~A (unverified, or unknown sender)" (packet-type p) (hex (packet-sender p))))))
      (chirp::bad-packet (e)
        (if (and (search "short" (chirp::bad-packet-why e)) (< (length frame) 4096))
            (setf (gethash link *partial*) frame)
            (progn (remhash link *partial*)
                   (note "link ~D: ~A (~D bytes)" link (chirp::bad-packet-why e) (length frame))))))))

(defun take-events (events)
  (dolist (e events) (note "event ~S ~A" (first e) (let ((x (second e))) (if (chirp::peer-p x) (peer-nickname x) (chat-text x)))))
  (let ((changed nil))
    (dolist (e events)
      (destructuring-bind (kind thing) e
        (case kind
          (:message (setf *rows* (append *rows* (list (list :chat thing))) changed t))
          (:peer (unless (gethash (peer-id thing) *known*)
                   (setf (gethash (peer-id thing) *known*) (peer-nickname thing))
                   (setf *rows* (append *rows* (list (list :note (format nil "~A is here" (peer-nickname thing))
                                                           (chirp::unix-ms))))
                         changed t))
                 (show-title))
          (:leave (when (remhash (peer-id thing) *known*)
                    (setf *rows* (append *rows* (list (list :note (format nil "~A left" (peer-nickname thing))
                                                            (chirp::unix-ms))))
                          changed t))
                  (show-title)))))
    (when changed (show-rows))))

(defun tick ()
  "Once a second, from the desk's frame loop: the mesh's announces and its forgetting."
  (let ((now (get-internal-real-time)))
    (when (and *mesh* (> (- now *last-tick*) internal-time-units-per-second))
      (setf *last-tick* now)
      (mesh-tick *mesh*))))

;;; ---- the screens ----

(defun clock (ms)
  (let* ((secs (+ (floor ms 1000) *utc-offset*)) (day (mod secs 86400)))
    (format nil "~2,'0D:~2,'0D" (floor day 3600) (floor (mod day 3600) 60))))

(defun show-rows ()
  (when *table*
    (k:app-table-set
     *table*
     (mapcar (lambda (r)
               (ecase (first r)
                 (:chat (let ((c (second r)))
                          (list :title (format nil "~A  ·  ~A" (chat-nickname c) (clock (chat-timestamp c)))
                                :strong (chat-nickname c)
                                :subtitle (chat-text c)
                                :image (if (chat-mine c) "person.circle.fill" "person.circle")
                                :tint (if (chat-mine c) :label :secondary))))
                 (:note (list :title (format nil "~A  ·  ~A" (second r) (clock (third r)))
                              :image "dot.radiowaves.left.and.right" :tint :secondary))))
             *rows*))
    (k:app-table-scroll-end *table* :animated t)))

(defun show-title ()
  (when *root*
    (k:app-set-title *root* (format nil "#mesh  ·  ~D nearby" (hash-table-count (mesh-peers *mesh*))))))

(defun people ()
  (let* ((screen (k:app-push *nav* "Nearby"))
         (table (k:app-table screen))
         (peers (loop for p being the hash-values of (mesh-peers *mesh*) collect p)))
    (k:app-table-empty table "No One Nearby" "Phones with bitchat in Bluetooth range show here." "person.2.slash")
    (k:app-table-set table
                     (mapcar (lambda (p)
                               (list :title (peer-nickname p)
                                     :subtitle (format nil "~A  ·  ~:[through the mesh~;in range~]"
                                                       (peer-id p) (peer-links p))
                                     :image "person.circle" :tint :secondary))
                             (sort peers #'string< :key #'peer-nickname)))))

(defun radio-log ()
  (let* ((screen (k:app-push *nav* "Radio"))
         (table (k:app-table screen :plain t)))
    (k:app-table-set table (mapcar (lambda (l) (list :title l)) *log*))))

(defun nickname ()
  (let ((f (k:app-form "Nickname" :save "Save"))
        (id (mesh-identity *mesh*)))
    (k:app-form-field f "Nickname" (identity-nickname id) :placeholder "anon")
    (k:app-form-note f (format nil "What people nearby see you as.  Your id: ~A" (hex (identity-peer-id id))))
    (k:on-event :form f (lambda (v h) (declare (ignore h))
                          (when (and (consp v) (eql (first v) 1))
                            (let ((name (string-trim " " (second v))))
                              (when (plusp (length name))
                                (setf (identity-nickname id) name)
                                (save-identity id)
                                (mesh-announce *mesh*))))))
    (k:app-form-show f)))

(defun open-chat ()
  "The chat, shown over what is there: the radio on, this phone announced to whoever is in range."
  (unless *mesh*
    (setf *utc-offset* (ignore-errors (k:app-utc-offset))
          *mesh* (make-mesh :identity (load-identity) :send #'send-frame))
    (k:on-event :ble 0 (lambda (v h) (declare (ignore h)) (handler-case (on-radio v)
                                                            (error (e) (note "error: ~A" e)))))
    (pushnew 'tick cl-user::*kiln-frame-hooks*)
    (k:ble-start +service-uuid+ +characteristic-uuid+))
  (setf *nav* (k:app-nav-present "#mesh")
        *root* (k:app-nav-root *nav*)
        *table* (k:app-table *root* :plain t))
  (k:app-table-empty *table* "Nobody Has Said Anything" "Messages from phones nearby show here." "bubble.left.and.bubble.right")
  (let ((close (k:app-bar-button *root* "xmark"))
        (more (k:app-bar-menu *root* "ellipsis"
                              '((:id 1 :title "Nearby" :image "person.2")
                                (:id 2 :title "Nickname" :image "person.text.rectangle")
                                (:id 3 :title "Radio Log" :image "antenna.radiowaves.left.and.right"))
                              :right t))
        (compose (k:app-compose *root* "Message")))
    (k:on-event :tap close (lambda (v h) (declare (ignore v h)) (k:app-nav-dismiss *nav*)))
    (k:on-event :menu more (lambda (id h) (declare (ignore h))
                             (case id (1 (people)) (2 (nickname)) (3 (radio-log)))))
    (k:on-event :compose compose (lambda (text h) (declare (ignore h))
                                   (when (and (stringp text) (plusp (length text)))
                                     (let ((c (mesh-say *mesh* text)))
                                       (setf *rows* (append *rows* (list (list :chat c))))
                                       (show-rows))))))
  (show-rows)
  (show-title)
  :chat)
