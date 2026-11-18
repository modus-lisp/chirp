;;;; packet.lisp -- bitchat's binary packet, as its Swift BinaryProtocol writes and reads it.
;;;;
;;;; Big-endian throughout.  A v1 header is 14 bytes, v2 (source-routed) 16:
;;;;   version(1) type(1) ttl(1) timestamp(8, ms since 1970) flags(1) payload-length(2; v2: 4)
;;;; then the sender (8 bytes, always), the recipient (8, flag 0x01; absent = broadcast), the route (v2,
;;;; flag 0x08: a count and that many 8-byte ids), the payload (flag 0x04: an original-size prefix, 2 bytes
;;;; or 4, then raw DEFLATE), the signature (64, flag 0x02: Ed25519), and padding a decoder ignores.
;;;; PAYLOAD-LENGTH counts the payload and its size prefix only.

(in-package #:chirp)

(defconstant +announce+ #x01)
(defconstant +message+ #x02)
(defconstant +leave+ #x03)
(defconstant +noise-handshake+ #x10)
(defconstant +noise-encrypted+ #x11)
(defconstant +fragment+ #x20)

(defconstant +flag-recipient+ #x01)
(defconstant +flag-signature+ #x02)
(defconstant +flag-compressed+ #x04)
(defconstant +flag-route+ #x08)
(defconstant +flag-rsr+ #x10)              ; left out of what is signed

;; the GATT service and its one characteristic (the release build's: a debug build's service ends ...5A)
(defparameter +service-uuid+ "F47B5E2D-4A9E-4C5A-9B3F-8E1D2C3A4B5C")
(defparameter +characteristic-uuid+ "A1B2C3D4-E5F6-4A5B-8C9D-0E1F2A3B4C5D")

(deftype u8v () '(simple-array (unsigned-byte 8) (*)))

(defun u8v (n) (make-array n :element-type '(unsigned-byte 8) :initial-element 0))

(defstruct packet
  (version 1) (type +message+) (ttl 7) (timestamp 0) (flags 0)
  (sender (u8v 8)) (recipient nil) (route nil)     ; RECIPIENT nil: broadcast; ROUTE a list of 8-byte ids
  (payload (u8v 0)) (signature nil)
  (wire nil))       ; a compressed payload as it came: its size prefix and DEFLATE bytes, re-sent and signed as they are

(defun hex (bytes)
  (with-output-to-string (o) (loop for b across bytes do (format o "~(~2,'0x~)" b))))

(defun id8 (bytes)
  "BYTES as an 8-byte id: cut, or zero-padded."
  (let ((out (u8v 8))) (replace out bytes :end2 (min 8 (length bytes))) out))

;;; ---- writing ----

(defun %put (out at value bytes)
  "VALUE big-endian into BYTES bytes of OUT at AT; returns the position after."
  (dotimes (i bytes (+ at bytes))
    (setf (aref out (+ at i)) (ldb (byte 8 (* 8 (- bytes 1 i))) value))))

(defun %pad-target (size)
  "The bucket a frame of SIZE bytes is padded to (the smallest of 256 512 1024 2048 with room for 16 more),
   or NIL when the padding it needs would not fit a byte."
  (let ((bucket (find-if (lambda (b) (>= b (+ size 16))) '(256 512 1024 2048))))
    (and bucket (<= (- bucket size) 255) bucket)))

(defun encode-packet (p &key (padding nil) (signature t) (ttl (packet-ttl p)))
  "P's frame.  PADDING: up to a bucket, each pad byte the pad's length (as bitchat pads private traffic, and
   what it signs).  SIGNATURE nil: without it, its flag cleared (what is signed).  A payload of ours is sent as
   it is, never compressed; one that came compressed goes as it came (its WIRE bytes)."
  (let* ((route (packet-route p))
         (v2 (or route (= (packet-version p) 2)))
         (recipient (packet-recipient p))
         (sig (and signature (packet-signature p)))
         (payload (or (packet-wire p) (packet-payload p)))
         (flags (logior (logand (packet-flags p) (lognot (logior +flag-recipient+ +flag-signature+
                                                                 +flag-compressed+ +flag-route+)))
                        (if recipient +flag-recipient+ 0)
                        (if sig +flag-signature+ 0)
                        (if route +flag-route+ 0)
                        (if (packet-wire p) +flag-compressed+ 0)))
         (size (+ (if v2 16 14) 8 (if recipient 8 0) (if route (+ 1 (* 8 (length route))) 0)
                  (length payload) (if sig 64 0)))
         (target (and padding (%pad-target size)))
         (out (u8v (or target size)))
         (at 0))
    (setf at (%put out at (if v2 2 1) 1)
          at (%put out at (packet-type p) 1)
          at (%put out at ttl 1)
          at (%put out at (packet-timestamp p) 8)
          at (%put out at flags 1)
          at (%put out at (length payload) (if v2 4 2)))
    (replace out (id8 (packet-sender p)) :start1 at) (incf at 8)
    (when recipient (replace out (id8 recipient) :start1 at) (incf at 8))
    (when route
      (setf at (%put out at (length route) 1))
      (dolist (hop route) (replace out (id8 hop) :start1 at) (incf at 8)))
    (replace out payload :start1 at) (incf at (length payload))
    (when sig (replace out sig :start1 at) (incf at 64))
    (when target (fill out (- target size) :start size))
    out))

(defun signing-bytes (p)
  "What P's signature covers: its frame without the signature, its TTL 0 (relays change it), the RSR flag
   cleared, padded."
  (let ((q (copy-packet p)))
    (setf (packet-flags q) (logand (packet-flags q) (lognot +flag-rsr+)))
    (encode-packet q :padding t :signature nil :ttl 0)))

(defun sign-packet (p secret)
  "P signed with the Ed25519 SECRET key: its signature set; returns P."
  (setf (packet-signature p) (n:ed25519-sign secret (signing-bytes p)))
  p)

(defun packet-signed-by-p (p public)
  "Whether P carries a signature by the Ed25519 PUBLIC key."
  (and (packet-signature p) public
       (ignore-errors (n:ed25519-verify public (signing-bytes p) (packet-signature p)))))

;;; ---- reading ----

(define-condition bad-packet (error) ((why :initarg :why :reader bad-packet-why))
  (:report (lambda (c s) (format s "bitchat packet: ~A" (bad-packet-why c)))))

(defun %bad (why) (error 'bad-packet :why why))

(defun %get (in at bytes)
  (let ((v 0)) (dotimes (i bytes v) (setf v (logior (ash v 8) (aref in (+ at i)))))))

(defun %inflate (data size)
  "DATA decompressed to SIZE bytes: raw DEFLATE (Apple's COMPRESSION_ZLIB), or a zlib stream if it has the
   header."
  (let ((out (if (and (>= (length data) 2) (= (logand (aref data 0) #x0f) 8)
                      (zerop (mod (+ (* 256 (aref data 0)) (aref data 1)) 31)))
                 (handler-case (cram:zlib-decompress data) (error () (cram:deflate-decompress data)))
                 (cram:deflate-decompress data))))
    (unless (= (length out) size) (%bad "decompressed size"))
    (coerce out 'u8v)))

(defun decode-packet (in)
  "The packet in the frame IN (bytes after it -- padding -- ignored).  Signals BAD-PACKET."
  (let ((len (length in)))
    (when (< len 14) (%bad "short"))
    (let* ((version (aref in 0))
           (v2 (= version 2))
           (head (if v2 16 14)))
      (unless (member version '(1 2)) (%bad (format nil "version ~D" version)))
      (when (< len (+ head 8)) (%bad "short"))
      (let* ((type (aref in 1)) (ttl (aref in 2)) (timestamp (%get in 3 8)) (flags (aref in 11))
             (plen (%get in 12 (if v2 4 2)))
             (at head)
             (sender (subseq in at (+ at 8)))
             (recipient nil) (route nil) (payload nil) (signature nil) (wire nil))
        (incf at 8)
        (when (logtest flags +flag-recipient+)
          (when (> (+ at 8) len) (%bad "short recipient"))
          (setf recipient (subseq in at (+ at 8))) (incf at 8))
        (when (and v2 (logtest flags +flag-route+))
          (when (>= at len) (%bad "short route"))
          (let ((n (aref in at)))
            (incf at)
            (when (> (+ at (* 8 n)) len) (%bad "short route"))
            (setf route (loop repeat n collect (prog1 (subseq in at (+ at 8)) (incf at 8))))))
        (when (> (+ at plen) len) (%bad "short payload"))
        (setf payload
              (if (logtest flags +flag-compressed+)
                  (let ((prefix (if v2 4 2)))
                    (when (< plen prefix) (%bad "short size prefix"))
                    (setf wire (subseq in at (+ at plen)))
                    (%inflate (subseq in (+ at prefix) (+ at plen)) (%get in at prefix)))
                  (subseq in at (+ at plen))))
        (incf at plen)
        (when (logtest flags +flag-signature+)
          (when (> (+ at 64) len) (%bad "short signature"))
          (setf signature (subseq in at (+ at 64))))
        (make-packet :version version :type type :ttl ttl :timestamp timestamp
                     :flags (logand flags (lognot +flag-compressed+))
                     :sender sender :recipient recipient :route route :payload payload
                     :signature signature :wire wire)))))
