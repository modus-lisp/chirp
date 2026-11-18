;;;; identity.lisp -- who we are on the mesh, and what an announce says.
;;;;
;;;; Two key pairs: a Noise static key (Curve25519) -- the identity, and later the private messages' -- and an
;;;; Ed25519 key that signs.  The PEER ID is the first 8 bytes of SHA-256 of the Noise public key; a
;;;; fingerprint is all 32.  An ANNOUNCE's payload is TLVs of a type byte, a length byte and the value:
;;;; 0x01 nickname (UTF-8), 0x02 Noise public key, 0x03 signing public key -- these three required --
;;;; 0x04 neighbours (8-byte ids), 0x05 capabilities, 0x06 a bridge geohash; unknown types skipped.

(in-package #:chirp)

(defstruct (persona (:constructor %make-identity) (:conc-name identity-))
  (nickname "")
  noise-secret noise-public           ; X25519
  signing-secret signing-public)      ; Ed25519

(defun peer-id-of (noise-public)
  "The 8-byte peer id of the Noise public key NOISE-PUBLIC."
  (subseq (n:sha256 noise-public) 0 8))

(defun identity-peer-id (id) (peer-id-of (identity-noise-public id)))

(defun new-identity (nickname)
  "A fresh identity named NICKNAME: new keys."
  (multiple-value-bind (ns np) (n:x25519-keypair)
    (multiple-value-bind (ss sp) (n:ed25519-keypair)
      (%make-identity :nickname nickname :noise-secret ns :noise-public np
                      :signing-secret ss :signing-public sp))))

(defun make-identity (&rest args) (apply #'%make-identity args))

(defun identity-plist (id)
  "ID as a plist to keep -- its keys as hex."
  (list :nickname (identity-nickname id)
        :noise-secret (hex (identity-noise-secret id)) :noise-public (hex (identity-noise-public id))
        :signing-secret (hex (identity-signing-secret id)) :signing-public (hex (identity-signing-public id))))

(defun unhex (s)
  (let ((out (u8v (floor (length s) 2))))
    (dotimes (i (length out) out) (setf (aref out i) (parse-integer s :start (* 2 i) :end (+ 2 (* 2 i)) :radix 16)))))

(defun identity-from-plist (p)
  (%make-identity :nickname (getf p :nickname "")
                  :noise-secret (unhex (getf p :noise-secret)) :noise-public (unhex (getf p :noise-public))
                  :signing-secret (unhex (getf p :signing-secret)) :signing-public (unhex (getf p :signing-public))))

;;; ---- text ----

(defun utf8 (string)
  #+sbcl (sb-ext:string-to-octets string :external-format :utf-8)
  #-sbcl (let ((out '()))
           (loop for ch across string
                 for c = (char-code ch)
                 do (cond ((< c #x80) (push c out))
                          ((< c #x800) (push (logior #xc0 (ash c -6)) out) (push (logior #x80 (logand c #x3f)) out))
                          ((< c #x10000) (push (logior #xe0 (ash c -12)) out)
                           (push (logior #x80 (logand (ash c -6) #x3f)) out) (push (logior #x80 (logand c #x3f)) out))
                          (t (push (logior #xf0 (ash c -18)) out) (push (logior #x80 (logand (ash c -12) #x3f)) out)
                             (push (logior #x80 (logand (ash c -6) #x3f)) out) (push (logior #x80 (logand c #x3f)) out))))
           (coerce (nreverse out) 'u8v)))

(defun from-utf8 (bytes)
  "BYTES as text; a malformed sequence shows as U+FFFD."
  (with-output-to-string (o)
    (let ((i 0) (n (length bytes)))
      (loop while (< i n)
            do (let* ((b (aref bytes i))
                      (len (cond ((< b #x80) 1) ((= (logand b #xe0) #xc0) 2) ((= (logand b #xf0) #xe0) 3)
                                 ((= (logand b #xf8) #xf0) 4) (t 0))))
                 (if (or (zerop len) (> (+ i len) n)
                         (loop for k from 1 below len thereis (/= (logand (aref bytes (+ i k)) #xc0) #x80)))
                     (progn (write-char (code-char #xfffd) o) (incf i))
                     (let ((c (if (= len 1) b (logand b (ash #xff (- (1+ len)))))))
                       (loop for k from 1 below len do (setf c (logior (ash c 6) (logand (aref bytes (+ i k)) #x3f))))
                       (write-char (or (ignore-errors (code-char c)) (code-char #xfffd)) o)
                       (incf i len))))))))

;;; ---- the announce ----

(defun announce-payload (id &key neighbours)
  "ID's announce: its nickname, Noise key and signing key; NEIGHBOURS (8-byte ids, up to 10) when given."
  (let* ((nick (let ((b (utf8 (identity-nickname id)))) (subseq b 0 (min 255 (length b)))))
         (near (subseq neighbours 0 (min 10 (length neighbours))))
         (tlvs (append (list (cons 1 nick) (cons 2 (identity-noise-public id)) (cons 3 (identity-signing-public id)))
                       (and near (list (cons 4 (apply #'concatenate 'u8v near))))))
         (out (u8v (loop for (nil . v) in tlvs sum (+ 2 (length v)))))
         (at 0))
    (loop for (type . v) in tlvs
          do (setf (aref out at) type (aref out (1+ at)) (length v))
             (replace out v :start1 (+ at 2))
             (incf at (+ 2 (length v))))
    out))

(defun read-announce (payload)
  "An announce's fields: (values nickname noise-public signing-public neighbours), or NIL when one of the
   three it needs is missing or malformed."
  (let ((at 0) (n (length payload)) nick noise sign near)
    (loop while (< (1+ at) n)
          do (let* ((type (aref payload at)) (len (aref payload (1+ at))) (v (+ at 2)))
               (when (> (+ v len) n) (return))
               (let ((value (subseq payload v (+ v len))))
                 (case type
                   (1 (setf nick (from-utf8 value)))
                   (2 (when (= len 32) (setf noise value)))
                   (3 (when (= len 32) (setf sign value)))
                   (4 (setf near (loop for i from 0 below (floor len 8) collect (subseq value (* 8 i) (* 8 (1+ i))))))))
               (setf at (+ v len))))
    (and nick noise sign (values nick noise sign near))))
