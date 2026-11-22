;;;; glass.lisp -- the chat in a glass window: bitchat's public channel on a desktop (Windows first).
;;;;
;;;; app.lisp's chat for a machine without kiln's native screens: one glass window (the app contract
;;;; of glass.wm, so SERVE-WM opens it from its menu) showing the channel, who is nearby, and a line
;;;; to type on.  The mesh is mesh.lisp's, unchanged.
;;;;
;;;; THE RADIO is the host's, found by name so chirp needs nothing to load it: modus on Windows
;;;; provides W32-BLE-START / -POLL / -SEND / -STOP in MODUS-INT (modus net/win32-ble.lisp), whose
;;;; events are kiln-native's BLE events -- (:state ON) (:link LINK UP MAX) (:data LINK BYTES)
;;;; (:log TEXT) -- so ON-RADIO below is app.lisp's.  With no radio the window says so and the rest
;;;; still works (a chat with nobody).
;;;;
;;;; EVERYTHING IS THE WINDOW'S: the mesh, the radio, the rows, all in the closures BITCHAT-APP
;;;; returns, none in a global.  glass's compositor thread runs the app, and on modus a thread may
;;;; not store its own objects anywhere another can reach.
;;;;
;;;; THE IDENTITY is kept in ~/.chirp/identity.sexp, secret keys included: there is no keychain
;;;; here (app.lisp's rule for a machine without one).  The radio's log, frames and all, goes to
;;;; ~/.chirp/chirp.log, to send back from a test.
;;;;
;;;; Keys: type and Return to send; Tab shows the radio's log instead of the chat (and back);
;;;; Escape clears the line.

(defpackage #:chirp-glass
  (:use #:cl #:chirp)
  (:export #:bitchat-app #:radio-available-p))

(in-package #:chirp-glass)

(defun %radio-fn (name)
  "The host's radio function NAME, or NIL.  INTERN, not FIND-SYMBOL: modus interns a kernel
   function's name lazily, so FIND-SYMBOL answers NIL for one no source has mentioned."
  (let* ((p (find-package "MODUS-INT"))
         (s (and p (intern name p))))
    (and s (fboundp s) (symbol-function s))))

(defun radio-available-p () (and (%radio-fn "W32-BLE-START") t))

(defun %home-file (name)
  (merge-pathnames (concatenate 'string ".chirp/" name) (user-homedir-pathname)))

(defun %load-identity ()
  (let* ((path (%home-file "identity.sexp"))
         (plist (ignore-errors (with-open-file (in path) (let ((*read-eval* nil)) (read in))))))
    (or (and plist (ignore-errors (identity-from-plist plist)))
        (let ((id (new-identity (format nil "anon~4,'0D" (random 10000 (make-random-state t))))))
          (%save-identity id)
          id))))

(defun %save-identity (id)
  (let ((path (%home-file "identity.sexp")))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (with-standard-io-syntax (let ((*print-readably* nil)) (prin1 (identity-plist id) out))))))

(defun %wrap (text width size)
  "TEXT in rows no wider than WIDTH at SIZE: broken at spaces, inside a word only when it alone is wider."
  (let ((rows '()) (row ""))
    (flet ((fits (s) (<= (glass:text-width s :size size) width)))
      (loop for word in (let ((acc '()) (start 0))
                          (loop for k below (length text)
                                when (char= (char text k) #\Space)
                                  do (push (subseq text start (1+ k)) acc) (setf start (1+ k)))
                          (push (subseq text start) acc)
                          (nreverse acc))
            do (cond ((fits (concatenate 'string row (string-right-trim " " word)))
                      (setf row (concatenate 'string row word)))
                     (t (when (plusp (length row)) (push row rows))
                        (setf row "")
                        (loop for ch across word
                              do (if (fits (concatenate 'string row (string ch)))
                                     (setf row (concatenate 'string row (string ch)))
                                     (progn (push row rows) (setf row (string ch))))))))
      (push row rows)
      (nreverse rows))))

(defun %clock (ms)
  (multiple-value-bind (s m h) (decode-universal-time (+ (floor ms 1000) 2208988800))
    (declare (ignore s))
    (format nil "~2,'0D:~2,'0D" h m)))

(defun %keysym-char (ks)
  (cond ((<= #x20 ks #x7e) (code-char ks))
        ((<= #xa0 ks #xff) (code-char ks))
        ((<= #x1000000 ks #x110ffff) (code-char (- ks #x1000000)))))

(defparameter +ink+ #xe6ebf2)
(defparameter +dim+ #x8b949e)
(defparameter +paper+ #x10141a)
(defparameter +bar+ #x1b2129)
(defparameter +mine+ #xf0b429)
(defparameter +them+ #x58a6ff)

(defun bitchat-app (fb)
  "The chat in FB: the radio on, this machine announced to whoever is in range.  glass.wm's app
   contract (ON-KEY ON-POINTER TICK COPY-P CLOSE-FN RESIZE-FN TEXT-P)."
  (let* ((start (%radio-fn "W32-BLE-START")) (poll (%radio-fn "W32-BLE-POLL"))
         (send (%radio-fn "W32-BLE-SEND")) (stop (%radio-fn "W32-BLE-STOP"))
         (radio nil) (radio-on nil) (why nil)
         (links (make-hash-table)) (partial (make-hash-table))
         (rows '()) (known (make-hash-table :test 'equal)) (log '())
         (line "") (show-log nil) (dirty t) (last-tick 0)
         (log-path (%home-file "chirp.log"))
         (mesh nil))
    (labels ((note (fmt &rest args)
               (let ((text (apply #'format nil fmt args)))
                 (push text log)
                 (when (> (length log) 400) (setf log (subseq log 0 300)))
                 (ignore-errors
                  (ensure-directories-exist log-path)
                  (with-open-file (out log-path :direction :output :if-exists :append :if-does-not-exist :create)
                    (format out "~D ~A~%" (chirp::unix-ms) text)))
                 (when show-log (setf dirty t))))
             (row (r) (setf rows (append rows (list r)) dirty t)
               (when (> (length rows) 500) (setf rows (subseq rows 100))))
             (send-frame (frame &key link except)
               (note "out ~:[all~;~:*~D~]~@[ but ~D~] ~A" link except (hex frame))
               (when radio
                 (if link
                     (funcall send radio link frame)
                     (loop for l being the hash-keys of links
                           unless (eql l except) do (funcall send radio l frame)))))
             (fit-frames ()
               (let ((smallest (loop for m being the hash-values of links minimize m)))
                 (setf (chirp::mesh-max-frame mesh)
                       (max 64 (min 512 (if (plusp (hash-table-count links)) smallest 512))))))
             (take-events (events)
               (dolist (e events)
                 (destructuring-bind (kind thing) e
                   (case kind
                     (:message (row (list :chat thing)))
                     (:peer (unless (gethash (peer-id thing) known)
                              (setf (gethash (peer-id thing) known) (peer-nickname thing))
                              (row (list :note (format nil "~A is here" (peer-nickname thing)) (chirp::unix-ms))))
                      (setf dirty t))
                     (:leave (when (remhash (peer-id thing) known)
                               (row (list :note (format nil "~A left" (peer-nickname thing)) (chirp::unix-ms))))
                      (setf dirty t))))))
             (take-frame (link bytes)
               (note "in ~D ~A" link (hex bytes))
               (let* ((so-far (gethash link partial))
                      (frame (if so-far (concatenate '(simple-array (unsigned-byte 8) (*)) so-far bytes)
                                 (coerce bytes '(simple-array (unsigned-byte 8) (*))))))
                 (handler-case
                     (let ((p (decode-packet frame)))
                       (remhash link partial)
                       (let ((before (mesh-dropped mesh)))
                         (take-events (chirp::mesh-take mesh link p frame))
                         (when (> (mesh-dropped mesh) before)
                           (note "dropped type ~D from ~A (unverified, or unknown sender)"
                                 (packet-type p) (hex (packet-sender p))))))
                   (chirp::bad-packet (e)
                     (if (and (search "short" (chirp::bad-packet-why e)) (< (length frame) 4096))
                         (setf (gethash link partial) frame)
                         (progn (remhash link partial)
                                (note "link ~D: ~A (~D bytes)" link (chirp::bad-packet-why e) (length frame))))))))
             (on-radio (value)
               (destructuring-bind (kind &rest args) value
                 (case kind
                   (:state (setf radio-on (eql (first args) 1) dirty t)
                    (note "radio ~:[off~;on~]" radio-on)
                    (unless radio-on (row (list :note "No Bluetooth radio is on: nobody can hear this." (chirp::unix-ms)))))
                   (:log (note "~A" (first args)))
                   (:link (destructuring-bind (link up max) args
                            (if (eql up 1)
                                (progn (setf (gethash link links) max)
                                       (note "link ~D up (~D bytes)" link max)
                                       (fit-frames)
                                       (mesh-link-up mesh link))
                                (progn (remhash link links) (remhash link partial)
                                       (note "link ~D down" link)
                                       (fit-frames)
                                       (take-events (mesh-link-down mesh link))))
                            (setf dirty t)))
                   (:data (destructuring-bind (link bytes) args (take-frame link bytes))))))
             (say ()
               (let ((text (string-trim " " line)))
                 (setf line "" dirty t)
                 (when (plusp (length text))
                   (row (list :chat (mesh-say mesh text))))))
             (draw ()
               (let* ((w (glass:fb-width fb)) (h (glass:fb-height fb))
                      (size 15) (lh 20) (margin 12) (bar 30) (input 34)
                      (bottom (- h input)))
                 (glass:fb-fill fb +paper+)
                 ;; the title bar: the channel, who is near, the radio
                 (glass:fb-rect fb 0 0 w bar +bar+)
                 (glass:fb-text fb margin 7 (format nil "#mesh  ·  ~D nearby  ·  ~D link~:P"
                                                    (hash-table-count (mesh-peers mesh))
                                                    (hash-table-count links) (hash-table-count links))
                                :size 14 :color +ink+)
                 (let ((r (cond ((null radio) (or why "no radio"))
                                (radio-on "radio on") (t "radio off"))))
                   (glass:fb-text fb (- w margin (glass:text-width r :size 13)) 8 r :size 13
                                  :color (if radio-on #x3fb950 +dim+)))
                 ;; the rows, newest at the bottom, as many as fit
                 (let ((y bottom) (inner (- w (* 2 margin))))
                   (if show-log
                       (dolist (l log)
                         (when (< y (+ bar lh)) (return))
                         (dolist (r (reverse (%wrap l inner 12)))
                           (decf y 16)
                           (when (< y bar) (return))
                           (glass:fb-text fb margin y r :size 12 :color +dim+)))
                       (dolist (r (reverse rows))
                         (when (< y (+ bar lh)) (return))
                         (ecase (first r)
                           (:chat (let* ((c (second r))
                                         (head (format nil "~A  ~A" (chat-nickname c) (%clock (chat-timestamp c))))
                                         (body (%wrap (chat-text c) inner size)))
                                    (dolist (b (reverse body))
                                      (decf y lh)
                                      (when (< y bar) (return))
                                      (glass:fb-text fb margin y b :size size :color +ink+))
                                    (decf y lh)
                                    (when (>= y bar)
                                      (glass:fb-text fb margin y head :size 13
                                                     :color (if (chat-mine c) +mine+ +them+)))
                                    (decf y 6)))
                           (:note (decf y lh)
                            (when (>= y bar)
                              (glass:fb-text fb margin y (format nil "~A  ~A" (second r) (%clock (third r)))
                                             :size 13 :color +dim+))
                            (decf y 6)))))
                   (when (and (not show-log) (null rows))
                     (glass:fb-text fb margin (+ bar 16) "Nobody has said anything yet." :size size :color +dim+)
                     (glass:fb-text fb margin (+ bar 16 lh) "Phones with bitchat in Bluetooth range show here."
                                    :size 13 :color +dim+)))
                 ;; the line being typed
                 (glass:fb-rect fb 0 bottom w input +bar+)
                 (let ((shown (if (plusp (length line)) line
                                  (if show-log "Radio log  ·  Tab for the chat" "Message  ·  Tab for the radio log"))))
                   (let ((x (glass:fb-text fb margin (+ bottom 8) shown :size size
                                           :color (if (plusp (length line)) +ink+ +dim+))))
                     (when (plusp (length line))
                       (glass:fb-rect fb (+ x 1) (+ bottom 8) 2 18 +mine+))))
                 (setf dirty nil)
                 t)))
      (setf mesh (make-mesh :identity (%load-identity) :send #'send-frame))
      (note "chirp on ~A, as ~A (~A)" (machine-type) (identity-nickname (mesh-identity mesh))
            (hex (identity-peer-id (mesh-identity mesh))))
      (if start
          (multiple-value-bind (r reason) (funcall start +service-uuid+ +characteristic-uuid+)
            (setf radio r why (and (null r) reason))
            (unless r (note "radio: ~A" reason)))
          (setf why "no Bluetooth in this Lisp"))
      (values
       ;; ON-KEY
       (lambda (down ks)
         (when down
           (cond ((= ks #xff0d) (say))                                   ; Return
                 ((= ks #xff08) (when (plusp (length line))              ; BackSpace
                                  (setf line (subseq line 0 (1- (length line))) dirty t)))
                 ((= ks #xff09) (setf show-log (not show-log) dirty t))  ; Tab
                 ((= ks #xff1b) (setf line "" dirty t))                  ; Escape
                 (t (let ((ch (%keysym-char ks)))
                      (when (and ch (< (length line) 400))
                        (setf line (concatenate 'string line (string ch)) dirty t)))))))
       nil
       ;; TICK: the radio every frame, the mesh's announces and forgetting once a second
       (lambda ()
         (when radio
           (dolist (e (funcall poll radio))
             (handler-case (on-radio e) (error (c) (note "error: ~A" c)))))
         (let ((now (get-internal-real-time)))
           (when (> (- now last-tick) internal-time-units-per-second)
             (setf last-tick now)
             (handler-case (mesh-tick mesh) (error (c) (note "tick error: ~A" c)))))
         (when dirty (draw)))
       nil
       ;; CLOSE: a leave to whoever is linked, then the radio off
       (lambda ()
         (when radio
           (ignore-errors (chirp::%send-packet mesh (mesh-leave-frame mesh)))
           (ignore-errors (funcall stop radio))
           (setf radio nil)))
       ;; RESIZE
       (lambda (w h) (glass:fb-resize fb w h +paper+) (draw))
       t))))
