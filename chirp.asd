;;;; chirp.asd -- a bitchat client in Common Lisp: the BLE mesh chat's wire protocol, over whatever moves the bytes.

(defsystem "chirp"
  :description "bitchat's mesh protocol: packets, signed announces and public messages, fragments, relaying."
  :license "MIT"
  :depends-on ("natrium" "cram")
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "packet")
                             (:file "identity")
                             (:file "mesh")))))
