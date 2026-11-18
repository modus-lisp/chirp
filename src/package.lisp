;;;; package.lisp

(defpackage #:chirp
  (:use #:cl)
  (:local-nicknames (#:n #:natrium))
  (:export
   ;; packets
   #:packet #:make-packet #:packet-version #:packet-type #:packet-ttl #:packet-timestamp #:packet-flags
   #:packet-sender #:packet-recipient #:packet-route #:packet-payload #:packet-signature
   #:encode-packet #:decode-packet #:signing-bytes #:sign-packet #:packet-signed-by-p
   #:+announce+ #:+message+ #:+leave+ #:+noise-handshake+ #:+noise-encrypted+ #:+fragment+
   #:+service-uuid+ #:+characteristic-uuid+
   ;; identity
   #:persona #:make-identity #:new-identity #:identity-nickname #:identity-peer-id
   #:identity-noise-public #:identity-signing-public #:identity-plist #:identity-from-plist
   #:peer-id-of #:hex
   ;; the mesh
   #:mesh #:make-mesh #:mesh-identity #:mesh-peers #:mesh-messages #:mesh-receive #:mesh-announce-frame
   #:mesh-say #:mesh-announce #:mesh-leave-frame #:mesh-dropped #:take-announce #:mesh-link-up #:mesh-link-down #:mesh-tick
   #:peer #:peer-id #:peer-nickname #:peer-seen #:peer-links
   #:chat #:chat-sender #:chat-nickname #:chat-text #:chat-timestamp #:chat-mine))
