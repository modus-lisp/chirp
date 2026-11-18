# chirp

A [bitchat](https://github.com/permissionlesstech/bitchat) client in Common Lisp: the Bluetooth LE mesh chat's
wire protocol, with the radio left to whoever hosts it.

It speaks bitchat's public channel with the App Store app today:

- **Packets** — bitchat's binary format (v1 and v2 headers, recipient and route, padding), read and written byte
  for byte; compressed payloads (raw DEFLATE) read, and re-sent exactly as they came.
- **Identity** — a Noise static key (X25519; the 8-byte peer id is SHA-256 of it) and an Ed25519 signing key.
- **Announces and public messages** — signed, and shown only when the signature checks against the key the sender
  announced (pinned on first sight).
- **The mesh** — relaying with TTL, dedup, fragments put back together.

Not yet: private messages (Noise XX), sending compressed payloads.

## Layout

- `src/packet.lisp` — the packet format, signing bytes
- `src/identity.lisp` — keys, peer ids, the announce
- `src/mesh.lisp` — a node: what comes in over links, what goes out, who is about, what was said
- `src/app.lisp` (`chirp/app`) — the chat on an iPhone, in native screens over [modus](https://github.com/modus-lisp/modus)'s
  iOS host, which supplies the Bluetooth links; the secret keys in the system keychain (natrium's gossamer)

The protocol has no radio in it: a link is whatever the host calls a connection, and `mesh-receive` takes the frames
it delivers while the mesh's `send` function hands frames back.

## Tests

```
sbcl --script test/core.lisp
```

Three nodes in a line, with no radios: announces, a message relayed two hops, a forgery refused, a long message
fragmented and rebuilt, leaving.

Depends on [natrium](https://github.com/modus-lisp/natrium) (crypto) and [cram](https://github.com/modus-lisp/cram)
(inflate).  MIT.
