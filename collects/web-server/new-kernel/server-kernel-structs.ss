(module server-kernel-structs mzscheme
  (provide (struct connection (i-port o-port custodian close?)))

  (define-struct connection (i-port o-port custodian close?) (make-inspector)))
