(module server-kernel-structs mzscheme
  (provide (struct connection (i-port o-port custodian close?))
           (struct request-line (method uri-string major-version minor-version))
           )

  (define-struct connection (i-port o-port custodian close?) (make-inspector))
  (define-struct request-line (method uri-string major-version minor-version))
  )
