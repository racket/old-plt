(require-library "pretty.ss")
(require-library "macro.ss")
(call-with-output-file (build-path (collection-path "system") "mred-sig.ss")
  (lambda (port)
    (pretty-print
     `(define-signature mred^ ,(vector->list (signature->symbols mred^)))
     port))
  'truncate)
