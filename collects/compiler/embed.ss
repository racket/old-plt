
(module embed mzscheme
  (require (lib "unitsig.ss"))
  
  (require "sig.ss")

  (require "embed-unit.ss")

  (define-values/invoke-unit/sig (make-embedding-executable)
    compiler:embed@
    #f)

  (provide-signature-elements (make-embedding-executable)))
