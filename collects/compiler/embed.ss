
(module embed mzscheme
  (require (lib "unitsig.ss"))
  
  (require "sig.ss")

  (require "embed-unit.ss"
	   "embed-sig.ss")

  (define-values/invoke-unit/sig compiler:embed^
    compiler:embed@
    #f)

  (provide-signature-elements compiler:embed^))

