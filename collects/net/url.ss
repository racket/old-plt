(module url mzscheme
  (require (lib "unitsig.ss")
           "url-sig.ss"
           "url-unit.ss")
  (provide-signature-elements net:url^)

  (define-values/invoke-unit/sig net:url^ url@))

