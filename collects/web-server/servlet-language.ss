(module servlet-language mzscheme
  (require "servlet-primitives.ss"
           "servlet-sig.ss"
           (lib "unitsig.ss"))
  (provide (all-from-except "servlet-sig.ss" servlet^)
           (all-from mzscheme))
  (provide-signature-elements servlet^)
  
  (define-values/invoke-unit/sig servlet^ servlet@ #f))
