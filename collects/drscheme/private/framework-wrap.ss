(module framework-wrap mzscheme
  (require (lib "unitsig.ss")
           "mred-wrap.ss"
           (prefix orig: (lib "mred-sig.ss" "mred"))
           (lib "framework-unit.ss" "framework"))

  (provide-signature-elements framework^)
  
  (define-values/invoke-unit/sig framework^ framework@ #f mred^))