(module error-sig mzscheme
  (require (lib "unitsig.ss"))
  (provide errorS)
  (define-signature errorS (check-arg check-arity check-proc)))