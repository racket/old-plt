
(require-library "refer.ss")

(require-library "macrox.ss")

(begin-elaboration-time
 (invoke-open-unit
  (require-library "macror.ss"))
 (invoke-open-unit
  (require-library "letplusr.ss")))

(define-macro class-asi class-asi)
(define-macro class*-asi class*-asi)
(define-macro opt-lambda opt-lambda)
(define-macro let+ let+)
