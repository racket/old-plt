
(reference-library "refer.ss")

(begin-elaboration-time
 (invoke-open-unit
  (reference-library "matchr.ss")))

(define-macro match match)
(define-macro match-lambda match-lambda)
(define-macro match-lambda* match-lambda*)
(define-macro match-letrec match-letrec)
(define-macro match-let match-let)
(define-macro match-let* match-let*)
