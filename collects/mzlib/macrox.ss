
(reference-library "refer.ss")

(begin-elaboration-time
 (invoke-open-unit
  (reference-library "macroxr.ss")))

(define-macro send* send*)
(define-macro local local)
(define-macro recur recur)
(define-macro rec rec)
(define-macro signature->symbols signature->symbols)
