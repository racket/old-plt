#|

Rough BNF

(class/d
 super-expresion
 init-args
 ((public var ...)
  (override var ...)
  (inherit var ...)
  (rename (var var) ...))
 
  definitions-and-expressions ...)

;; only thing wrong with above bnf is that the public, etc. clauses
;; can appear multiple times in that same section. 

|#

(require-library "refer.ss")
(begin-elaboration-time
 (define-values/invoke-unit
  (class/d class/d* class/d*/names)
   (require-library "classdr.ss")))

(define-macro class/d class/d)
(define-macro class/d* class/d*)
(define-macro class/d*/names class/d*/names)
