
; Contributed by Shriram
; This code steals heavily from Dorai Sitaram's implementation.

(reference-library "refer.ss")

(begin-elaboration-time
 (invoke-open-unit
  (require-library "synruler.ss")))

(define-macro define-syntax define-syntax)

(keyword-name '-:sr:tag)
(keyword-name '-:sr:untag)
(keyword-name '-:sr:flatten)
(keyword-name '-:sr:matches-pattern?)
(keyword-name '-:sr:get-bindings)
(keyword-name '-:sr:expand-pattern)

