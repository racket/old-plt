#|

types:

  (define-type cols (listof (listof number)))
  (define-type rows (listof (listof number)))

  (define-type solution ???) ;; unspecified as of yet

  (define-type problem (make-problem string cols rows (union #f solution)))

|#

(require-library "functios.ss")
(require-library "prettys.ss")
(require-library "spidey.ss")

(define-signature GUI^ (paint-by-numbers-canvas%))
(define-signature MAIN^ (canvas 
			 problem
			 (struct problem (name rows cols))))
(define-signature SOLVE^ (solve))
