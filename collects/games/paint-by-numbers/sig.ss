(require-library "functios.ss")
(require-library "spidey.ss")

(define-signature GUI^ (paint-by-numbers-canvas%))
(define-signature MAIN^ (canvas 
			 problem
			 (struct problem (name rows cols))))
(define-signature SOLVE^ (solve))
