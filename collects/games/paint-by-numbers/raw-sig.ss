#|

types:

  (define-type cols (listof (listof number)))
  (define-type rows (listof (listof number)))

  (define-type solution (vector-of (vector-of (union 'on 'off 'unknown))))

  (define-type problem (make-problem string cols rows (union #f solution)))

|#

(require-library "cores.ss")

(define-signature GUI^
  (paint-by-numbers-canvas%
   design-paint-by-numbers-canvas%))

(define-signature SOLVE^
  (solve)) ; : ((list-of (list-of nat)) (list-of (list-of nat)) -> void)

(define-signature BOARD^ ())

