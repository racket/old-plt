(define a #t)

(define (b x)
  (and a #t x))

(b #f)

(define c #f)

(define (d x)
  (or c #f x))

(d #f)
