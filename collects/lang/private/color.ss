(module color mzscheme
  (provide (struct color (red green blue))
           (struct alpha-color (alpha red green blue))
           color-list?
           alpha-color-list?)
           
  (define (color-list? l)
    (and (list? l) (andmap color? l)))
  (define (alpha-color-list? l)
    (and (list? l) (andmap alpha-color? l)))

  (define-struct color (red green blue) (make-inspector))
  (define-struct alpha-color (alpha red green blue) (make-inspector)))
	   