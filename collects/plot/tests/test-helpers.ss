(module test-helpers mzscheme
  
  (provide apx-equal?)
  
  (define delta .00001)
  
  (define (~= a b) 
    (< (abs (- b a)) delta))

  ; approximate equality for numbers
  (define (apx-equal? a b)
    (cond [(and (pair? a) (pair? b)) (andmap apx-equal? a b) ] ; list case
          [(and (vector? a) (vector? b)) (apx-equal? (vector->list a) (vector->list b))] ; vector case
          [(and (number? a) (number? b)) (~= a b)] ; number
          [else (equal? a b)])))