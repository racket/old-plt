(
 
 ; 6.1
 
 (eqv? (top top -> boolean))
 (eq? (top top -> boolean))
 (equal? (top top -> boolean))
 
 ; 6.2.5
 
 (number? (top -> boolean))
 (complex? (top -> boolean))
 (real? (top -> boolean))
 (rational? (top -> boolean))
 (integer? (top -> boolean))
 
 (exact? (number -> boolean))
 (inexact? (number -> boolean))
 
 (= (number number number *-> boolean))
 (< (real real real *-> boolean))
 (> (real real real *-> boolean))
 (<= (real real real *-> boolean))
 (>= (real real real *-> boolean))
 
 (zero? (number -> boolean))
 (positive? (real -> boolean))
 (negative? (real -> boolean))
 (odd? (integer -> boolean))
 (even? (integer -> boolean))
 
 (max (real real *-> real))
 (min (real real *-> real))
 
 (+ (number *-> number))
 (* (number *-> number))
 
 ; Could be case-lambdas, but we don't have interval analysis and dependant types yet.
 ; See R5RS
 (- (number number *-> number))
 (/ (number number *-> number))

 (abs (real -> real))
 
 (quotient (integer integer -> integer))
 (remainder (integer integer -> integer))
 (modulo (integer integer -> integer))

 
 
 
 
 (cons (forall ([a top]
                [b top])
               (a b -> (cons a b))))
 (car (forall ([a top])
              ((cons a top) -> a)))
 (cdr (forall ([a top])
              ((cons top a) -> a)))
 
 (void (-> (void)))
 
 (pi 3.1)
 
 )
