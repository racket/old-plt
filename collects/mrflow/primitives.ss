
(
 ; When are we going to be able to compute all this directly from an S-exp version of R5RS ?
 
 ; 6.1 Equivalence predicates
 
 (eqv? (top top -> boolean))
 (eq? (top top -> boolean))
 (equal? (top top -> boolean))
 
 
 ; 6.2.5 Numerical operations
 
 ; in Scheme it seems that positive = strictly positive and
 ; negative = strictly negative
 
 (number? (top -> boolean))
 (complex? (top -> boolean))
 (real? (top -> boolean))
 (rational? (top -> boolean))
 (integer? (top -> boolean))
 
 (exact? (complex -> boolean))
 (inexact? (complex -> boolean))
 
 (= (complex complex complex *-> boolean))
 (< (real real real *-> boolean))
 (> (real real real *-> boolean))
 (<= (real real real *-> boolean))
 (>= (real real real *-> boolean))
 
 (zero? (complex -> boolean))
 (positive? (real -> boolean))
 (negative? (real -> boolean))
 (odd? (integer -> boolean))
 (even? (integer -> boolean))
 
 ; if any arg inexact => result inexact
 (max (real real *-> real))
 ; if any arg inexact => result inexact
 (min (real real *-> real))
 
 ; no arg => 0
 ; z => z
 (+ (forall ([z complex])
            (case-lambda
             [() 0]
             [(z) z]
             [complex complex])))
 ; no arg => 1
 ; z => z
 (* (forall ([z complex])
            (case-lambda
             [() 1]
             [(z) z]
             [complex complex])))
 
 ; z => -z
 (- (complex complex *-> complex))
 ; z => 1/z
 (/ (complex complex *-> complex))
 
 ; returns non-negative real
 (abs (real -> real))
 
 ; second arg non-zero
 (quotient (integer integer -> integer))
 ; second arg non-zero
 ; result has same sign as first arg
 (remainder (integer integer -> integer))
 ; second arg non-zero
 ; result has same sign as second arg
 (modulo (integer integer -> integer))
 
 ; no arg => 0
 ; n => n (from math)
 ; result is non-negative integer
 (gcd (forall ([a integer])
              (case-lambda
               [() 0]
               [(a) a]
               [integer integer])))
 ; no arg => 1
 ; n => n (from math)
 ; result is non-negative integer
 (lcm (forall ([a integer])
              (case-lambda
               [() 1]
               [(a) a]
               [integer integer])))
 
 (numerator (rational -> integer))
 ; result always positive
 ; 0 => 1
 (denominator (rational -> integer))
 
 (floor (real -> integer))
 (ceiling (real -> integer))
 (truncate (real -> integer))
 (round (real -> integer))
 
 (rationalize (real real -> rational))
 
 (exp (complex -> complex))
 (log (complex -> complex))
 (sin (complex -> complex))
 (cos (complex -> complex))
 (tan (complex -> complex))
 (asin (complex -> complex))
 (acos (complex -> complex))
 (atan (case-lambda
        [(complex) complex]
        [(real real) complex]))
 
 ; positive real part, or zero real part and non-negative imaginary part
 (sqrt (complex -> complex))
 
 ; (expt 0 0) = 1
 ; (expt 0 z) = 0
 (expt (complex complex -> complex))
 
 (make-rectangular (real real -> complex))
 (make-polar (real real -> complex))
 (real-part (complex -> real))
 (imag-part (complex -> real))
 ; returns non-negative real
 (magnitude (complex -> real))
 (angle (complex -> real))
 
 (exact->inexact (complex -> inexact-complex))
 (inexact->exact (complex -> exact-complex))
 
 
 ; 6.2.6 Numerical input and output
 
 ; this really ougth to be called complex->string and string->complex,
 ; especially since R5RS explicitely uses a "z" as the first argument
 ; name... R5RS seems to actually confuse complex and number quite a lot,
 ; despite the second note in section 6.2.5, page 21.
 
 ; radix is either 2, 8, 10, or 16
 (number->string (case-lambda
                  [(complex) string]
                  [(complex exact-integer) string]))
 
 ; radix is either 2, 8, 10, or 16
 (string->number (case-lambda
                  [(string) (union complex #f)]
                  [(string exact-integer) (union complex #f)]))
 
 
 ; 6.3.1 Booleans
 
 (not (boolean -> boolean))
 
 (boolean? (top -> boolean))
 
 
 ; 6.3.2 Pairs and lists
 
 (pair? (top -> boolean))
 
 (cons (forall ([a top]
                [b top])
               (a b -> (cons a b))))
 
 (car (forall ([a top])
              ((cons a top) -> a)))
 
 (cdr (forall ([a top])
              ((cons top a) -> a)))
 
 ; set-car!
 
 ; set-cdr!
 
 ; ouch
 (caar (forall ([a top])
               ((cons (cons a top) top) -> a)))
 (cdar (forall ([a top])
               ((cons (cons top a) top) -> a)))
 (cadr (forall ([a top])
               ((cons top (cons a top)) -> a)))
 (cddr (forall ([a top])
               ((cons top (cons top a)) -> a)))
 (caaar (forall ([a top])
                ((cons (cons (cons a top) top) top) -> a)))
 (cdaar (forall ([a top])
                ((cons (cons (cons top a) top) top) -> a)))
 (cadar (forall ([a top])
                ((cons (cons top (cons a top)) top) -> a)))
 (cddar (forall ([a top])
                ((cons (cons top (cons top a)) top) -> a)))
 (caadr (forall ([a top])
                ((cons top (cons (cons a top) top)) -> a)))
 (cdadr (forall ([a top])
                ((cons top (cons (cons top a) top)) -> a)))
 (caddr (forall ([a top])
                ((cons top (cons top (cons a top))) -> a)))
 (cdddr (forall ([a top])
                ((cons top (cons top (cons top a))) -> a)))
 (caaaar (forall ([a top])
                 ((cons (cons (cons (cons a top) top) top) top) -> a)))
 (cdaaar (forall ([a top])
                 ((cons (cons (cons (cons top a) top) top) top) -> a)))
 (cadaar (forall ([a top])
                 ((cons (cons (cons top (cons a top)) top) top) -> a)))
 (cddaar (forall ([a top])
                 ((cons (cons (cons top (cons top a)) top) top) -> a)))
 (caadar (forall ([a top])
                 ((cons (cons top (cons (cons a top) top)) top) -> a)))
 (cdadar (forall ([a top])
                 ((cons (cons top (cons (cons top a) top)) top) -> a)))
 (caddar (forall ([a top])
                 ((cons (cons top (cons top (cons a top))) top) -> a)))
 (cdddar (forall ([a top])
                 ((cons (cons top (cons top (cons top a))) top) -> a)))
 (caaadr (forall ([a top])
                 ((cons top (cons (cons (cons a top) top) top)) -> a)))
 (cdaadr (forall ([a top])
                 ((cons top (cons (cons (cons top a) top) top)) -> a)))
 (cadadr (forall ([a top])
                 ((cons top (cons (cons top (cons a top)) top)) -> a)))
 (cddadr (forall ([a top])
                 ((cons top (cons (cons top (cons top a)) top)) -> a)))
 (caaddr (forall ([a top])
                 ((cons top (cons top (cons (cons a top) top))) -> a)))
 (cdaddr (forall ([a top])
                 ((cons top (cons top (cons (cons top a) top))) -> a)))
 (cadddr (forall ([a top])
                 ((cons top (cons top (cons top (cons a top)))) -> a)))
 (cddddr (forall ([a top])
                 ((cons top (cons top (cons top (cons top a)))) -> a)))
 
 (null? (top -> boolean))
 
 (list? (top -> boolean))
 
 (list (forall ([a top])
               (a *-> a)))
 
 ;(length ((listof top) -> exact-integer))
 
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; not R5RS, just for testing
 
 (id (forall ([a top]) (a -> a)))
 
 (void (-> (void)))
 
 (pi 3.1)
 
 )
