
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
             [(rest complex complex (listof complex)) complex]
             [() 0]
             [(z) z]
             )))
 ; no arg => 1
 ; z => z
 (* (forall ([z complex])
            (case-lambda
             [(rest complex complex (listof complex)) complex]
             [() 1]
             [(z) z]
             )))
 
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
               [(rest integer integer (listof integer)) integer]
               [() 0]
               [(a) a]
               )))
 ; no arg => 1
 ; n => n (from math)
 ; result is non-negative integer
 (lcm (forall ([a integer])
              (case-lambda
               [(rest integer integer (listof integer)) integer]
               [() 1]
               [(a) a]
               )))
 
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
 
 ; the rest argument does all the work
 (list (forall ([a top])
               (case-lambda
                [(rest a) a])))
 
 (length ((listof top) -> exact-integer))
 
 (append (forall ([a top]
                  [b top][c top][d top])
                 (case-lambda
                  ; the last element could be not a list => improper list
                  [(rest (listof b) (listof c) (listof d)) (union d (listof (union b c)))]
                  [() ()]
                  [(a) a]
                  )))
 
 (reverse (forall ([a top])
                  ((listof a) -> (listof a))))
 
 ; exact-integer should be non-negative...
 (list-tail (forall ([a top])
                    ((listof a) exact-integer -> (listof a))))
 
 (list-ref (forall ([a top])
                   ((listof a) exact-integer -> a)))
 
 (memq (forall ([a top]
                [b top])
               (a (listof b) -> (union #f (cons a (listof b))))))
 (memv (forall ([a top]
                [b top])
               (a (listof b) -> (union #f (cons a (listof b))))))
 (member (forall ([a top]
                  [b top])
                 (a (listof b) -> (union #f (cons a (listof b))))))
 
 (assq (forall ([a top]
                [b (cons top top)])
               (a (listof b) -> (union #f (cons (cons a top) (listof b))))))
 (assv (forall ([a top]
                [b (cons top top)])
               (a (listof b) -> (union #f (cons (cons a top) (listof b))))))
 (assoc (forall ([a top]
                 [b (cons top top)])
                (a (listof b) -> (union #f (cons (cons a top) (listof b))))))
 
 
 ; 6.3.3. Symbols
 
 (symbol? (top -> boolean))
 
 (symbol->string (symbol -> string))
 
 (string->symbol (string -> symbol))
 
 
 ; 6.3.4 Characters
 
 (char? (top -> boolean))
 
 (char=? (char char -> boolean))
 (char<? (char char -> boolean))
 (char>? (char char -> boolean))
 (char<=? (char char -> boolean))
 (char>=? (char char -> boolean))
 
 (char-ci=? (char char -> boolean))
 (char-ci<? (char char -> boolean))
 (char-ci>? (char char -> boolean))
 (char-ci<=? (char char -> boolean))
 (char-ci>=? (char char -> boolean))
 
 (char-alphabetic? (char -> boolean))
 (char-numeric? (char -> boolean))
 (char-whitespace? (char -> boolean))
 (char-upper-case? (letter -> boolean))
 (char-lower-case? (letter -> boolean))
 
 ; R5RS doesn't say the integer has to be positive...
 (char->integer (char -> exact-integer))
 (integer->char (exact-integer -> char))
 
 (char-upcase (char -> char))
 (char-downcase (char -> char))
 
 
 ; 6.3.5 Strings
 
 (string? (top -> boolean))
 
 ; integer should be non-negative
 (make-string (case-lambda
               [(exact-integer) string]
               [(exact-integer char) string]))
 
 (string (case-lambda
          [(rest char (listof char)) string]
          [() ""]
          ))
 
 ; exact positive integer ? exact integer ? integer ? 
 (string-length (string -> exact-integer))
 
 (string-ref (string exact-integer -> char))
 
 ; string-set!
 
 (string=? (string string -> boolean))
 (string-ci=? (string string -> boolean))
 
 (string<? (string string -> boolean))
 (string>? (string string -> boolean))
 (string<=? (string string -> boolean))
 (string>=? (string string -> boolean))
 (string-ci<? (string string -> boolean))
 (string-ci>? (string string -> boolean))
 (string-ci<=? (string string -> boolean))
 (string-ci>=? (string string -> boolean))
 
 (substring (string exact-integer exact-integer -> string))
 
 (string-append (forall ([a string])
                        (case-lambda
                         ; the last element could be not a list => improper list
                         [(rest string string (listof string)) string]
                         [() ""]
                         [(a) a]
                         )))
 
 (string->list (string -> (listof char)))
 (list->string ((listof char) -> string))
 
 ; (string-copy (forall ([a string]) (a -> a))) works only if we don't have string-set!
 (string-copy (string -> string))
 
 ; string-fill!
 
 
 ; 6.3.6 Vectors
 
 (vector? (top -> boolean))
 
 ; integer should be non-negative
 (make-vector (forall ([a top])
                      (case-lambda
                       [(exact-integer) (vector top)]
                       [(exact-integer a) (vector a)])))
 
 (vector (forall ([a top])
                 (a *-> (vector a))))
 
 (vector-length ((vector top) -> exact-integer))
 
 (vector-ref (forall ([a top])
                     ((vector a) exact-integer -> a)))
 
 ; vector-set!
 
 (vector->list (forall ([a top])
                       ((vector a) -> (listof a))))
 (list->vector (forall ([a top])
                       ((listof a) -> (vector a))))
 
 ; vector-fill!
 
 
 ; 6.4 Control features
 
 (procedure? (top -> boolean))
 
 (apply (forall ([a top][b top][c top])
                (case-lambda
                 ; this would almost work, except for the last argument, that would
                 ; show up as a list in the result
                 ;[(rest (case-lambda
                 ;        [(rest a) b])
                 ;       a)
                 ; b])))
                 ; so we have to deconstruct everything, and be *very* conservative.
                 ; This *will* raise errors about '() not being a pair, but that's the
                 ; best we can if we want to cover all the possible cases.
                 [(rest (case-lambda
                         [(rest (listof (union a b))) c])
                        (listof (union a (listof b))))
                  c])))
 
 (map (forall ([a top][b top]
               [c top][d top][e top]
               [f top][g top][h top][i top]
               [j top][k top][l top][m top][n top]
               [o top][p top][q top][r top][s top][t top]
               )
              (case-lambda
               [((a -> b) (listof a)) (listof b)]
               [((c d -> e) (listof c) (listof d)) (listof e)]
               [((f g h -> i) (listof f) (listof g) (listof h)) (listof i)]
               [((j k l m -> n) (listof j) (listof k) (listof l) (listof m)) (listof n)]
               [((o p q r s -> t) (listof o) (listof p) (listof q) (listof r) (listof s)) 
                (listof t)]
               ; use at your own risks: you'll loose arity checking and get spurious errors
               ; about '() not being a pair (but the result of map will be properly conservative,
               ; so if you ignore the errors for map itself anf make sure the arity of the
               ; function given to map is correct, then you might be able to use the output
               ; of map to detect errors down the flow - except that the output of map will be
               ; a list => using car on it or stuff like that will trigger another error...)
               ; The whole problem is that map needs a dependent type...
               ;[(rest
               ;  (case-lambda
               ;   [(rest o p q r (listof s)) t])
               ;  (listof o) (listof p) (listof q) (listof r) (listof (listof s)))
               ; (listof t)]
               )))
 
 (for-each (forall ([a top];[b top]
                    [c top][d top];[e top]
                    [f top][g top][h top];[i top]
                    [j top][k top][l top][m top];[n top]
                    [o top][p top][q top][r top][s top];[t top]
                    )
                   (case-lambda
                    [((a -> top) (listof a)) void]
                    [((c d -> top) (listof c) (listof d)) void]
                    [((f g h -> top) (listof f) (listof g) (listof h)) void]
                    [((j k l m -> top) (listof j) (listof k) (listof l) (listof m)) void]
                    [((o p q r s -> top) (listof o) (listof p) (listof q) (listof r) (listof s)) 
                     void]
                    ; use at your own risks: you'll loose arity checking and get spurious errors
                    ; about '() not being a pair (but the result of map will be properly conservative,
                    ; so if you ignore the errors for map itself anf make sure the arity of the
                    ; function given to map is correct, then you might be able to use the output
                    ; of map to detect errors down the flow - except that the output of map will be
                    ; a list => using car on it or stuff like that will trigger another error...)
                    ; The whole problem is that map needs a dependent type...
                    ;[(rest
                    ;  (case-lambda
                    ;   [(rest o p q r (listof s)) top])
                    ;  (listof o) (listof p) (listof q) (listof r) (listof (listof s)))
                    ; void]
                    )))

 ; (delay expr) => (#%app make-a-promise (lambda () expr))
 ; if we have the arrow type in the argument of make-a-promise, then the application
 ; will happen immediately, which we don't want. So instead
 ; a will be the thunk, and having the arrow type for this thunk in the type for force
 ; will force the application of the thunk inside force.
 ; pp-type for promises just "forgets" to show the enclosing thunk part of the type.
 ; It's ugly, but it works, and it works well enough to approximate memoization.
 (make-a-promise (forall ([a top])
                         (a -> (promise a))))
 (force (forall ([a top])
                ((promise (-> a)) -> a)))

 (call-with-current-continuation (forall ([a top]
                                          [b top])
                                         (((a -> bottom) -> b) -> (union a b))))

 ; correct, but currently triggers a bug.
 ;(call-with-current-continuation (forall ([a top]
 ;                                         [b top])
 ;                                        ((; continuation
 ;                                          (case-lambda [(rest a) bottom])
 ;                                          ;result of body of lambda that
 ;                                          ;receives the continuation
 ;                                          -> b)
 ;                                         ; result of call/cc
 ;                                         -> (union (values a) b))))

 ; multiple values are simulated internally as a list...
 (values (forall ([a top])
                 (case-lambda
                  [(rest a) (values a)]
                  )))
 
 (call-with-values (forall ([a top] ; one or multiple values
                            [b top])
                           (case-lambda
                            [((case-lambda
                               [() (values a)])
                              (case-lambda
                               [(rest a) b]))
                             b])))
 
; this limited values works fine, but then call-with-values doesnt', because all the clauses
; in call-with-values would have only two arguments, making discrimination between the different
; cases impossible.
; (values (forall ([a top]
;                  [b top][c top]
;                  [d top][e top][f top]
;                  [g top][h top][i top][j top]
;                  [k top][l top][m top][n top][o top])
;                 (case-lambda
;                  [() (values)]
;                  [(a) (values a)]
;                  [(b c) (values b c)]
;                  [(d e f) (values d e f)]
;                  [(g h i j) (values g h i j)]
;                  [(k l m n o) (values k l m n o)])))
;
; (call-with-values (forall ([a top][b top]
;                            [c top][d top][e top])
;                           (case-lambda
;                            [((case-lambda [() (values a)]) (case-lambda [(a) b])) b]
;                            [((case-lambda [() (values c d)]) (case-lambda [(c d) e])) e]
;                            )))
 
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; not R5RS, just for testing
 
 (id (forall ([a top]) (a -> a)))
 (make-func (-> (-> 1)))
 (void (-> void))
 (foo (cons 1 2))
 (pi 3.1)
 ; one required argument that has to be a list, the elements are then extracted
 (gather-one1 (forall ([a top])
                      ((listof a) -> a)))
 (gather-one2 (forall ([a top])
                      (case-lambda
                       [((listof a)) a])))
 ; unknown number of arguments that are converted into a list by the rest argument,
 ; then extracted
 (gather-many1 (forall ([a top])
                       (a *-> a)))
 (gather-many2 (forall ([a top])
                       (case-lambda
                        [(rest (listof a)) a])))
 ; don't try this at home
 ;(gather-other (forall ([a top])
 ;                      ((a) *-> a)))
 
 (gen-nums (-> (listof number)))
 
 (apply-gen (forall ([a top]
                     [b top])
                    (case-lambda
                     [((case-lambda [(rest a) b]) a) b])))
 
 (lnum (forall ([a top])
               ((listof a) -> a)))
 )
