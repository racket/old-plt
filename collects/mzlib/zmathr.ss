;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; zmath.ss: some basic complex math routines
;;
;; (mutilated versions from Steve Moshier's Cephes lib)
;; - freely translated for mzscheme by K.Howard 3/96
;; - each function checks to see if built-in real functions
;; - will suffice (ie. imaginary part = 0 and/or real part > 0)
;; - this should be faster, and also improve catching domain 
;; - error exceptions
;; - note: there are portions of this code I DON'T understand,
;;   (ie: ztans [see below]) but seem to give good answers 
;; - so caveat emptor ;-}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (unit/sig
   mzlib:zmath^
   (import)

   ;; circular constants and aliases
   (define e (exp 1.0)) 
   (define pi (* 4.0 (atan 1.0))) 
   (define make-complex make-rectangular) 
   (define zabs magnitude) 
   
   ;; sgn function 
   (define sgn
     (lambda (x)
       (cond
	((< x 0) -1)
	((> x 0)  1)
	(else 0))))
   
   ;; complex conjugate
   (define conjugate
     (lambda (z)
       (make-complex 
	(real-part z)
	(- (imag-part z)))))
   
   ;; real hyperbolic functions
   (define sinh 
     (lambda (x)
       (/
	(- (exp x) (exp (- x)))
	2.0)))

   (define cosh
     (lambda (x)
       (/
	(+ (exp x) (exp (- x)))
	2.0)))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;             internal utilities             ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   ;; compute cosh and sinh of a real number
   (define zchsh
     (lambda (x)
       (if (<= (abs x) 0.5)
	   (make-complex (cosh x) (sinh x))
	   (begin
	     (define e1 (exp x))
	     (define e2 (/ 0.5 e1))
	     (define e3 (* 0.5 e1))
	     (make-complex (+ e3 e2) (- e3 e2))))))

   ;; subtract the nearest integer multiple of pi
   ;; uses extended precision value of pi
   (define redupi 
     (lambda (x)
       ;; constants for extended precision 
       ;; note: see Cephes lib for machine-specific stuff
       (define DP1 3.14159265160560607910E0)
       (define DP2 1.98418714791870343106E-9)
       (define DP3 1.14423774522196636802E-17)

       (define t (/ x pi))
       (let ([t (if (>= t 0.0)
		    (floor (+ t 0.5))
		    (floor (- t 0.5)))])
	 (- 
	  (-
	   (- 
	    x
	    (* t DP1))
	   (* t DP2))
	  (* t DP3)))))

   ;; Taylor series expansion for cosh(2y) - cos(2x)
   ;; (pretty obscure implementation ;-)
   (define ztans 
     (lambda (z)
       ;; constants for extended precision 
       ;; note: see Cephes lib for machine-specific stuff
       (define MACHEP  1.11022302462515654042E-16)

       (define x-1 (abs (* 2.0 (real-part z))))
       (define y-1 (abs (* 2.0 (imag-part z))))
       (define x-2 (redupi x-1))
       (define x (* x-2 x-2))
       (define y (* y-1 y-1))
       (define x2 1.0)
       (define y2 1.0)
       (define f  1.0)
       (define rn 0.0)
       (define d  0.0)
       (define t  0.0)
       (do () ((<= (abs (/ t d)) MACHEP))
	 (set! rn (+ rn 1.0))
	 (set! f (* f rn))
	 (set! rn (+ rn 1.0))
	 (set! f (* f rn))
	 (set! x2 (* x x2))
	 (set! y2 (* y y2))
	 (set! t (+ y2 x2))
	 (set! t (/ t f))
	 (set! d (+ d t))
	 
	 (set! rn (+ rn 1.0))
	 (set! f (* f rn))
	 (set! rn (+ rn 1.0))
	 (set! f (* f rn))
	 (set! x2 (* x x2))
	 (set! y2 (* y y2))
	 (set! t (- y2 x2))
	 (set! t (/ t f))
	 (set! d (+ d t)))
       d))
  )
