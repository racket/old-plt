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
   (define zsqrt sqrt) 
   
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
   
   ;; basic circular functions
   (define zsin
     (lambda (x)
       (if (= (imag-part x) 0.0)
	   (sin (real-part x))
	   (begin
	     (define z (zchsh (imag-part x)))
	     (make-complex
	      (* (sin (real-part x)) (real-part z))
	      (* (cos (real-part x)) (imag-part z)))))))
   
   (define zcos
     (lambda (x)
       (if (= (imag-part x) 0.0)
	   (cos (real-part x))
	   (begin
	     (define z (zchsh (imag-part x)))
	     (make-complex
	      (* (cos (real-part x)) (real-part z))
	      (* (- (sin (real-part x))) (imag-part z)))))))
   
   (define ztan
     (lambda (z)
       (if (= (imag-part z) 0.0)
	   (tan (real-part z))
	   (begin
	     (define d (+ (cos  (* 2.0 (real-part z)))
			  (cosh (* 2.0 (imag-part z)))))
	     (let ([d (if (< (abs d) 0.25)
			  (ztans z)
			  d)])
	       (make-complex
		(/ (sin  (* 2.0 (real-part z))) d)
		(/ (sinh (* 2.0 (imag-part z))) d)))))))
   
   ;; inverse circular functions
   ;; asin(z) = -i * log(i*z + sqrt(1 - z^2))
   (define zasin
     (lambda (z)
       (define x (real-part z))
       (define y (imag-part z))
       (if (= y 0.0)
	   (asin x)
	   (begin
	     (define ca z)
	     (define ct (make-complex (- y) x))
	     ;; zz = sqrt(1 - z^2)
	     (define zz 
	       (zsqrt
		(make-complex 
		 (- 1.0 (* (- x y) (+ x y)))
		 (* -2.0 x y))))
	     (let ([zz (zlog (+ zz ct))])
	       (make-complex
		(imag-part zz)
		(- (real-part zz))))))))
   
   ;; acos(z) = pi/2 - asin(z)
   (define zacos
     (lambda (z)
       (if (= (imag-part z) 0.0)
	   (acos (real-part z))
	   (begin
	     (define z2 (zasin z))
	     (make-complex
	      (- (/ pi 2.0) (real-part z2))
	      (- (imag-part z2)))))))
   
   ;; w = atan(z) = atan(x + iy)
   ;; Re w = 1/2 atan((2 * x) / (1 - x^2 - y^2)) + k * PI
   ;; Im w = 1/4 log((x^2 + (y + 1)^2) / (x^2 + (y - 1)^2))
   ;; where k is an arbitrary integer
   (define zatan
     (lambda (z)
       (define x (real-part z))
       (define y (imag-part z))
       (if (= y 0.0)
	   (atan x)
	   (begin
	     (define x2 (* x x))
	     (define a (- 1.0 x2 (* y y)))
	     (define t (/ (atan (* 2.0 x) a) 2.0))
	     (define x1 (redupi t))
	     (let* ([t (- y 1.0)]
		    [a (+ x2 (* t t))]
		    [t (+ y 1.0)]
		    [a (/ (+ x2 (* t t)) a)])
	       (make-complex x1 (/ (log a) 4.0)))))))
   
   ;; natural log and exp
   (define zlog
     (lambda (z)
       (if (and 
	    (= (imag-part z) 0.0)
	    (> (real-part z) 0.0))
	   (log (real-part z))
	   (begin
	     (define r (magnitude z))
	     (make-complex
	      (log r)
	      (atan (imag-part z) (real-part z)))))))
   
   (define zexp
     (lambda (z)
       (if (= (imag-part z) 0.0)
	   (exp (real-part z))
	   (expt e z))))

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
