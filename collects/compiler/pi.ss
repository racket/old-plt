;;;; "pi.scm", program for computing digits of numerical value of PI.
;;; Copyright (C) 1991 Aubrey Jaffer.
;;; See the file `COPYING' for terms applying to this program.

;;; (pi <n> <d>) prints out <n> digits of pi in groups of <d> digits.

;;; 'Spigot' algorithm origionally due to Stanly Rabinowitz.
;;; This algorithm takes time proportional to the square of <n>/<d>.
;;; This fact can make comparisons of computational speed between systems
;;; of vastly differring performances quicker and more accurate.

;;; Try (pi 100 5)
;;; The digit size <d> will have to be reduced for larger <n> or an
;;; overflow error will occur (on systems lacking bignums).

;;; File modified for MzScheme compiler Sept 16, 1996
;;   [do form translated]

(define (pi n args)
  (let* ([d args]

	 [r (let loop ([s 1] [i 0])
	      (if (>= i d)
		  s
		  (loop (* 10 s) (+ 1 i))))]

	 [n (+ (quotient n d) 1)]
	 [m (quotient (* n (* d 3322)) 1000)]
         [a (make-vector (+ 1 m) 2)]
	 [out (open-output-string)])
    
    (vector-set! a m 4)

    (let loop1 ([j 1] [q 0] [b 2])
      (if (> j n)
	  (void)
	  (begin
	    (let loop2 ([k m])
	      (if (zero? k)
		  (void)
		  (begin
		    (set! q (+ q (* (vector-ref a k) r)))
		    (let ([t (+ 1 (* 2 k))])
		      (vector-set! a k (remainder q t))
		      (set! q (* k (quotient q t))))
		    (loop2 (- k 1)))))
	    (let ([s (number->string (+ b (quotient q r)))])
	      (let loop3 ([l (string-length s)])
		(if (>= l d)
		    (display s out)
		    (begin
		      (display #\0 out)
		      (loop3 (+ 1 l))))))
	    (if (zero? (modulo j 10)) (newline out) (display #\space out))
			 
	    (loop1 (+ 1 j) 0 (remainder q r)))))
    out))
    
;    (do ([j 1 (+ 1 j)]
;	 [q 0 0]
;	 [b 2 (remainder q r)])
;	((> j n))
;      
 ;     (do ([k m (- k 1)])
	;;  ((zero? k))
;	
;	(set! q (+ q (* (vector-ref a k) r)))
;	(let ([t (+ 1 (* 2 k))])
;	  (vector-set! a k (remainder q t))
;	  (set! q (* k (quotient q t)))))
;      (let ([s (number->string (+ b (quotient q r)))])
;	(do ([l (string-length s) (+ 1 l)])
;	    ((>= l d) (display s))
;	  (display #\0)))
;      (if (zero? (modulo j 10)) (newline) (display #\space)))
;    (newline)))

;
