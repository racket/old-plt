(define regular-poly
  (lambda (sides radius)
    (let* ([theta (/ (* 2 pi) sides)]
	   [side-len (* 2 radius (sin (/ theta 2)))])
      (letrec ([draw-sides
		(lambda (n)
		  (unless (zero? n)
		    (draw side-len)
		    (turn/radians theta)
		    (draw-sides (sub1 n))))])
	(tprompt (move radius)
		 (turn/radians (/ (+ pi theta) 2))
		 (draw-sides sides))))))

(define regular-polys
  (lambda (sides s)
    (letrec ([make-polys
	      (lambda (n)
		(unless (zero? n)
		  (regular-poly sides (* n 5))
		  (make-polys (sub1 n))))])
      (make-polys sides))))

(define radial-turtles
  (lambda (n)
    (cond
      [(zero? n) (void)]
      [else
       (begin (split (turn/radians (/ pi (expt 2 (sub1 n)))))
	      (radial-turtles (sub1 n)))])))

(define spaced-turtles
  (lambda (n)
    (cond
      [(zero? n) (void)]
      [else (begin (split (move (expt 2 (+ n 1))))
		   (spaced-turtles (sub1 n)))])))

(define spokes
  (lambda ()
    (begin
      (radial-turtles 4)
      (spaced-turtles 5)
      (turn/radians (/ pi 2))
      (draw 10))))

(define spyro-gyra
  (lambda () 
    (begin
      (radial-turtles 4)
      (regular-poly 3 100))))

(define neato
  (letrec ([spiral 
	    (lambda (d t)
	      (when (<= 1 d)
		(draw d)
		(turn/radians t)
		(spiral (- d 1) t)))])
    (lambda ()
      (begin
	(radial-turtles 4)
	(spiral 30 (/ pi 12))))))

(define graphics-bexam
  (letrec ([gb (lambda (d)
		 (if (<= d 1)
		     (draw d)
		     (let ([d (/ d 3)])
		       (begin
			 (gb d)
			 (turn/radians (- (/ pi 2)))
			 (gb d)
			 (turn/radians (/ pi 2))
			 (gb d)
			 (turn/radians (/ pi 2))
			 (gb d)
			 (turn/radians (- (/ pi 2)))
			 (gb d)))))])
    (lambda ()
      (let ([square-size (expt 3 5)])
	(begin
	  (split (turn/radians (/ pi 2))
		 (move square-size)
		 (turn/radians (- (/ pi 2)))
		 (move square-size)
		 (turn/radians pi))
	  (split (move square-size)
		 (turn/radians (/ pi 2)))
	  (gb square-size))))))

(define serp-size 300)

(define serp
  (let ([sqrt3 (sqrt 3)]
	[-2pi/3 (- (/ (* 2 pi) 3))]
	[pi/6 (/ pi 6)]
	[-5pi/6 (- (/ (* 5 pi) 6))]
	[pi/2 (/ pi 2)])
    (letrec ([engine
	      (lambda (distance)
		(if (< distance .6)
		    (void)
		    (let* ([side-half (* distance sqrt3)]
			   [side (* 2 side-half)])
		      
		      (begin
			(turn/radians -2pi/3)
			(move distance)
			(split (move distance)
			       (turn/radians -5pi/6)
			       (draw side)
			       (turn/radians -5pi/6)
			       (move distance)
			       (turn/radians pi)
			       (split (turn/radians -5pi/6)
				      (move side-half)
				      (turn/radians pi/6)))
			(engine (/ distance 2))))))])
      (lambda (distance)
	(begin
	  (move (* 2 distance))
	  (turn/radians (/ (* 5 pi) 6))
	  (draw (* distance 2 (sqrt 3)))
	  (turn/radians (/ (* 2 pi) 3))
	  (move (* distance 2 (sqrt 3)))
	  (turn/radians (/ (* 2 pi) 3))
	  (draw (* distance 2 (sqrt 3)))
	  (turn/radians (/ (* 2 pi) 3))
	  (turn/radians (/ pi 6))
	  (move (* 2 distance))
	  (turn/radians pi)
	  (engine distance))))))

(define serp-nosplit
  (let ([sqrt3 (sqrt 3)]
	[-2pi/3 (- (/ (* 2 pi) 3))]
	[pi/6 (/ pi 6)]
	[-5pi/6 (- (/ (* 5 pi) 6))]
	[pi/2 (/ pi 2)])
    (letrec ([engine
	      (lambda (distance)
		(if (< distance .6)
		    (void)
		    (let* ([side-half (* distance sqrt3)]
			   [side (* 2 side-half)])
		      (begin
			(turn/radians -2pi/3)
			(move distance)
			(engine (/ distance 2))
			(move distance)
			(turn/radians -5pi/6)
			(draw side)
			(turn/radians -5pi/6)
			(move distance)
			(turn/radians pi)
			(engine (/ distance 2))
			(turn/radians -5pi/6)
			(move side-half)
			(turn/radians pi/6)
			(engine (/ distance 2))
			(move (- distance))))))])
      (lambda (distance)
	(begin
	  (move (* 2 distance))
	  (turn/radians (/ (* 5 pi) 6))
	  (draw (* distance 2 (sqrt 3)))
	  (turn/radians (/ (* 2 pi) 3))
	  (move (* distance 2 (sqrt 3)))
	  (turn/radians (/ (* 2 pi) 3))
	  (draw (* distance 2 (sqrt 3)))
	  (turn/radians (/ (* 2 pi) 3))
	  (turn/radians (/ pi 6))
	  (move (* 2 distance))
	  (turn/radians pi)
	  (engine distance))))))

(define koch-size (expt 3 5))

(define koch-split
  (letrec ([build-up-turtles 
	    (lambda (n)
	      (cond
		[(<= n 3) 'built]
		[else (let ([third (/ n 3)])
			(begin
			  (split* 'stay-put
				  (move (* 2 third))
				  (begin (move third)
					 (turn/radians (- (/ pi 3))))
				  (begin (move third)
					 (turn/radians (- (/ pi 3)))
					 (move third)
					 (turn/radians (* 2 (/ pi 3)))))
			  (build-up-turtles third)))]))])
    (lambda (koch-size)
      (begin
	(split* 'stay-put
		(begin (move koch-size)
		       (turn/radians (/ (* 2 pi) 3)))
		(begin (turn/radians (/ pi 3))
		       (move koch-size)
		       (turn/radians pi)))
	(build-up-turtles koch-size)
	(draw 3)))))

(define koch-draw
  (letrec ([side
	    (lambda (n)
	      (cond
		[(<= n 3) (draw n)]
		[else (let ([third (/ n 3)])
			(begin
			  (side third)
			  (turn/radians (- (/ pi 3)))
			  (side third)
			  (turn/radians (* 2 (/ pi 3)))
			  (side third)
			  (turn/radians (- (/ pi 3)))
			  (side third)))]))])
    (lambda (koch-size)
      (begin
	(split* 'stay-put
		(begin (move koch-size)
		       (turn/radians (/ (* 2 pi) 3)))
		(begin (turn/radians (/ pi 3))
		       (move koch-size)
		       (turn/radians pi)))
	(side koch-size)))))

(define lorenz
  (lambda (a b c)
    (letrec ([loop 
	      (lambda (x y z)
		(let* ([delta 0.01]
		       [dx (* delta (* a (- y x)))]
		       [dy (* delta (- (* x b) y (* x z)))]
		       [dz (* delta (- (* x y) (* c z)))])
		  (begin
		    (draw-offset dx dz)
		    (sleep 0.05)
		    (erase-offset (- dx) (- dz))
		    (move-offset dx dz)
		    (loop (+ x dx)
			  (+ y dy)
			  (+ z dz)))))])
      (loop 1 1 1))))

(define lorenz1 (lambda () (lorenz 50 60 11)))

(define peano-size (expt 3 6))
(define peano-position-turtle
  (lambda ()
    (begin
      (clear)
      (move -300)
      (turn/radians (/ pi 2))
      (move 200)
      (turn/radians (- (/ (* 3 pi) 4))))))
     
(define peano
  (let* ([long 1/2]
	 [short (/ (- 1 long) 2 (sqrt 2))])
    (lambda (n)
      (let ([do-curve
	     (lambda (f)
	       (let ([leg
		      (lambda (d)
			(let ([t (d (/ pi 4))])
			  (begin
			    (f (* n long))
			    (turn/radians t)
			    (f (* n short))
			    (turn/radians t))))])
		 (begin (leg +)
			(leg -)
			(leg -)
			(leg -)
			(leg +)
			(leg +)
			(leg +)
			(leg -)
			
			(f (* n long)))))])
	(do-curve
	 (cond
	   [(< n 20) draw]
	   [else peano]))))))

(define peano1
  (lambda (l)
    (if (<= l 3)
	(draw l)
	(let ([l (/ l 3)])
	  (begin
	    (peano1 l)
	    (tprompt (peano1 l)
		     (split* (turn/radians (/ pi 2))
			     (turn/radians (- (/ pi 2))))
		     (peano1 l))
	    (tprompt (split* (turn/radians (/ pi 2))
			     (turn/radians (- (/ pi 2))))
		     (peano1 l))
	    (tprompt (split* (move l)
			     (begin (turn/radians (/ pi 2))
				    (move l)
				    (turn/radians (- (/ pi 2))))
			     (begin (turn/radians (- (/ pi 2)))
				    (move l)
				    (turn/radians (/ pi 2))))
		     (peano1 l))
	    (move (* 2 l)))))))

(define peano2
  (lambda (l)
    (if (<= l 3)
	(draw l)
	(let ([l (/ l 3)])
	  (begin
	    (peano2 l)
	    (turn/radians (/ pi 2))
	    (peano2 l)
	    (turn/radians (- (/ pi 2)))
	    (peano2 l)
	    (turn/radians (- (/ pi 2)))
	    (peano2 l)
	    (peano2 l)
	    (turn/radians (- (/ pi 2)))
	    (peano2 l)
	    (turn/radians (- (/ pi 2)))
	    (peano2 l)
	    (turn/radians (- (/ pi 2)))
	    (peano2 l)
	    (peano2 l))))))

(define fern-size 30)

(define fern1
  (lambda (n)
    (when (< 1 n)
      (draw (/ n 2))
      (tprompt (split* (turn/radians (/ pi 3))
		       (turn/radians (- (/ pi 3))))
	       (fern1 (/ n 2)))
      (draw (/ n 2))
      (turn/radians 0.08)
      (fern1 (- n 1)))))


;; need to backup a little for this one.
(define fern2
  (lambda (n)
    (let ([d 0.04])
      (letrec ([fernd 
		(lambda (n sign)
		  (when (< 1 n)
		    (draw (/ n 2))
		    (tprompt (turn/radians (/ pi 3))
			     (fernd (/ n 2) -))
		    (tprompt (turn/radians (- (/ pi 3)))
			     (fernd (/ n 2) +))
		    (draw (/ n 2))
		    (turn/radians (sign d))
		    (fernd (- n 1) sign)))])
	(fernd n +)))))
