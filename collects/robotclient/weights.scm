(module weights mzscheme

  (require (lib "list.ss"))

(define wall-threat-value (make-parameter 0))
(define water-threat-value (make-parameter 0))
(define blank-threat-value (make-parameter 0))
(define wall-danger-value (make-parameter 0))
(define water-danger-value (make-parameter 0))
(define blank-danger-value (make-parameter 0))
(define wall-escape-value (make-parameter 0))
(define water-escape-value (make-parameter 0))
(define blank-escape-value (make-parameter 0))
(define wall-push-value (make-parameter 0))
(define water-push-value (make-parameter 0))
(define blank-push-value (make-parameter 0))
(define blank-value (make-parameter 0))
(define wall-value (make-parameter 0))
(define water-value (make-parameter 0))
(define home-value (make-parameter 0))
(define next-water-value (make-parameter 0))
(define one-home-value (make-parameter 0))
(define two-home-value (make-parameter 0))
(define three-home-value (make-parameter 0))
(define destination-value (make-parameter 0))
(define one-destination-value (make-parameter 0))
(define two-destination-value (make-parameter 0))
(define three-destination-value (make-parameter 0))
(define dvw-value (make-parameter 0))
(define pickup-value (make-parameter 0))
(define dropoff-value (make-parameter 0))
(define wall-escape-bid (make-parameter 0))
(define water-escape-bid (make-parameter 0))
(define blank-escape-bid (make-parameter 0))
(define wall-push-bid (make-parameter 0))
(define water-push-bid (make-parameter 0))
(define blank-push-bid (make-parameter 0))
	
	(require "board.ss")
	(provide calc-weight
		 update-robots)
  (define myboard #f)
  (define-struct search-player (x y id money capacity packages))
  
	(define (update-robots robot-list p)
	  (do 
	      ([y (- (search-player-y p) 2) (+ y 1)]
	       [counter-y 0 (+ counter-y 1)])
	      ((< counter-y 5) (void 2))
	    (do ([x (- (search-player-x p) 2) (+ x 1)]
                  [counter-x 0 (+ counter-x 1)])
               ((< counter-x 5) (void 2))
	      (set-valid (get-spot board x y)))))
	;; 

	
	(define (calc-weight mtype x y p list-of-pack)
	  (cond
	   [(eq? mtype 'm)
	    (let*
		([spot (get-spot myboard x y)]
		 [weight (if (= 1 (get-valid spot))
			     (get-weight spot)
			     (let ([new-weight
				    (+ (if (could-player-move? myboard x y p)
				    (+ (* (wall-danger-value) (wall-danger? myboard x y))
				       (* (water-danger-value) (water-danger? myboard x y))
				       (* (blank-danger-value) (blank-danger? myboard x y))
				       (* (wall-threat-value) (wall-threat? myboard x y))
				       (* (water-threat-value) (water-threat? myboard x y))
				       (* (blank-threat-value) (blank-threat? myboard x y))
				       (* (water-escape-value) (water-escape? myboard x y p))
				       (* (blank-escape-value) (blank-escape? myboard x y p))
				       (* (wall-escape-value) (wall-escape? myboard x y p))
				       (* (water-push-value) (water-push? myboard x y p))
				       (* (blank-push-value) (blank-push? myboard x y p))
				       (* (wall-push-value) (wall-push? myboard x y p )))
				    
				    0)
				(if (blank? myboard x y)
				    (blank-value)
				    (if (wall? myboard x y)
					(wall-value)
					(if (water? myboard x y)
					    (water-value)
					    (if (home? myboard x y)
						(home-value)))))
				(* (next-water-value) (next-to-water? myboard x y))
				(* (destination-value) (destination? myboard x y))
				(* (one-destination-value) (one-away-destination? myboard x y))
				(* (two-destination-value) (two-away-destination? myboard x y))
				(* (three-destination-value) (three-away-destination? myboard x y))
				(* (one-home-value) (one-away-base? myboard x y))
				(* (two-home-value) (two-away-base? myboard x y))
				(* (three-home-value) (three-away-base? myboard x y)))])
			       (begin
				 (set-weight new-weight (get-spot myboard x y))
				 (set-valid 1 (get-spot myboard x y))
				 new-weight)))]
		 [bid (if (could-player-move? myboard x y)
			  (figure-bid myboard x y p)
			  1)])
		(list weight bid null null))]
	   [(symbol=? mtype 'd)
	    (let* ([ptod (get-packages-for x y (search-player-packages))]
		   [weight (+ (if (could-player-move? myboard x y p)
				 (+ (* (wall-danger-value) (wall-danger? myboard x y))
				    (* (water-danger-value) (water-danger? myboard x y))
				    (* (blank-danger-value) (blank-danger? myboard x y))
				    (* (wall-threat-value) (wall-threat? myboard x y))
				    (* (water-threat-value) (water-threat? myboard x y))
				    (* (blank-threat-value) (blank-threat? myboard x y)))
				 0)
			     (* (dropoff-value) (if (null? ptod)
						    0
						    (eval `(+ ,@(map packages-weight ptod))))))]
		   [bid (if (or (= (wall-danger? myboard x y) 1) (= (water-danger? myboard x y) 1) (= (blank-danger? myboard x y) 1))
			    bid-max
			    1)])
	      (list weight bid ptod null))]
	   [(symbol=? mtype 'p)
	    (let* ([ptop (most-of (wleft p) (quicksort list-of-pack (lambda (p1 p2) (< (pack-val p1) (pack-val p2)))))]
		   [weight (* (should-pick-up)
			      (if (null? ptop)
				  0
				  (eval `(* ,@(map pack-val ptop)))))])
		   (list weight 1 null ptop))]
	   [else
	    (error "not a recognized symbol")]))

	(define-syntax wleft
	  (syntax-rules ()
			((_ p)
			 (eval `(- (search-player-capacity p) (+ ,@(map packages-weight (search-player-packages p))))))))
	;; 
	(define-syntax figure-bid
	  (syntax-rules ()
			((_ board x y p)
			 (cond
			  [(water-escape? board x y) (water-escape-bid)]
			  [(water-push? board x y) (water-push-bid)]
			  [(empty-escape? board x y) (empty-escape-bid)]
			  [(wall-escape? board x y) (wall-escape-bid)]
			  [(empty-push? board x y) (empty-bush-bid)]
			  [(wall-push? board x y) (wall-push-bid)]))))
	
	(define (most-of player-left lop)
	  (if (null? lop)
	      null
	      (if (<= player-left 0)
		  null
		  (if (< (packages-weight (car lop)) player-left)
		      (cons (car lop) (most-of (- player-left (packages-weight (car lop))) (cdr lop)))))))
	
	   (define (pack-val package)
	     (* (dvw-value) (+ (abs (- x (packages-x package))) (abs (- y (packages-y package))))))
	   
	(define-syntax get-packages-for 
	  (syntax-rules ()
			((_ x y plist)
			 (filter (lambda (package) (and (= x (package-x package))
							(= y (package-y package))))
				 plist))))

	(define-syntax destination?
	  (syntax-rules ()
			((_ x y plist)
			 (not (null? (get-packages-for x y plist))))))


	(define-syntax one-away-destination?
	  (syntax-rules ()
			((_ board x y)
			 (if (or (not (null? (get-packages-for (+ x 1) y)))
				 (not (null? (get-packages-for (- x 1) y)))
				 (not (null? (get-packages-for x (+ y 1))))
				 (not (null? (get-packages-for x (- y 1)))))
			     1
			     0))))

	(define-syntax two-away-destination?
	  (syntax-rules ()
			((_ board x y)
			 (if (or (not (null? (get-packages-for (+ x 2) y)))
				 (not (null? (get-packages-for (- x 2) y)))
				 (not (null? (get-packages-for x (+ y 2))))
				 (not (null? (get-packages-for x (- y 2))))
				 (not (null? (get-packages-for (+ x 1) (+ y 1))))
				 (not (null? (get-packages-for (+ x 1) (- y 1))))
				 (not (null? (get-packages-for (- x 1) (- y 1))))
				 (not (null? (get-packages-for (- x 1) (+ y 1)))))
			     1
			     0))))


	(define-syntax three-away-destination?
	  (syntax-rules ()
			((_ board x y)
			 (if (or (not (null? (get-packages-for (+ x 3) y)))
				 (not (null? (get-packages-for (- x 3) y)))
				 (not (null? (get-packages-for x (+ y 3))))
				 (not (null? (get-packages-for x (- y 3))))
				 (not (null? (get-packages-for (+ x 2) (+ y 1))))
				 (not (null? (get-packages-for (+ x 2) (- y 1))))
				 (not (null? (get-packages-for (- x 2) (+ y 1))))
				 (not (null? (get-packages-for (- x 2) (- y 1))))
				 (not (null? (get-packages-for (+ x 1) (+ y 2))))
				 (not (null? (get-packages-for (- x 1) (+ y 2))))
				 (not (null? (get-packages-for (+ x 1) (- y 2))))
				 (not (null? (get-packages-for (- x 1) (- y 2)))))
			     1
			     0))))

	(define-syntax is-robot?
	  (syntax-rules ()
			((_ board x y)
			 (= 1 (get-robot (get-spot board x y))))))

	(define-syntax wall?
	  (syntax-rules ()
			((_ board x y)
			 (= 2 (get-type (get-spot board x y))))))

	(define-syntax water?
	  (syntax-rules ()
			((_ board x y)
			 (= 1 (get-type (get-spot board x y))))))

	(define-syntax home?
	  (syntax-rules ()
			((_ board x y)
			 (= 3 (get-type (get-spot board x y))))))

	(define-syntax blank?
	  (syntax-rules ()
			((_ board x y)
			 (= 0 (get-type (get-spot board x y))))))

	(define-syntax robot-threat?
	  (syntax-rules ()
			((_ board x y)
			 (or (is-robot? board x y)
			     (is-robot? board (- x 1) y)
			     (is-robot? board (+ x 1) y)
			     (is-robot? board x (- y 1))
			     (is-robot? board x (+ y 1))))))
			 
	(define-syntax wall-threat?
	  (syntax-rules ()
			((_ board x y)
			 (if (or
			      (and (robot-threat? board (- x 1) y)
				   (wall? board (+ x 1) y))
			      (and (robot-threat? board (+ x 1) y)
				   (wall? board (- x 1) y))
			      (and (robot-threat? board x (- y 1))
				   (wall? board x (+ y 1)))
			      (and (robot-threat? board x (+ y 1))
				   (wall? board x (- y 1))))
			     1
			     0))))

	(define-syntax water-threat?
	  (syntax-rules ()
			((_ board x y)
			 (if (or
			      (and (robot-threat? board (- x 1) y)
				   (water? board (+ x 1) y))
			      (and (robot-threat? board (+ x 1) y)
				   (water? board (- x 1) y))
			      (and (robot-threat? board x (- y 1))
				   (water? board x (+ y 1)))
			      (and (robot-threat? board x (+ y 1))
				   (water? board x (- y 1))))
			     1
			     0))))

	(define-syntax blank-threat?
	  (syntax-rules ()
			((_ board x y)
			 (if (or
			      (and (robot-threat? board (- x 1) y)
				   (or (blank? board (+ x 1) y) (home? board (+ x 1) y)))
			      (and (robot-threat? board (+ x 1) y)
				   (or (blank? board (- x 1) y) (home? board (- x 1) y)))
			      (and (robot-threat? board x (- y 1))
				   (or (blank? board x (+ y 1)) (home? board x (+ y 1))))
			      (and (robot-threat? board x (+ y 1))
				   (or (blank? board x (- y 1)) (home? board x (- y 1)))))
			     1
			     0))))

	(define-syntax pinned?
	  (syntax-rules ()
			((_ board x y t)
			 (or (and (is-robot? board (- x 1) y)
				  (= t (get-type (get-spot board (+ x 1) y))))
			     (and (is-robot? board (+ x 1) y)
				  (= t (get-type (get-spot board (- x 1) y))))
			     (and (is-robot? board x (- y 1))
				  (= t (get-type (get-spot board x (+ y 1)))))
			     (and (is-robot? board x (+ y 1))
				  (= t (get-type (get-spot board x (- y 1)))))))))

	(define-syntax wall-danger?
	  (syntax-rules ()
			((_ board x y)
			 (if (pinned? board x y 2)
			     1
			     0))))

	(define-syntax water-danger?
	  (syntax-rules ()
			((_ board x y)
			 (if (pinned? board x y 1)
			     1
			     0))))

	(define-syntax blank-danger?
	  (syntax-rules ()
			((_ board x y)
			 (if (or (pinned? board x y 0)
				 (pinned? board x y 3))
			     1
			     0))))

	(define-syntax wall-escape?
	  (syntax-rules ()
			((_ board x y p)
			 (if (and (pinned? board (search-player-x p) (search-player-y p) 2)
				  (or (not (is-robot? board x y)) (not (wall? board (+ x (- x (search-player-x p))) (+ y (- y (search-player-y p)))))))
			     1
			     0))))

	(define-syntax blank-escape?
	  (syntax-rules ()
			((_ board x y p)
			 (if (and (or 
				   (pinned? board (search-player-x p) (search-player-y p) 0)
				   (pinned? board (search-player-x p) (search-player-y p) 3))
				  (or (not (is-robot? board x y)) (not (wall? board (+ x (- x (search-player-x p))) (+ y (- y (search-player-y p)))))))
			     1
			     0))))

	(define-syntax water-escape?
	  (syntax-rules ()
			((_ board x y p)
			 (if (and (pinned? board (search-player-x p) (search-player-y p) 1)
				  (or (not (is-robot? board x y)) (not (wall? board (+ x (- x (search-player-x p))) (+ y (- y (search-player-y p)))))))
			     1
			     0))))


	(define-syntax wall-push?
	  (syntax-rules ()
			((_ board x y p)
			 (if (and (is-robot? board x y)
				  (wall? board (+ x (- x (search-player-x p))) (+ y (- y (search-player-y p)))))
			     1
			     0))))

	(define-syntax blank-push?
	  (syntax-rules ()
			((_ board x y p)
			 (if (and (is-robot? board x y)
				  (blank? board (+ x (- x (search-player-x p))) (+ y (- y (search-player-y p)))))
			     1
			     0))))

	(define-syntax water-push?
	  (syntax-rules ()
			((_ board x y p)
			 (if (and (is-robot? board x y)
				  (water? board (+ x (- x (search-player-x p))) (+ y (- y (search-player-y p)))))
			     1
			     0))))
	
	(define-syntax next-to-water?
	  (syntax-rules ()
			((_ board x y)
			 (if (or (water? board (+ x 1) y)
				 (water? board (- x 1) y)
				 (water? board x (+ y 1))
				 (water? board x (- y 1)))
			     1
			     0))))

	(define-syntax one-away-base?
	  (syntax-rules ()
			((_ board x y)
			 (if (or (home? board (+ x 1) y)
				 (home? board (- x 1) y)
				 (home? board x (+ y 1))
				 (home? board x (- y 1)))
			     1
			     0))))

	(define-syntax two-away-base?
	  (syntax-rules ()
			((_ board x y)
			 (if (or (home? board (+ x 2) y)
				 (home? board (- x 2) y)
				 (home? board x (+ y 2))
				 (home? board x (- y 2))
				 (home? board (+ x 1) (+ y 1))
				 (home? board (+ x 1) (- y 1))
				 (home? board (- x 1) (- y 1))
				 (home? board (- x 1) (+ y 1)))
			     1
			     0))))


	(define-syntax three-away-base?
	  (syntax-rules ()
			((_ board x y)
			 (if (or (home? board (+ x 3) y)
				 (home? board (- x 3) y)
				 (home? board x (+ y 3))
				 (home? board x (- y 3))
				 (home? board (+ x 2) (+ y 1))
				 (home? board (+ x 2) (- y 1))
				 (home? board (- x 2) (+ y 1))
				 (home? board (- x 2) (- y 1))
				 (home? board (+ x 1) (+ y 2))
				 (home? board (- x 1) (+ y 2))
				 (home? board (+ x 1) (- y 2))
				 (home? board (- x 1) (- y 2)))
			     1
			     0))))
	  
	(define-syntax could-player-move?
	  (syntax-rules ()
			((_ board x y p)
			 (if (= x (search-player-x p))
			     (<= (abs (- (search-player-y p) y) 1))
			     (if (= y (search-player-y p))
				 (<= (abs (- (search-player-x p) x) 1)))))))
        
        )