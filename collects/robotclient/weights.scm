(module weights mzscheme

  (require (lib "list.ss"))
  (require "board.ss"
	   "client-parameters.ss"
           "heuristics.ss")
  (provide calc-weight
	   is-robot-within?
           update-robots
           (struct search-player (x y id money capacity packages)))
;(define wall-threat-value (make-parameter 1))
;(define water-threat-value (make-parameter 1))
;(define blank-threat-value (make-parameter 1))
;(define wall-danger-value (make-parameter 1))
;(define water-danger-value (make-parameter 1))
;(define blank-danger-value (make-parameter 1))
;(define wall-escape-value (make-parameter 1))
;(define water-escape-value (make-parameter 1))
;(define blank-escape-value (make-parameter 1))
;(define wall-push-value (make-parameter 1))
;(define water-push-value (make-parameter 1))
;(define blank-push-value (make-parameter 1))
;(define blank-value (make-parameter 1))
;(define wall-value (make-parameter 1))
;(define water-value (make-parameter 1))
;(define home-value (make-parameter 1))
;(define next-water-value (make-parameter 1))
;(define one-home-value (make-parameter 1))
;(define two-home-value (make-parameter 1))
;(define three-home-value (make-parameter 1))
;(define destination-value (make-parameter 1))
;(define one-destination-value (make-parameter 1))
;(define two-destination-value (make-parameter 1))
;(define three-destination-value (make-parameter 1))
;(define dvw-value (make-parameter 1))
;(define pickup-value (make-parameter 1))
;(define dropoff-value (make-parameter 1))
;(define wall-escape-bid (make-parameter 1))
;(define water-escape-bid (make-parameter 1))
;(define blank-escape-bid (make-parameter 1))
;(define wall-push-bid (make-parameter 1))
;(define water-push-bid (make-parameter 1))
;(define blank-push-bid (make-parameter 1))
;(define max-bid-const (make-parameter 10))
(define max-bid (make-parameter 0))
(define player-cur (make-parameter #f))
  

  (define-struct search-player (x y id money capacity packages))
  
	(define (update-robots robot-list p)
          (player-cur p)
          (let ([around-val (if (> (home-squares) (dest-squares))
                                (home-squares)
                                (dest-squares))])
            (do 
                ([y (- (search-player-y (player-cur)) around-val) (+ y 1)]
                 [counter-y 0 (+ counter-y 1)])
	      ((< counter-y (+ 1 (* 2 around-val))) (void 2))
              (do ([x (- (search-player-x (player-cur)) around-val) (+ x 1)]
                   [counter-x 0 (+ counter-x 1)])
                ((< counter-x (+ 1 (* 2 around-val))) (void 2))
                (set-invalid (get-spot board x y))))))
	;; 

	
	(define (calc-weight mtype x y p list-of-pack)
          (begin
            (if (= (max-bid) 0)
                (max-bid (* (max-bid-const) (/ (player-initial-money) (* (board-height) (board-width))))))
            (player-cur p)
	  (cond
	   [(eq? mtype 'm)
	    (let*
		([spot (get-spot (board) x y)]
		 [no-packages (if (null? (search-player-packages (player-cur)))
				    1
				    0)]
		 [weight (if (= 1 (get-valid spot))
			     (get-weight spot)
			     (let ([new-weight
				    (+ (if (could-player-move? (board) x y (player-cur))
                                           (+ (* (wall-danger-value) (wall-danger? (board) x y))
                                              (* (water-danger-value) (water-danger? (board) x y))
                                              (* (blank-danger-value) (blank-danger? (board) x y))
                                              (* (wall-threat-value) (wall-threat? (board) x y))
                                              (* (water-threat-value) (water-threat? (board) x y))
                                              (* (blank-threat-value) (blank-threat? (board) x y))
                                              (* (water-escape-value) (water-escape? (board) x y (player-cur)))
                                              (* (blank-escape-value) (blank-escape? (board) x y (player-cur)))
                                              (* (wall-escape-value) (wall-escape? (board) x y p))
                                              (* (water-push-value) (water-push? (board) x y p))
                                              (* (blank-push-value) (blank-push? (board) x y p))
                                              (* (wall-push-value) (wall-push? (board) x y p )))
				    
                                           0)
                                       (if (blank? (board) x y)
                                           (blank-value)
                                           0)
                                       (* (next-water-value) (next-to-water? (board) x y))
                                       (let ([dist (+ (abs (- x (car (path-loc)))) (abs (- y (cdr (path-loc)))))])
                                         (if (< dist (path-squares))
                                             (if (path-geometric)
                                                 (* (path-value) (expt (path-falloff) dist))
                                                 (+ (path-value) (let ([arith (* (- (path-falloff)) dist)])
                                                                   (if (> (abs arith) (path-value))
                                                                       (- (path-value))
                                                                       arith))))
                                             0))
                                           
                                       (if (null? (search-player-packages (player-cur)))
					          0
                                                  (let loop ([dist 0]
                                                             [counter 0])
                                                    (cond [(<= (dest-squares) dist) counter]
                                                          [else (let ([toadd (if (package-away-goal? dist x y (search-player-packages (player-cur)))
										 (if (dest-geometric)
										     (* (destination-value) (expt (dest-falloff) dist))
										     (+ (destination-value) (let ([arith (* (- (dest-falloff)) dist)])
													      (if (> (abs arith) (destination-value))
														  (- (destination-value))
														  arith))))
                                                                                0)])
                                                                  (loop (+ 1 dist) (+ counter toadd)))])))
                                       
                                       (if (null? (search-player-packages (player-cur)))
                                           (let loop ([dist 0]
                                                      [counter 0])
                                             (cond [(<= (home-squares) dist) counter]
                                                   [else (let ([toadd (if (home-away-goal? dist x y (home-list))
									  (if (home-geometric)
									      (* (home-value) (expt (home-falloff) dist))
									      (+ (home-value) (let ([arith (* (- (home-falloff)) dist)])
												(if (> (abs arith) (home-value))
												    (- (home-value))
												    arith))))
                                                                          0)])
                                                           (loop (+ 1 dist) (+ counter toadd)))]))
                                           0))])
			       (begin
				 (set-weight (inexact->exact (round new-weight)) (get-spot (board) x y))
				 (set-valid (get-spot (board) x y))
				 (inexact->exact (round new-weight)))))]
		 [bid (if (could-player-move? (board) x y p)
                          (begin
                           ; (printf "Possible player move, max-bid:~a~n" (max-bid))

			  (figure-bid (board) x y p))
			  1)])
		(values weight (if (= (round bid) 0)
				   1
                                   (inexact->exact (round bid))) null null))]
	   [(eq? mtype 'd)
	    (let* ([ptod (get-packages-for x y (search-player-packages (player-cur)))]
		   [weight (+ (if (could-player-move? (board) x y p)
				 (+ (* (wall-danger-value) (wall-danger? (board) x y))
				    (* (water-danger-value) (water-danger? (board) x y))
				    (* (blank-danger-value) (blank-danger? (board) x y))
				    (* (wall-threat-value) (wall-threat? (board) x y))
				    (* (water-threat-value) (water-threat? (board) x y))
				    (* (blank-threat-value) (blank-threat? (board) x y)))
				 0)
			     (* (dropoff-value) (if (null? ptod)
						    0
						    (apply + (map package-weight ptod)))))]
		   [bid (if (or (= (wall-danger? (board) x y) 1) (= (water-danger? (board) x y) 1) (= (blank-danger? (board) x y) 1))
			    (* (water-escape-bid) (max-bid))
			    1)])
	      (values weight (if (= (round bid) 0)
                                 1
                                 (inexact->exact (round bid))) ptod null))]
	   [(eq? mtype 'p)
	    (let* ([spack 
                    (begin
;                      (printf "list-of-pack:~a~n" list-of-pack)
                      (quicksort list-of-pack (lambda (p1 p2) (< (pack-val p1 x y) (pack-val p2 x y)))))]
                   [ptop
                    (begin
;                      (printf "spack:~a~n" spack)
                    (most-of (wleft p) spack))]
		   [weight (begin ;(printf "ptop:~a~n" ptop)
                             (* (pickup-value)
			      (if (null? ptop)
				  0
				  (apply * (map pack-val ptop (repeat x (length ptop)) (repeat y (length ptop)) )))))])
		   (values weight 1 null ptop))]
	   [else
	    (error "not a recognized symbol")])))

(define (repeat elem num)
  (if (<= num 0)
      null
      (cons elem (repeat elem (- num 1)))))
		
	(define (wleft p)
          (- (search-player-capacity (player-cur)) (eval `(+ ,@(map package-weight (search-player-packages (player-cur)))))))
	;; 
	(define-syntax figure-bid
	  (syntax-rules ()
			((_ board x y p)
			 (cond
			  [(= (water-escape? board x y p) 1) (begin (* (water-escape-bid) (max-bid)))]
			  [(= (water-push? board x y p) 1) (begin (* (water-push-bid) (max-bid)))]
			  [(= (blank-escape? board x y p) 1) (begin   (* (blank-escape-bid) (max-bid)))]
			  [(= (wall-escape? board x y p) 1) (begin  (* (wall-escape-bid) (max-bid)))]
			  [(= (blank-push? board x y p) 1) (begin  (* (blank-push-bid) (max-bid)))]
			  [(= (wall-push? board x y p) 1) (begin  (* (wall-push-bid) (max-bid)))]
                          [else 1]))))
	
	(define (most-of player-left lop)
	  (if (null? lop)
	      null
	      (if (<= player-left 0)
		  null
		  (if (< (package-weight (car lop)) player-left)
		      (cons (car lop) (most-of (- player-left (package-weight (car lop))) (cdr lop)))
                      (most-of player-left (cdr lop))))))
	
	   (define (pack-val package x y)
	     (* (dvw-value) (+ (abs (- x (package-x package))) (abs (- y (package-y package))))))
	   
	(define-syntax get-packages-for 
	  (syntax-rules ()
			((_ x y plist)
			 (filter (lambda (package) (and (= x (package-x package))
							(= y (package-y package))))
				 plist))))

	(define-syntax destination?
	  (syntax-rules ()
			((_ x y plist)
			 (if (not (null? (get-packages-for x y plist)))
                             1
                             0))))


  (define-syntax x-away-goal?
    (syntax-rules ()
      ((_ n x y gx gy)
       (and (= n (+ (abs (- x gx)) (abs (- y gy))))
            (no-barriers? x y gx gy)))))
  
  (define-syntax no-barriers?
    (syntax-rules ()
      ((_ x y gx gy)
       (and (clear-path-horiz? x y gx gy) (clear-path-vert? x y gx gy)))))
  
  (define (create-path-horiz x y gx gy)
    (cond
      [(= x gx) (cond
                  [(= y gy) null]
                  [(> y gy) (cons (cons x (sub1 y)) (create-path-horiz x (sub1 y) gx gy))]
                  [(< y gy) (cons (cons x (add1 y)) (create-path-horiz x (add1 y) gx gy))])]
      [(> x gx) (cons (cons (sub1 x) y) (create-path-horiz (sub1 x) y gx gy))]
      [(< x gx) (cons (cons (add1 x) y) (create-path-horiz (add1 x) y gx gy))]))

  (define (clear-path-horiz? x y gx gy)
    (if (or (wall? (board) x y)
            (water? (board) x y))
        #f
        (cond
          [(= x gx) (cond
                      [(= y gy) #t]
                      [(> y gy) (clear-path-horiz? x (sub1 y) gx gy)]
                      [(< y gy) (clear-path-horiz? x (add1 y) gx gy)])]
          [(> x gx) (clear-path-horiz? (sub1 x) y gx gy)]
          [(< x gx) (clear-path-horiz? (add1 x) y gx gy)])))
  
  (define (clear-path-vert? x y gx gy)
    (if (or (wall? (board) x y)
            (water? (board) x y))
        #f
        (cond
          [(= y gy) (cond
                      [(= x gx) #t]
                      [(> x gx) (clear-path-vert? (sub1 x) y gx gy)]
                      [(< x gx) (clear-path-vert? (add1 x) y gx gy)])]
          [(> y gy) (clear-path-vert? x (sub1 y) gx gy)]
          [(< y gy) (clear-path-vert? x (add1 y) gx gy)])))


  (define-syntax package-away-goal?
    (syntax-rules ()
      ((_ n x y plist)
       (not (null? (filter (lambda (p) (x-away-goal? n x y (package-x p) (package-y p))) plist))))))
  
  (define-syntax home-away-goal?
    (syntax-rules ()
      ((_ n x y hlist)
       (let ([res (not (null? (filter (lambda (h) (x-away-goal? n x y (car h) (cdr h))) hlist)))])
         ;       (when res (printf "res: ~a~n" res))
         res))))
  
	(define-syntax is-robot?
	  (syntax-rules ()
			((_ board x y)
                         
			 (and (= 1 (get-robot (get-spot board x y)))
                              (not (and (= (search-player-x (player-cur)) x) (= (search-player-y (player-cur)) y))) ))))

  (define (is-robot-within? x y n)
    (let ([max-y (+ y n)]
          [max-x (+ x n)])
      (let loop ([cur-y (- y n)])
        (if (> cur-y max-y)
            #f
            (let loopx ([cur-x (- x n)])
              (if (> cur-x max-x)
                  (loop (+ 1 cur-y))
                  (if (and (= 1 (get-robot (get-spot (board) x y)))
                           (not (and (= x cur-x) (= y cur-y))))
                      (begin (printf "Robot is within!~n") #t)
                      (loopx (+ 1 cur-x)))))))))
  
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
			((_ board x y )
			 (or (is-robot? board x y )
			     (is-robot? board (- x 1) y )
			     (is-robot? board (+ x 1) y )
			     (is-robot? board x (- y 1) )
			     (is-robot? board x (+ y 1) )))))
			 
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
			 (or (if
                              (and (is-robot? board (- x 1) y )
				  (= t (get-type (get-spot board (+ x 1) y))))
                              (begin
                                ;(printf "pinned (- x 1) y~n")
                                #t)
                              #f)
                             (if
			     (and (is-robot? board (+ x 1) y )
				  (= t (get-type (get-spot board (- x 1) y))))
                             (begin
                                ;(printf "pinned (+ x 1) y~n")
                                #t)
                              #f)
			     (if
                              (and (is-robot? board x (- y 1) )
				  (= t (get-type (get-spot board x (+ y 1)))))
                              (begin
                                ;(printf "pinned (- y 1) y~n")
                                #t)
                              #f)
			     (if
                              (and (is-robot? board x (+ y 1) )
				  (= t (get-type (get-spot board x (- y 1)))))
                              (begin
                                ;(printf "pinned (+ y 1) y~n")
                                #t)
                              #f)))))

	(define-syntax wall-danger?
	  (syntax-rules ()
			((_ board x y)
			 (if (pinned? board x y 2 )
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
			 (if (and 
                              (pinned? board (search-player-x (player-cur)) (search-player-y (player-cur)) 2)
				  (or (not (is-robot? board x y )) (not (wall? board (+ x (- x (search-player-x (player-cur)))) (+ y (- y (search-player-y (player-cur))))))))
			     1
			     0))))

	(define-syntax blank-escape?
	  (syntax-rules ()
			((_ board x y p)
			 (if (and (or 
				   (pinned? board (search-player-x (player-cur)) (search-player-y (player-cur)) 0)
				   (pinned? board (search-player-x (player-cur)) (search-player-y (player-cur)) 3))
				  (or (not (is-robot? board x y )) (not (wall? board (+ x (- x (search-player-x (player-cur)))) (+ y (- y (search-player-y (player-cur))))))))
			     1
			     0))))

	(define-syntax water-escape?
	  (syntax-rules ()
			((_ board x y p)

                           
			 (if (and 
                              (if (pinned? board (search-player-x (player-cur)) (search-player-y (player-cur)) 1)
                                  (begin
                                    ;(printf "Pinned on a water escape~n")
                                    #t)
                                  #f)
				  (or (not (is-robot? board x y )) (not (wall? board (+ x (- x (search-player-x (player-cur)))) (+ y (- y (search-player-y (player-cur))))))))
			     1
			     0))))


	(define-syntax wall-push?
	  (syntax-rules ()
			((_ board x y p)
			 (if (and (is-robot? board x y )
				  (wall? board (+ x (- x (search-player-x (player-cur)))) (+ y (- y (search-player-y (player-cur))))))
			     1
			     0))))

	(define-syntax blank-push?
	  (syntax-rules ()
			((_ board x y p)
			 (if (and (is-robot? board x y )
				  (blank? board (+ x (- x (search-player-x (player-cur)))) (+ y (- y (search-player-y (player-cur))))))
			     1
			     0))))

	(define-syntax water-push?
	  (syntax-rules ()
			((_ board x y p)
			 (if (and (is-robot? board x y )
				  (water? board (+ x (- x (search-player-x (player-cur)))) (+ y (- y (search-player-y (player-cur))))))
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
			 (if (= x (search-player-x (player-cur)))
			     (<= (abs (- (search-player-y (player-cur)) y)) 1)
			     (if (= y (search-player-y (player-cur)))
				 (<= (abs (- (search-player-x (player-cur)) x)) 1))))))
        
        )