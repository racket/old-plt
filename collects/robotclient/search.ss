(module search mzscheme
  
  (require (lib "list.ss"))
  
  (require "board.ss"
           "client-parameters.ss"
           "heuristics.ss"
           "weights.scm")
  
  (provide compute-move best-cmd)
  
  (define (weight-from-goal x) (* (step-weight) x))
  
  ;(make-qelt qelt move qelt)
  (define-struct qelt (left move right))
  
  ;(make-move num num num cord num command)
  (define-struct move (weight x y back step orig))
  ;(make-cord num num)
  (define-struct cord (x y) (make-inspector))
  
  (define current-player (make-parameter null))
  
  (define best-cmd (make-parameter null))
  (define best-weight (make-parameter -inf.0))
  (define last-move (make-parameter null))
  
  (define queue-head (make-parameter null))
  (define in-queue (make-parameter 0))
  
  (define (enqueue move)
    (cond
      ((void? move) (void))
      ((null? (queue-head)) 
       (best-cmd (move-orig move))
       (best-weight (move-weight move))

       (queue-head (make-qelt null move null))
       (in-queue 1))
      (else
       (let loop ((current (queue-head)))
         (if (> (move-weight move) (move-weight (qelt-move current)))
             (let ((q (make-qelt (qelt-left current) move current)))
               (cond 
                 ((and (= (in-queue) 6) (eq? current (queue-head)))
                  (when (> (move-weight move) (best-weight))
                    (best-cmd (move-orig move))
                    (best-weight (move-weight move)))
                  (queue-head q)
                  (set-qelt-left! current q)
                  (remove-queue-tail))
                 ((= (in-queue) 6)
                  (set-qelt-left! current q)
                  (set-qelt-right! (qelt-left q) q)
                  (remove-queue-tail))
                 ((eq? current (queue-head))
                  (when (> (move-weight move) (best-weight))
                    (best-cmd (move-orig move))
                    (best-weight (move-weight move)))
                  (queue-head q)
                  (set-qelt-left! current q)
                  (in-queue (add1 (in-queue))))
                 (else
                  (set-qelt-left! current q)
                  (set-qelt-right! (qelt-left q) q)
                  (in-queue (add1 (in-queue))))))
             (if (null? (qelt-right current))
                 (when (< (in-queue) 6)
                   (let ((q (make-qelt current move null)))
                     (set-qelt-right! current q)
                     (in-queue (add1 (in-queue)))))
                 (loop (qelt-right current))))))))
  
  (define (dequeue)
    (let ((current (queue-head)))
      (queue-head (qelt-right current))
      (unless (null? (queue-head)) (set-qelt-left! (queue-head) null))
      (in-queue (sub1 (in-queue)))
      (qelt-move current)))
  
  (define (remove-queue-tail)
    (let loop ((current (queue-head)))
      (if (null? (qelt-right current))
          (set-qelt-right! (qelt-left current) null)
          (loop (qelt-right current)))))
  
  (define (get-move-weight x y)
    (let-values (((weight bid _ __) (calc-weight 'M x y (current-player) null)))
      (values weight bid)))
  
  (define (move-maker weighted-g back g command?)
    (lambda (weight x y bid . special)
      (make-move (+ weighted-g weight) x y back g (if command? command? (make-command bid (car special) (cadr special))))))

  (define (coord-alright? x y back-coord-list)
    (let ([type (get-type (get-spot (board) x y))])
      (and (not (or (= type 1) (= type 2)))
	   (not (member (make-cord x y) back-coord-list)))))

  (define (generate-moves first? x y weight back-check g command packages)
    (let ([maker (move-maker (weight-from-goal g) (cons (make-cord x y) back-check) g command)])
      (let-values ([(pick-weight p-bid _ pick) (calc-weight 'P x y (current-player) packages)]
		   [(drop-weight d-bid drop __) (if (not (null? (search-player-packages (current-player))))
						    (calc-weight 'D x y (current-player) null)
						    (values #f void null void))]
		   [(west-weight w-bid) (if (coord-alright? (sub1 x) y back-check) (get-move-weight (sub1 x) y) (values #f #f))]
		   [(east-weight e-bid) (if (coord-alright? (add1 x) y back-check) (get-move-weight (add1 x) y) (values #f #f))]
		   [(sud-weight s-bid)  (if (coord-alright? x (sub1 y) back-check) (get-move-weight x (sub1 y)) (values #f #f))]
		   [(nort-weight n-bid) (if (coord-alright? x (add1 y) back-check) (get-move-weight x (add1 y)) (values #f #f))])
	(cond
	 [(not (null? drop)) (maker drop-weight x y d-bid 'D drop)]
	 [(and first? (not (null? pick))) (maker pick-weight x y p-bid 'P pick)]
	 [else (list (when nort-weight (maker (+ weight nort-weight) x (add1 y) n-bid 'N null))
		     (when east-weight (maker (+ weight east-weight) (add1 x) y e-bid 'E null))
		     (when sud-weight  (maker (+ weight sud-weight) x (sub1 y) s-bid 'S null))
		     (when west-weight (maker (+ weight west-weight) (sub1 x) y w-bid 'W null)))]))))
  
  (define (compute-move packages robots)
    (queue-head null)
    (in-queue 0)
    (when (null? (last-move)) (last-move (make-cord -inf.0 -inf.0)))
    (best-cmd (make-command 1 'P null))
    (best-weight -inf.0)
    
    (current-player (make-search-player (get-player-x) (get-player-y) (player-id) (player-money) (player-capacity) (packages-held)))
    (update-robots robots (current-player))
    
    (let ((moves (generate-moves #t (get-player-x) (get-player-y) 0 (list (last-move)) 0 #f packages)))
      (if (move? moves)
          (best-cmd (move-orig moves))
          (begin (for-each enqueue moves)
                 (unless (null? (queue-head))
		   (search-node (dequeue))))))
    
    (last-move (make-cord (get-player-x)
                          (get-player-y)))

    (best-cmd)
    )

  
  (define (search-node move)
    (when (< (move-step move) 30)
      (let ((moves (generate-moves #f
				   (move-x move) 
                                   (move-y move)
                                   (move-weight move)
                                   (move-back move) 
                                   (add1 (move-step move))  
                                   (move-orig move)
				   '())))
        (if (move? moves)
            (best-cmd (move-orig moves))
            (begin (for-each enqueue moves)
                   (unless (null? (queue-head))
		     (search-node (dequeue))))))))
  
  )