(module search mzscheme
  
  (require (lib "list.ss"))
  
  (require "board.ss"
           "client-parameters.ss"
           "heuristics.ss"
           "weights.scm")
  
  (provide compute-move best-cmd)

  (define search-limit (make-parameter 800))
  (define (weight-from-goal x) (* (step-weight) x))
  
  (define-struct queue (head tail))
  
  (define (create-queue)
    (make-queue null null))
  
  (define (enqueue! q v)
    (let ((new-cell (cons v null)))
      (cond
        ((not (null? (queue-tail q)))
         (set-cdr! (queue-tail q) new-cell)
         (set-queue-tail! q new-cell))
        (else (set-queue-head! q new-cell)
              (set-queue-tail! q new-cell)))))
  
  (define (dequeue! q)
    (begin0
      (car (queue-head q))
      (cond
        ((eq? (queue-head q) (queue-tail q))
         (set-queue-tail! q null)))
      (set-queue-head! q (cdr (queue-head q)))))

  (define (queue-empty? queue)
    (null? (queue-head queue)))

  ;(make-qelt qelt move qelt)
  (define-struct qelt (weight dir x y cmd path/coords))
  
  ;(make-move num num num cord num list-of-command)
  (define-struct move (weight x y back step orig))
  ;(make-cord num num)
  (define-struct cord (x y) (make-inspector))
  
  (define current-player (make-parameter null))
  
  (define best-cmd (make-parameter null))
  (define best-weight (make-parameter -inf.0))
  ;(define last-move (make-parameter null))

  (define (coord-alright? x y back-coord-list)
    (let ([type (get-type (get-spot (board) x y))])
      (and (not (or (= type 1) (= type 2)))
	   (not (member (make-cord x y) back-coord-list)))))

  (define (get-move-weight x y)
    (let-values (((weight bid _ __) (calc-weight 'M x y (current-player) null)))
      (values weight bid)))

  (define (breadth-search limit queue)
    (cond
      [(or (queue-empty? queue) (= limit 0)) (void)]
      [else (let ([cur-item (dequeue! queue)])
	      (let ([weight (qelt-weight cur-item)]
		    [dir (qelt-dir cur-item)]
		    [x (qelt-x cur-item)]
		    [y (qelt-y cur-item)]
		    [cmd (qelt-cmd cur-item)]
		    [path/coords (qelt-path/coords cur-item)])
		(let ([depth (length path/coords)])
		  (let-values ([(cur-weight cur-bid) (get-move-weight x y)])
		    (let ([aveweight (/ (+ weight cur-weight) depth)]
			  [cmd (if cmd cmd (make-command cur-bid dir '()))]
			  [path/coords (cons (make-cord x y) path/coords)]
			  [weight (+ weight cur-weight)])
		      (when (= depth 1)
;			(printf "Move to the ~a has weight ~a~n" (command-command cmd) cur-weight))
		      (let ([northopt (make-qelt weight 'N x (add1 y) cmd path/coords)]
			    [southopt (make-qelt weight 'S x (sub1 y) cmd path/coords)]
			    [eastopt (make-qelt weight 'E (add1 x) y cmd path/coords)]
			    [westopt (make-qelt weight 'W (sub1 x) y cmd path/coords)])
			(when (> aveweight (best-weight))
			  (best-weight aveweight)
			  (best-cmd cmd))
			(when (coord-alright? (qelt-x northopt) (qelt-y northopt) path/coords)
			  (enqueue! queue northopt))
			(when (coord-alright? (qelt-x eastopt) (qelt-y eastopt) path/coords)
			  (enqueue! queue eastopt))
			(when (coord-alright? (qelt-x westopt) (qelt-y westopt) path/coords)
			  (enqueue! queue westopt))
			(when (coord-alright? (qelt-x southopt) (qelt-y southopt) path/coords)
			  (enqueue! queue southopt))
			(breadth-search (sub1 limit) queue)))))))]))

  (define (compute-move packages robots)
    (let ([x (get-player-x)]
	  [y (get-player-y)])
      (current-player (make-search-player x y (player-id) (player-money) (player-capacity) (packages-held)))
      (update-robots robots (current-player))
      (let-values ([(pick-weight p-bid _b packs-to-pick) (calc-weight 'P x y (current-player) packages)]
		   [(drop-weight d-bid packs-to-drop _d) (calc-weight 'D x y (current-player) packages)])
	(best-cmd (make-command 1 'P null)) (best-weight -inf.0)
	(let ([start-coord-list (list (make-cord x y))]
	      [queue (create-queue)])
	  
	  (when (coord-alright? x (add1 y) '())
	    (enqueue! queue (make-qelt 0 'N x (add1 y) #f start-coord-list)))
	  (when (coord-alright? (add1 x) y '())
	    (enqueue! queue (make-qelt 0 'E (add1 x) y #f start-coord-list)))
	  (when (coord-alright? x (sub1 y) '())
	    (enqueue! queue (make-qelt 0 'S x (sub1 y) #f start-coord-list)))
	  (when (coord-alright? (sub1 x) y '())
	    (enqueue! queue (make-qelt 0 'W (sub1 x) y #f start-coord-list)))
	  (breadth-search (search-limit) queue)

	  (let ([drop-weight (if (null? packs-to-drop) -inf.0 drop-weight)]
		[pick-weight (if (null? packs-to-pick) -inf.0 pick-weight)])
;	      (printf "(best-weight): ~a~ndrop-weight: ~a~npick-weight: ~a~n"
;		(best-weight) drop-weight pick-weight)
	      (cond
		[(and (>= drop-weight (best-weight)) (>= drop-weight pick-weight))
		 (make-command d-bid 'D packs-to-drop)]
		[(>= (best-weight) pick-weight) (best-cmd)]
		[else (make-command p-bid 'P packs-to-pick)]))))))

  
)