(unit/sig loa^
  (import mzlib:function^
	  mred^
	  loa:grid^)

  (define color "BLUE")

  (define black-pen (send the-pen-list find-or-create-pen "BLACK" 1 'solid))
  (define black-brush (send the-brush-list find-or-create-brush "BLACK" 'solid))
  (define white-pen (send the-pen-list find-or-create-pen color 1 'solid))
  (define white-brush (send the-brush-list find-or-create-brush color 'solid))

  (define loa-canvas% grid-canvas%)

  (define loa-pasteboard%
    (class/d grid-pasteboard% args
      ((inherit get-snip-at)
       (override get-moves))

      (define (on-board? x y)
	(and (<= 0 x 7)
	     (<= 0 y 7)))

      (define (iterate-in-dir x y step init combine)
	(if (on-board? x y)
	    (let-values ([(nx ny) (step x y)])
	      (iterate-in-dir nx ny step (combine init x y) combine))
	    init))
      
      (define (find-line snip forward backward)
	(let*-values ([(x y) (values (send snip get-x) (send snip get-y))]
		      [(color) (send snip get-color)]
		      [(fx fy) (forward x y)]
		      [(bx by) (backward x y)])
	  (letrec ([find-spaces
		    (lambda (x y move)
		      (let loop ([x x]
				 [y y])
			(cond
			 [(on-board? x y)
			  (cons (cons x y)
				(let-values ([(nx ny) (move x y)])
				  (loop nx ny)))]
			 [else null])))]
		   [spaces
		    (append
		     (list (cons x y))
		     (find-spaces fx fy forward)
		     (find-spaces bx by backward))]
		   [count (foldl (lambda (p c)
				   (if (get-snip-at (car p) (cdr p))
				       (+ c 1)
				       c))
				 0
				 spaces)]
		   [step-n
		    (lambda (dir x y n)
		      (cond
		       [(not (on-board? x y)) #f]
		       [(zero? n)
			(let ([nsnip (get-snip-at x y)])
			  (if (or (not nsnip)
				  (not (eq? color (send nsnip get-color))))
			      (cons x y)
			      #f))]
		       [else (let-values ([(nx ny) (dir x y)])
			       (let ([n-snip (get-snip-at x y)])
				 (if (or (not n-snip)
					 (eq? color (send n-snip get-color)))
				     (step-n dir nx ny (- n 1))
				     #f)))]))]

		   [step-forward (step-n forward x y count)]
		   [step-forward-l (if step-forward (list step-forward) null)]
		   [step-backward (step-n backward x y count)]
		   [step-backward-l (if step-backward (cons step-backward step-forward-l) step-forward-l)])

	    step-backward-l)))

      (define (get-moves snip)
	(let ([x (send snip get-x)]
	      [y (send snip get-y)])

	  (append (find-line snip (lambda (x y) (values x (+ y 1))) (lambda (x y) (values x (- y 1))))
		  (find-line snip (lambda (x y) (values (+ x 1) (+ y 1))) (lambda (x y) (values (- x 1) (- y 1))))
		  (find-line snip (lambda (x y) (values (- x 1) (+ y 1))) (lambda (x y) (values (+ x 1) (- y 1))))
		  (find-line snip (lambda (x y) (values (+ x 1) y)) (lambda (x y) (values (- x 1) y))))))

      (apply super-init args)))

  (define loa-checker%
    (class grid-snip% (color x y)

      (inherit get-width get-height)
      (public
	[get-color
	 (lambda () color)])
      (override
	[draw
	 (lambda (dc x y left top right bottom dx dy draw-caret)
	   (let ([width (get-width)]
		 [height (get-height)])
	     (if (eq? color 'black)
		 (begin (send dc set-pen black-pen)
			(send dc set-brush black-brush))
		 (begin (send dc set-pen white-pen)
			(send dc set-brush white-brush)))
	     (send dc draw-ellipse x y width height)))])

      (sequence
	(super-init x y)))))

