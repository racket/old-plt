(require-library "pretty.ss")

(unit/sig loa^
  (import mzlib:function^
	  mred^
	  loa:computer-player^
	  loa:grid^)

  (define color "VIOLET RED")

  (define black-pen (send the-pen-list find-or-create-pen "BLACK" 1 'solid))
  (define black-brush (send the-brush-list find-or-create-brush "BLACK" 'solid))
  (define white-pen (send the-pen-list find-or-create-pen color 1 'solid))
  (define white-brush (send the-brush-list find-or-create-brush color 'solid))

  (define loa-canvas% grid-canvas%)

  (define (get-connected-region board snip)
    (let ([ht (make-hash-table)])
      (let loop ([snip snip])
	(hash-table-get
	 ht
	 snip
	 (lambda ()
	   (hash-table-put! ht snip #t)
	   (let* ([x (send snip get-x)]
		  [y (send snip get-y)]
		  [check
		   (lambda (nx ny)
		     (let* ([next-snip (send board get-snip-at nx ny)]
			    [condition
			     (and next-snip
				  (eq? (send next-snip get-color)
				       (send snip get-color)))])
		       (printf "at (~a, ~a) looking (~a, ~a): ~a~n" x y nx ny condition)
		       (when condition
			 (loop next-snip))))])
	     (check (+ x 1) y)
	     (check (- x 1) y)
	     (check x (+ y 1))
	     (check x (- y 1))
	     (check (+ x 1) (+ y 1))
	     (check (- x 1) (+ y 1))
	     (check (+ x 1) (- y 1))
	     (check (- x 1) (- y 1))))))
      (hash-table-map ht (lambda (x y) x))))

  (define (get-connected-regions board)
    (let loop ([regions null]
	       [snip (send board find-first-snip)])
      (cond
       [(not snip) regions]
       [(ormap (lambda (region) (member snip region))
	       regions)
	(loop regions
	      (send snip next))]
       [else
	(loop (cons (get-connected-region board snip) regions)
	      (send snip next))])))

  (define loa-pasteboard%
    (class/d grid-pasteboard% args
      ((inherit get-snip-at get-all-snips-at
		animate-to find-first-snip
		remove)
       (override get-moves moved))

      (define (make-move snip x y)
	(send snip set-x x)
	(send snip set-y y)
	(animate-to snip x y))

      (define (do-computer-move)
	(let-values ([(snip x y) (computer-move this)])
	  (make-move snip x y)))

      (define (moved moved-snips)
	(for-each (lambda (moved-snip)
		    (for-each (lambda (overlapping-snip)
				(unless (eq? overlapping-snip moved-snip)
				  (remove overlapping-snip)))
			      (get-all-snips-at (send moved-snip get-x) (send moved-snip get-y))))
		  moved-snips)
	(let ([semaphore (make-semaphore)])
	  (thread
	   (lambda ()
	     (do-computer-move)
	     (semaphore-post semaphore)))
	  (yield semaphore)))

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
	(super-init x y))))


  (define frame (make-object frame% "Lines of Action" #f))
  (define loa-pasteboard (make-object loa-pasteboard% 8 8))
  (define loa-canvas (make-object loa-canvas% frame loa-pasteboard))

  (send loa-canvas min-width 300)
  (send loa-canvas min-height 300)

  (define (make color x y)
    (send loa-pasteboard insert
	  (make-object loa-checker% color x y)))

  (let loop ([n 6])
    (unless (zero? n)
      (make 'white 0 n)
      (make 'white 7 n)
      (make 'black n 0)
      (make 'black n 7)
      (loop (- n 1))))

  (send frame show #t))

