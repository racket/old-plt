(unit/sig turtle^
  (import [mred : mred^]
	  mzlib:function^)
  
  (define turtles:window #f)
  (define turtles:shown? #f)

  (define turtle-icon-color "SALMON")
  (define turtle-icon-pen (send mred:the-pen-list find-or-create-pen
				turtle-icon-color
				1 'solid))

  (define show-turtle-icons? #t)

  (define plot-window%
    (class mred:frame% (name width height)
      (inherit show)
      (public 
	[bitmap (make-object mred:bitmap% width height #t)])
      (private
	[w-pen (send mred:the-pen-list find-or-create-pen "white" 1 'solid)]
	[w-brush (send mred:the-brush-list find-or-create-brush "white" 'solid)]
	[b-pen (send mred:the-pen-list find-or-create-pen "black" 1 'solid)]
	[b-brush (send mred:the-brush-list find-or-create-brush "black" 'solid)]
	[xor-pen (send mred:the-pen-list find-or-create-pen "black" 1 'xor)]
	[memory-dc (make-object mred:bitmap-dc%)]
	[flip-icons-dc
	 (lambda (dc)
	   (flatten (lambda (x) x))
	   (send dc set-pen xor-pen)
	   (for-each (lambda (turtle)
		       (let ([x (turtle-x turtle)]
			     [y (turtle-y turtle)]
			     [theta (turtle-angle turtle)]
			     [size 2])
			 (send dc draw-line
			       x y
			       (+ x (* size (cos theta)))
			       (+ y (* size (sin theta))))))
		     turtles-state)
	   (send dc set-pen b-pen))])
      (public
	[flip-icons
	 (lambda ()
	   (flip-icons-dc
	    (send canvas get-dc)))]
	[draw-into-dc
	 (lambda (dc)
	   (send dc clear)
	   (send dc draw-bitmap (send memory-dc get-bitmap) 0 0)
	   (flip-icons-dc dc))]
	[canvas% 
	 (class mred:canvas% args
	   (inherit get-dc)
	   (override
	    [on-paint
	      (lambda ()
		(draw-into-dc (get-dc)))])
	   (sequence (apply super-init args)))]
	[clear
	 (lambda () 
	   (send memory-dc set-pen w-pen)
	   (send memory-dc set-brush w-brush)
	   (send memory-dc draw-rectangle 0 0 width height)
	   (send canvas on-paint)
	   (send memory-dc set-pen b-pen))])
      (sequence
	(send memory-dc set-bitmap bitmap)
	(send memory-dc clear)
	(super-init name #f width height))
      
      (public
	[on-menu-command (lambda (op) (turtles #f))])
      (private
	[menu-bar (make-object mred:menu-bar% this)]
	[file-menu (make-object mred:menu% "File" menu-bar)])
      (sequence 
	(make-object mred:menu-item%
	  "Print"
	  file-menu
	  (lambda (_1 _2)
	    (turtles #f)))
	(make-object mred:menu-item%
	  "Close"
	  file-menu
	  (lambda (_1 _2)
	    (turtles #f))))
      
      (public
	[save-turtle-bitmap
	 (lambda (fn type)
	   (send bitmap save-file fn type))])

      (public
	[canvas (make-object canvas% this)]
	[wipe-line (let* ([dc (send canvas get-dc)]
			  [dc-line (ivar memory-dc draw-line)]
			  [canvas-line (ivar dc draw-line)]
			  [dc-pen (ivar memory-dc set-pen)]
			  [canvas-pen (ivar dc set-pen)])
		     (lambda (a b c d)
		       (dc-pen w-pen)
		       (canvas-pen w-pen)
		       (dc-line a b c d)
		       (canvas-line a b c d)
		       (dc-pen b-pen)
		       (canvas-pen b-pen)))]
	[draw-line (let* ([dc (send canvas get-dc)]
			  [dc-line (ivar memory-dc draw-line)]
			  [canvas-line (ivar dc draw-line)])
		     (lambda (a b c d)
		       (dc-line a b c d)
		       (canvas-line a b c d)))])
      (sequence
	(send canvas min-width width)
	(send canvas min-height height)
	(clear))))
  
  (define turtle-window-size
    (let-values ([(w h) (mred:get-display-size)]
		 [(user/client-offset) 65])
      (min 800
	   (- w user/client-offset)
	   (- h user/client-offset))))
  
  (define-struct turtle (x y angle))
  ; x : int
  ; y: int
  ; angle : int

  (define-struct cached (turtles cache))
  ; turtles : (list-of turtle)
  ; cache : turtle -> turtle

  (define-struct tree (children))
  ; children : (list-of cached)
  
  (define clear-turtle (make-turtle (/ turtle-window-size 2)
				    (/ turtle-window-size 2) 0))

  ;; turtles-state is either a
  ;;    - (list-of turtle) or
  ;;    - tree 
  (define turtles-state (list clear-turtle))
  
  ;; the cache contains a turtle-offset, which is represented
  ;; by a turtle -- but it is a delta not an absolute.
  (define empty-cache (make-turtle 0 0 0))
  (define turtles-cache empty-cache)
  
  (define init-error (lambda _ (error 'turtles "Turtles not initialized. Evaluate (turtles).")))
  (define inner-line init-error)
  (define inner-wipe-line init-error)
  (define inner-clear-window init-error)
  (define inner-save-turtle-bitmap init-error)

  (define line (lambda (a b c d) (inner-line a b c d)))
  (define wipe-line (lambda (a b c d) (inner-wipe-line a b c d)))
  (define clear-window (lambda () (inner-clear-window)))
  (define save-turtle-bitmap (lambda (x y) (inner-save-turtle-bitmap x y)))

  (define turtles
    (case-lambda
     [() (turtles #t)]
     [(x)
      (set! turtles:shown? x)
      (unless turtles:window
	(set! turtles:window
	      (make-object plot-window%
		"Turtles"
		turtle-window-size
		turtle-window-size))
	(set! inner-line (ivar turtles:window draw-line))
	(set! inner-wipe-line (ivar turtles:window wipe-line))
	(set! inner-clear-window (ivar turtles:window clear))
	(set! inner-save-turtle-bitmap (ivar turtles:window save-turtle-bitmap)))
      (send turtles:window show x)]))
  
  (define clear 
    (lambda ()
      (clear-window)
      (set! turtles-cache empty-cache)
      (set! turtles-state (list clear-turtle))))
  
  ;; cache elements:
  (define-struct c-forward (distance))
  (define-struct c-turn (angle))
  (define-struct c-draw (distance))
  (define-struct c-offset (x y))
  
  ;; combines a cache-element and a turtle-offset.
  ;; turtle-offsets are represented as turtles, 
  ;; however they are deltas, not absolutes.
  (define combine
    (lambda (entry cache)
      (cond 
	[(c-forward? entry)
	 (let* ([n (c-forward-distance entry)]
		[angle (turtle-angle cache)]
		[x (turtle-x cache)]
		[y (turtle-y cache)]
		[newx (+ x (* n (cos angle)))]
		[newy (+ y (* n (sin angle)))])
	   (make-turtle newx newy angle))]
	[(c-offset? entry)
	 (let* ([tx (turtle-x cache)]
		[ty (turtle-y cache)]
		[newx (+ tx (c-offset-x entry))]
		[newy (+ ty (c-offset-y entry))])
	   (make-turtle newx newy 
			(turtle-angle cache)))]
	[(c-turn? entry)
	 (make-turtle (turtle-x cache)
		      (turtle-y cache)
		      (- (turtle-angle cache)
			 (c-turn-angle entry)))]
	[else
	 (error 'turtles-cache "illegal entry in cache: ~a" entry)])))
  
  ;; this applies an offset to a turtle.
  ;; an offset is a turtle, representing what would happen 
  ;;    if the turtle had started at zero.
  (define apply-cache
    (lambda (offset)
      (let ([x (turtle-x offset)]
	    [y (turtle-y offset)]
	    [offset-angle (turtle-angle offset)])
	(lambda (turtle)
	  (let* ([angle (turtle-angle turtle)])
	    (let* ([c (cos angle)]
		   [s (sin angle)]
		   [rx (- (* x c) (* y s))]
		   [ry (+ (* y c) (* x s))])
	      (make-turtle (+ rx (turtle-x turtle))
			   (+ ry (turtle-y turtle))
			   (+ offset-angle angle))))))))
  
  (define flatten
    (lambda (at-end)
      (letrec ([walk-turtles
		(lambda (turtles cache list)
		  (cond
		    [(tree? turtles)
		     (let ([children (tree-children turtles)]
			   [ac (apply-cache cache)])
		       (foldl (lambda (child list)
				(walk-turtles (cached-turtles child)
					      (ac (cached-cache child))
					      list))
			      list
			      children))]
		    [else
		     (let ([f (compose at-end (apply-cache cache))])
		       (foldl (lambda (t l) (cons (f t) l)) list turtles))]))])
	(set! turtles-state (walk-turtles turtles-state turtles-cache null))
	(set! turtles-cache empty-cache))))
  
  (define (flip-icons) (send turtles:window flip-icons))

  (define draw/erase
    (lambda (doit)
      (lambda (n)
	(flip-icons)
	(flatten
	 (lambda (turtle)
	   (let* ([x (turtle-x turtle)]
		  [y (turtle-y turtle)]
		  [angle (turtle-angle turtle)]
		  [d (if (zero? n) 0 (sub1 (abs n)))]
		  [res (if (< n 0) (- d) d)]
		  [c (cos angle)]
		  [s (sin angle)]
		  [drawx (+ x (* res c))]
		  [drawy (+ y (* res s))]
		  [newx (+ x (* n c))]
		  [newy (+ y (* n s))])
	     (unless (zero? n)
	       (doit x y drawx drawy))
	     (make-turtle newx newy angle))))
	(flip-icons))))

  (define draw (draw/erase (lambda (a b c d) (line a b c d))))
  (define erase (draw/erase (lambda (a b c d) (wipe-line a b c d))))
  
  (define move
    (lambda (n)
      (flip-icons)
      (set! turtles-cache (combine (make-c-forward n) turtles-cache))
      (flip-icons)))
  
  (define turn/radians
    (lambda (d)
      (flip-icons)
      (set! turtles-cache (combine (make-c-turn d) turtles-cache))
      (flip-icons)))
  
  (define turn
    (lambda (c)
      (flip-icons)
      (turn/radians (* (/ c 360) 2 pi))
      (flip-icons)))
  
  (define move-offset
    (lambda (x y)
      (flip-icons)
      (set! turtles-cache (combine (make-c-offset x y) turtles-cache))
      (flip-icons)))
  
  (define erase/draw-offset
    (lambda (doit)
      (lambda (x y)
	(flip-icons)
	(flatten
	 (lambda (turtle)
	   (let* ([tx (turtle-x turtle)]
		  [ty (turtle-y turtle)]
		  [newx (+ tx x)]
		  [newy (+ ty y)])
	     (doit tx ty newx newy)
	     (make-turtle newx newy (turtle-angle turtle)))))
	(flip-icons))))
  
  (define erase-offset (erase/draw-offset (lambda (a b c d) (wipe-line a b c d))))
  (define draw-offset (erase/draw-offset (lambda (a b c d) (line a b c d))))
  
  (define splitfn
    (lambda (e)
      (flip-icons)
      (let ([t turtles-state]
	    [c turtles-cache])
	(e)
	(set! turtles-state
	      (make-tree (list (make-cached turtles-state turtles-cache)
			       (make-cached t c))))
	(set! turtles-cache empty-cache)
	(flip-icons))))
  
  (define split*fn
    (lambda (es)
      (let ([t turtles-state]
	    [c turtles-cache]
	    [l '()])
	(flip-icons)
	(for-each (lambda (x)
		    (x)
		    (set! l (cons (make-cached turtles-state turtles-cache) l))
		    (set! turtles-state t)
		    (set! turtles-cache c))
		  es)
	(set! turtles-cache empty-cache)
	(set! turtles-state (make-tree l))
	(flip-icons))))
  
  (define pi 3.1415926535)
  
  (lambda ()
    (turn (/ pi 2))
    (move 10)
    (draw 10)
    (move 20)
    (turn (/ pi 3))
    (draw 10)
    (turn pi)
    (splitfn (lambda () (turn (/ pi 2))))
    (move 10)
    (split*fn (list (lambda () (turn (/ pi 2)))
		    (lambda () (move 200))
		    (lambda ()
		      (splitfn (lambda () 
				 (turn (/ pi 2)))))))
    (draw 10)
    (clear)))



  #|
  E ::= (move n)
      | (draw n)
      | (erase n)
      | (turn d)
      | (tprompt E)
      | (split E)
      | (split* E ... E)
      | (flip)  ; not implemented currently
  |#
