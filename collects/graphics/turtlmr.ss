(let ([turtles:window #f]
      [turtles:shown? #f])
(unit/sig turtle^
  (import mzlib:function^
	  turtle:create-window^)
  
  (define plot-window%
    (class wx:frame% (name width height)
      (inherit show set-menu-bar)
      (public 
	[bitmap (make-object wx:bitmap% width height)])
      (private
	[w-pen (make-object wx:pen% "white" 1 wx:const-solid)]
	[w-brush (make-object wx:brush% "white" wx:const-solid)]
	[b-pen (make-object wx:pen% "black" 1 wx:const-solid)]
	[b-brush (make-object wx:brush% "black" wx:const-solid)]
	[memory-dc (make-object wx:memory-dc%)])
      (public
	[canvas% 
	 (class wx:canvas% args
	   (inherit set-background draw-rectangle set-pen 
		    get-dc clear get-client-size)
	   (sequence
	     (apply super-init args))
	   (private
	     [dc (get-dc)]
	     [draw-line (ivar dc draw-line)])
	   (public
	     [on-paint
	      (lambda ()
		(send dc blit 0 0 width height memory-dc 0 0 wx:const-copy))]))]
	[clear (lambda () 
		 (send memory-dc set-pen w-pen)
		 (send memory-dc draw-rectangle 0 0 width height)
		 (send canvas on-paint)
		 (send memory-dc set-pen b-pen))])
      (sequence
	(send memory-dc select-object bitmap)
	(super-init '() name 0 0 width height)
	'(set-client-size (+ width 16) (+ height 16)))
      
      (public
	[on-menu-command (lambda (op) (turtles #f))])
      (private
	[menu-bar (make-object wx:menu-bar%)]
	[menu (make-object wx:menu%)])
      (sequence 
	(send menu append 1 "Close")
	(send menu-bar append menu "File")
	(set-menu-bar menu-bar))
      
      (public
	[canvas (make-object canvas% this 0 0 width height)]
	[wipe-line (let ([dc-line (ivar memory-dc draw-line)]
			 [canvas-line (ivar canvas draw-line)]
			 [dc-pen (ivar memory-dc set-pen)]
			 [canvas-pen (ivar canvas set-pen)])
		     (lambda (a b c d)
		       (dc-pen w-pen)
		       (canvas-pen w-pen)
		       (dc-line a b c d)
		       (canvas-line a b c d)
		       (dc-pen b-pen)
		       (canvas-pen b-pen)))]
	[draw-line (let ([dc-line (ivar memory-dc draw-line)]
			 [canvas-line (ivar canvas draw-line)])
		     (lambda (a b c d)
		       (dc-line a b c d)
		       (canvas-line a b c d)))])
      (sequence
	(clear))))
  
  (define turtle-window-size
    (let ([w (box 0)]
	  [h (box 0)]
	  [user/client-offset 65.])
      (wx:display-size w h)
      (min 800.
	   (- (unbox w) user/client-offset)
	   (- (unbox h) user/client-offset))))
  
  (define-struct turtle (x y angle))
  (define-struct cached (turtles cache))
  (define-struct tree (children))
  
  ;; *Turtles* is either a
  ;;    - list of turtles or
  ;;    - a tree with a list of *cached-Turtles*
  ;; 
  ;; *cached-Turtles* is a pair of a *Turtle* 
  ;;		    and a function that 
  ;;                  maps turtles to turtles, 
  ;;                  applying the cache.
  
  (define Clear-Turtle (make-turtle (/ turtle-window-size 2)
				    (/ turtle-window-size 2) 0))
  (define Turtles (list Clear-Turtle))
  
  ;; the cache contains a turtle-offset, which is represented
  ;; by a turtle -- but it is a delta not an absolute.
  (define Empty-Cache (make-turtle 0 0 0))
  (define Cache Empty-Cache)
  
  (define init-error (lambda _ (error 'turtles "window is not shown. apply `turtles' to no arguments")))
  (define line (if turtles:window
		   (ivar turtles:window draw-line)
		   init-error))
  (define wipe-line (if turtles:window
			(ivar turtles:window wipe-line)
			init-error))
  (define clear-window (if turtles:window
			   (ivar turtles:window clear)
			   init-error))
  (define save-turtle-bitmap
    (lambda (filename type)
      (if turtles:window
	  (send (ivar turtles:window bitmap) save-file filename type)
	  (init-error))))

  ; clear the turtles window on each execution
  (when turtles:window
    (send turtles:window clear))
  
  (define turtles
    (case-lambda
     [() (turtles #t)]
     [(x)
      (set! turtles:shown? x)
      (unless turtles:window
	(set! turtles:window
	      (create-turtle-window plot-window%
				    "Turtles"
				    turtle-window-size
				    turtle-window-size))
	(set! line (ivar turtles:window draw-line))
	(set! wipe-line (ivar turtles:window wipe-line))
	(set! clear-window (ivar turtles:window clear)))
      (send turtles:window show x)]))
  
  (define clear 
    (lambda ()
      (clear-window)
      (set! Cache Empty-Cache)
      (set! Turtles (list Clear-Turtle))))
  
  ;; cache elements:
  (define-struct c-forward (distance))
  (define-struct c-turn (angle))
  (define-struct c-draw (distance))
  (define-struct c-offset (x y))
  
  ;; combines a cache-element and a turtle-offset.
  ;; turtle-offsets are represented as turtles, 
  ;; however they are deltas, not absolutes.
  (define combine
    (lambda (entry Cache)
      (cond 
	[(c-forward? entry)
	 (let* ([n (c-forward-distance entry)]
		[angle (turtle-angle Cache)]
		[x (turtle-x Cache)]
		[y (turtle-y Cache)]
		[newx (+ x (* n (cos angle)))]
		[newy (+ y (* n (sin angle)))])
	   (make-turtle newx newy angle))]
	[(c-offset? entry)
	 (let* ([tx (turtle-x Cache)]
		[ty (turtle-y Cache)]
		[newx (+ tx (c-offset-x entry))]
		[newy (+ ty (c-offset-y entry))])
	   (make-turtle newx newy 
			(turtle-angle Cache)))]
	[(c-turn? entry)
	 (make-turtle (turtle-x Cache)
		      (turtle-y Cache)
		      (- (turtle-angle Cache)
			 (c-turn-angle entry)))]
	[else
	 (error 'turtle-cache "illegal entry in cache: ~a" entry)])))
  
  ;; this applies an offset to a turtle.
  ;; an offset is a turtle, representing what would happen 
  ;;    if the turtle had started at zero.
  (define apply-cache
    (lambda (offset)
      (let ([x (turtle-x offset)]
	    [y (turtle-y offset)]
	    [offset-angle (turtle-angle offset)])
	(lambda (Turtle)
	  (let* ([angle (turtle-angle Turtle)])
	    (let* ([c (cos angle)]
		   [s (sin angle)]
		   [rx (- (* x c) (* y s))]
		   [ry (+ (* y c) (* x s))])
	      (make-turtle (+ rx (turtle-x Turtle))
			   (+ ry (turtle-y Turtle))
			   (+ offset-angle angle))))))))
  
  (define flatten
    (lambda (at-end)
      (letrec ([walk-turtles
		(lambda (Turtles Cache list)
		  (cond
		    [(tree? Turtles)
		     (let ([children (tree-children Turtles)]
			   [ac (apply-cache Cache)])
		       (foldl (lambda (child list)
				(walk-turtles (cached-turtles child)
					      (ac (cached-cache child))
					      list))
			      list
			      children))]
		    [else
		     (let ([f (compose at-end (apply-cache Cache))])
		       (foldl (lambda (T l) (cons (f T) l)) list Turtles))]))])
	(set! Turtles (walk-turtles Turtles Cache null))
	(set! Cache Empty-Cache))))
  
  (define clear-cache flatten)
  
  (define draw/erase
    (lambda (doit)
      (lambda (n)
	(clear-cache (lambda (Turtle)
		       (let* ([x (turtle-x Turtle)]
			      [y (turtle-y Turtle)]
			      [angle (turtle-angle Turtle)]
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
			 (make-turtle newx newy angle)))))))
  (define draw (draw/erase (lambda (a b c d) (line a b c d))))
  (define erase (draw/erase (lambda (a b c d) (wipe-line a b c d))))
  
  (define move
    (lambda (n)
      (set! Cache (combine (make-c-forward n) Cache))))
  
  (define turn/radians
    (lambda (d)
      (set! Cache (combine (make-c-turn d) Cache))))
  
  (define turn
    (lambda (c)
      (turn/radians (* (/ c 360) 2 pi))))
  
  (define move-offset
    (lambda (x y)
      (set! Cache (combine (make-c-offset x y) Cache))))
  
  (define erase/draw-offset
    (lambda (doit)
      (lambda (x y)
	(clear-cache (lambda (Turtle)
		       (let* ([tx (turtle-x Turtle)]
			      [ty (turtle-y Turtle)]
			      [newx (+ tx x)]
			      [newy (+ ty y)])
			 (doit tx ty newx newy)
			 (make-turtle newx newy (turtle-angle Turtle))))))))
  
  (define erase-offset (erase/draw-offset (lambda (a b c d) (wipe-line a b c d))))
  (define draw-offset (erase/draw-offset (lambda (a b c d) (line a b c d))))
  
  (define splitfn
    (lambda (E)
      (let ([T Turtles]
	    [C Cache])
	(E)
	(set! Turtles (make-tree (list (make-cached Turtles Cache)
				       (make-cached T C))))
	(set! Cache Empty-Cache))))
  
  (define split*fn
    (lambda Es
      (let ([T Turtles]
	    [C Cache]
	    [l '()])
	(for-each (lambda (x)
		    (x)
		    (set! l (cons (make-cached Turtles Cache) l))
		    (set! Turtles T)
		    (set! Cache C))
		  Es)
	(set! Cache Empty-Cache)
	(set! Turtles (make-tree l)))))
  
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
    (clear))))



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
