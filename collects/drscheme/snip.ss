(unit/sig drscheme:snip^
  (import [mred : mred-interfaces^])

  (define separator-snipclass
    (make-object
	(class-asi mred:snip-class%
	  (public
	    [read (lambda (s) 
		    (let ([size-box (box 0)])
		      (send s get size-box)
		      (make-object separator-snip%)))]))))
  (send* separator-snipclass
	 (set-version 1)
	 (set-classname "drscheme:sepatator-snip%"))
  (send (mred:get-the-snip-class-list) add separator-snipclass)


  ;; the two numbers 1 and 2 which appear here are to line up this snip
  ;; with the embedded snips around it in the drscheme rep.
  ;; I have no idea where the extra pixels are going.
  (define separator-snip%
    (class mred:snip% ()
      (inherit get-style set-snipclass set-flags get-flags get-admin)
      (private [width 500]
	       [height 1]
	       [white-around 2])
      (public
	[write (lambda (s) 
		 (send s put (char->integer #\r)))]
	[copy (lambda () 
		(let ([s (make-object (object-class this))])
		  (send s set-style (get-style))
		  s))]
	[get-extent
	 (lambda (dc x y w-box h-box descent-box space-box lspace-box rspace-box)
	   (for-each (lambda (box) (unless (null? box) (set-box! box 0)))
		     (list descent-box space-box lspace-box rspace-box))
	   (let* ([admin (get-admin)]
		  [reporting-media (send admin get-media)]
		  [reporting-admin (send reporting-media get-admin)]
		  [widthb (box 0)]
		  [space 2])
	     (send reporting-admin get-view null null widthb null)
	     (set! width (- (unbox widthb) 
			    space
			    2)))
	   (set! height 1)
	   (unless (null? w-box)
	     (set-box! w-box width))
	   (unless (null? h-box)
	     (set-box! h-box (+ (* 2 white-around) height))))]
	[draw
	 (let* ([body-pen (send mred:the-pen-list find-or-create-pen
				"BLACK" 0 'solid)]
		[body-brush (send mred:the-brush-list find-or-create-brush
				  "BLACK" 'solid)])
	   (lambda (dc x y left top right bottom dx dy drawCaret)
	     (let ([orig-pen (send dc get-pen)]
		   [orig-brush (send dc get-brush)])
	       (send dc set-pen body-pen)
	       (send dc set-brush body-brush)
	       
	       (send dc draw-rectangle (+ x 1)
		     (+ white-around y) width height)
	       
	       (send dc set-pen orig-pen)
	       (send dc set-brush orig-brush))))])
      (sequence
	(super-init)
	(set-flags (cons 'hard-newline (get-flags)))
	(set-snipclass separator-snipclass))))

  (define make-snip-class
    (lambda (name get-%)
      (let ([ans (make-object
		     (class-asi mred:snip-class%
		       (public
			 [read (lambda (s) 
				 (let ([size-box (box 0)])
				   (send s get size-box)
				   (make-object (get-%) (unbox size-box))))])))])   
	(send* ans 
	       (set-version 1)
	       (set-classname name))
	(send (mred:get-the-snip-class-list) add ans)
	ans)))
  
  (define prompt-snip-class
    (make-snip-class "drscheme:prompt-snip%" 
		     (lambda () prompt-snip%)))
  
  (define equal-snip-class
    (make-snip-class "drscheme:equal-snip%" 
		     (lambda () equal-snip%)))
  
  (define make-snip%
    (lambda (snip-class draw-snip)
      (class mred:snip% ([initial-size 12])
	(inherit get-style set-snipclass set-style)
	(private [allowed-size initial-size])
	(public
	  [write (lambda (s) 
		   (send s put allowed-size))]
	  [copy (lambda () 
		  (let ([s (make-object (object-class this) allowed-size)])
		    (send s set-style (get-style))
		    s))]
	  [get-extent
	   (lambda (dc x y w-box h-box descent-box space-box lspace-box rspace-box)
	     (for-each (lambda (box) (unless (null? box) (set-box! box 2)))
		       (list descent-box space-box lspace-box rspace-box))
	     (set! allowed-size (send (get-style) get-size))
	     (unless (null? w-box)
	       (set-box! w-box allowed-size))
	     (unless (null? h-box)
	       (set-box! h-box allowed-size)))]
	  [draw
	   (let*-values
	       ([(bw?) (< (mred:get-display-depth) 3)]
		[(body-pen) (send mred:the-pen-list find-or-create-pen
				  "BLACK" 0 'solid)]
		[(body-brush) (send mred:the-brush-list find-or-create-brush
				    "BLACK" 'solid)]
		[(shadow-pen shadow-brush)
		 (if bw?
		     (let* ([pen (make-object mred:pen% "BLACK" 0 'solid)]
			    [brush (make-object mred:brush%)]
			    [a (integer->char #b01010101)]
			    [b (integer->char #b10101010)]
			    [bitmap (make-object mred:bitmap%
				      (list a b a b a b a b)
				      8 8 1)])
		       (send* pen
			      (set-colour "BLACK")
			      (set-stipple bitmap))
		       (send* brush
			      (set-colour "BLACK")
			      (set-stipple bitmap))
		       (values pen brush))
		     (values
		      (send mred:the-pen-list find-or-create-pen
			    "GRAY" 0 'solid)
		      (send mred:the-brush-list find-or-create-brush
			    "GRAY" 'solid)))])
	     (lambda (dc x y left top right bottom dx dy drawCaret)
	       (let* ([shadow-size (max 1 (floor (/ allowed-size 10)))]
		      [size (-  allowed-size shadow-size)]
		      [line-width (/ size 3)]
		      [orig-brush (send dc get-brush)]
		      [orig-pen (send dc get-pen)])
		 
		 (send dc set-pen shadow-pen)
		 (send dc set-brush shadow-brush)
		 
		 (draw-snip dc (+ x shadow-size) (+ y shadow-size)
			    size line-width)
		 
		 (send dc set-pen body-pen)
		 (send dc set-brush body-brush)
		 
		 (draw-snip dc x y size line-width)
		 
		 (send dc set-pen orig-pen)
		 (send dc set-brush orig-brush))))])
	(sequence
	  (super-init)
	  (set-snipclass snip-class)))))
  
  (define equal-snip%
    (make-snip%
     equal-snip-class
     (lambda (dc x y size line-width)
       (let ([top (- (+ y (/ size 2)) (* 5/4 line-width))])
	 (send dc draw-rectangle x (+ top (* 3/2 line-width)) size line-width)
	 (send dc draw-rectangle x top size line-width)))))
  
  (define prompt-snip%
    (make-snip%
     prompt-snip-class
     (lambda (dc x y size line-width)
       (send dc draw-rectangle x y line-width size)
       (let ([top-pos (- (+ y (/ size 2)) (/ line-width 2))])
	 (send dc draw-rectangle 
	       x
	       top-pos
	       size
	       line-width))))))
