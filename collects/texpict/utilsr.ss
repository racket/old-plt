
(unit/sig mrpict:utils^
  (import mred^ mrpict^)

  ;; Utilities for use with mrpict
  
  (define cons-colorized-picture
    (lambda (p color cmds)
      (cc-superimpose
       p
       (colorize
	(cons-picture
	 (ghost (launder p))
	 cmds)
	color))))

  (define color-frame
    (case-lambda
     [(p color w)
      (cc-superimpose
       p
       (let ([p2 (colorize (frame (ghost (launder p))) color)])
	 (if w
	     (linewidth w p2)
	     p2)))]
     [(p color) (color-frame p color)]))

  ;; Returns three values: pict dx dy
  (define (generic-arrow stem? solid? size angle)
    (values
     (dc
      (lambda (dc x y)
	(define (pt->xform-obj p)
	  (let* ([x (car p)]
		 [y (cadr p)]
		 [d (sqrt (+ (* x x) (* y y)))]
		 [a (atan y x)])
	    (make-object point% 
			 (* d size 1/2 (cos (+ a angle)))
			 (* d size 1/2 (- (sin (+ a angle)))))))
	(let ([b (send dc get-brush)]
	      [p (send dc get-pen)])
	  (send dc set-pen (send the-pen-list
				 find-or-create-pen
				 (send p get-color)
				 0
				 'solid))
	  (send dc set-brush (send the-brush-list
				   find-or-create-brush
				   (send p get-color)
				   (if solid? 'solid 'transparent)))
	  (send dc draw-polygon 
		(map pt->xform-obj
		     (if stem?
			 `((1 0)
			   (0 -1)
			   (0 -1/2)
			   (-1 -1/2)
			   (-1 1/2)
			   (0 1/2)
			   (0 1))
			 `((1 0)
			   (-1 -1)
			   (-1/2 0)
			   (-1 1))))
		(+ x (/ size 2)) (+ y (/ size 2)))
	  (send dc set-brush b)
	  (send dc set-pen p)))
      size size 0 0)
     (- (- 0 (* 1/2 size (cos angle))) (/ size 2))
     (- (+ (* 1/2 size) (- (* 1/2 size (sin angle)))) size)))

  (define (arrow/delta size angle)
    (generic-arrow #t #t size angle))
  (define (arrow size angle)
    (let-values ([(p dx dy) (arrow/delta size angle)])
      p))

  (define (arrowhead/delta size angle)
    (generic-arrow #f #t size angle))
  (define (arrowhead size angle)
    (let-values ([(p dx dy) (arrowhead/delta size angle)])
      p))

  (define (arrow-line dx dy size)
    (let-values ([(a adx ady) (arrow/delta size (atan dy dx))])
      (picture
       0 0
       `((connect 0 0 ,dx ,dy)
	 (place ,(+ dx adx) ,(+ ady dy) ,a)))))

  (define (arrows-line dx dy size)
    (picture
     0 0
     `((place 0 0 ,(arrow-line dx dy size))
       (place ,dx ,dy ,(arrow-line (- dx) (- dy) size)))))

  (define (circle size)
    (dc (lambda (dc x y)
	  (let ([b (send dc get-brush)])
	    (send dc set-brush (send the-brush-list find-or-create-brush
				     "white" 'transparent))
	    (send dc draw-ellipse x y size size)
	    (send dc set-brush b)))
	size size 0 0))

  (define (disk size)
    (dc (lambda (dc x y)
	  (send dc draw-ellipse x y size size))
	size size 0 0))


  (define (cloud w h)
    (dc
     (lambda (dc x y)
       (let ([b (send dc get-brush)]
	     [p (send dc get-pen)])
	 (send dc set-pen (send the-pen-list
				find-or-create-pen
				"white" 0 'transparent))
	 (send dc set-brush (send the-brush-list
				  find-or-create-brush
				  "gray"
				  'solid))
	 (send dc draw-ellipse
	       x (+ y (* 1/4 h))
	       (* 1/2 w) (* 1/2 h))
	 (send dc draw-ellipse
	       (+ x (* 1/5 w)) y
	       (* 3/5 w) (* 2/5 h))
	 (send dc draw-ellipse
	       (+ x (* 1/5 w)) (+ y (* 1/3 h))
	       (* 3/5 w) (* 2/3 h))
	 (send dc draw-ellipse
	       (+ x (* 3/5 w)) (+ y (* 1/4 h))
	       (* 2/5 w) (* 1/3 h))
	 (send dc draw-ellipse
	       (+ x (* 3/5 w)) (+ y (* 1/2 h))
	       (* 2/5 w) (* 1/3 h))

	 (send dc set-brush b)
	 (send dc set-pen p)))
     w h 0 0))

  (define (file-icon w h gray?)
    (dc
     (let* ([sw (lambda (x) (* (/ w 110) x))]
	    [sh (lambda (y) (* (/ h 150) y))]
	    [->pt (lambda (l)
		    (map (lambda (p)
			   (make-object point% 
					(sw (car p))
					(sh (cadr p))))
			 l))])
       (lambda (dc x y)
	 (define p (send dc get-pen))
	 (define b (send dc get-brush))

	 (let ([color (send the-brush-list
			    find-or-create-brush
			    (if gray?
				(make-object color% 200 200 255)
				"white")
			    'solid)])

	   (send dc set-pen (send the-pen-list 
				  find-or-create-pen "black" 
				  (send p get-width)
				  'solid))
	   (send dc set-brush color)
	   
	   (send dc draw-polygon 
		 (->pt '((0 0)
			 (0 150)
			 (110 150)
			 (110 20)
			 (90 0)))
		 x y)

	   (send dc draw-line (+ x (sw 90)) y (+ x (sw 90)) (+ y (sh 20)))
	   (send dc draw-line (+ x (sw 90)) (+ y (sh 20)) (+ x (sw 110)) (+ y (sh 20))))
	 
	 (send dc set-brush b)
	 (send dc set-pen p)))
     w h 0 0))
  )
