
(module utils mzscheme
  (require (lib "class.ss")
	   (lib "math.ss")
	   (lib "etc.ss")
	   (lib "mred.ss" "mred"))

  (require "mrpict.ss")

  ;; Utilities for use with mrpict
  
  (provide cons-colorized-picture
	   color-frame
	   round-frame
	   color-round-frame
	   color-dash-frame

	   arrow
	   arrowhead
	   arrow-line
	   arrows-line
	   
           ellipse
           filled-ellipse
	   circle
	   disk

	   cloud
	   file-icon
	   jack-o-lantern
	   angel-wing
	   desktop-machine

	   add-line
	   add-arrow-line
	   add-arrows-line

	   bitmap

           find-pen
           find-brush
           
           color-series
           scale-color
	   scale)

  (define (re-pict box naya)
    (let ([w (pict-width box)]
	  [h (pict-height box)]
	  [d (pict-descent box)]
	  [a (pict-ascent box)])
      (make-pict (pict-draw naya)
		 w h
		 a d
		 (list (make-child box 0 0)))))
  
  (define cons-colorized-picture
    (lambda (p color cmds)
      (re-pict
       p
       (cc-superimpose
	p
	(colorize
	 (cons-picture
	  (ghost (launder p))
	  cmds)
	 color)))))

  (define (round-frame p radius)
    (re-pict
     p
     (cc-superimpose
      p
      (let ([w (pict-width p)]
	    [h (pict-height p)])
	(dc (lambda (dc x y)
	      (let ([b (send dc get-brush)])
		(send dc set-brush (send the-brush-list find-or-create-brush
					 "white" 'transparent))
		(send dc draw-rounded-rectangle x y w h radius)
		(send dc set-brush b)))
	    (pict-width p) (pict-height p) 0 0)))))

  ;; FIXME: abstract common part of color-frame, etc.

  (define color-frame
    (case-lambda
     [(p color w)
      (re-pict
       p
       (cc-superimpose
	p
	(let ([p2 (colorize (frame (ghost (launder p))) color)])
	  (if w
	      (linewidth w p2)
	      p2))))]
     [(p color) (color-frame p color #f)]))
  
  (define color-round-frame
    (case-lambda
     [(p radius color w)
      (re-pict
       p
       (cc-superimpose
	p
	(let ([p2 (colorize (round-frame (ghost (launder p)) radius) color)])
	  (if w
	      (linewidth w p2)
	      p2))))]
     [(p radius color) (color-round-frame p radius color #f)]))  

  (define color-dash-frame
    (case-lambda
     [(p seg-length color w)
      (re-pict
       p
       (cc-superimpose
	p
	(let ([p2 (colorize (dash-frame (ghost (launder p)) seg-length) color)])
	  (if w
	      (linewidth w p2)
	      p2))))]
     [(p seg-length color) (color-dash-frame p seg-length color #f)]))  

  ;; Returns three values: pict dx dy
  (define (generic-arrow stem? solid? size angle pen-thickness)
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
     (let ([generic-x (- (- 0 (* 1/2 size (cos angle))) (/ size 2))])
       (case (system-type)
         [(macosx) (+ generic-x (quotient pen-thickness 2))]
         [else generic-x]))
     (let ([generic-y (- (+ (* 1/2 size) (- (* 1/2 size (sin angle)))) size)])
       (case (system-type)
         [(macosx) (- generic-y (quotient pen-thickness 2))]
         [else generic-y]))))

  (define (arrow/delta size angle)
    (generic-arrow #t #t size angle 0))
  (define (arrow size angle)
    (let-values ([(p dx dy) (arrow/delta size angle)])
      p))

  (define (arrowhead/delta pen-thickness size angle)
    (generic-arrow #f #t size angle pen-thickness))
  (define (arrowhead size angle)
    (let-values ([(p dx dy) (arrowhead/delta 0 size angle)])
      p))

  (define (arrow-line dx dy size)
    (let-values ([(a adx ady) (arrowhead/delta 0 size (atan dy dx))])
      (picture
       0 0
       `((connect 0 0 ,dx ,dy)
	 (place ,(+ dx adx) ,(+ ady dy) ,a)))))

  (define (arrows-line dx dy size)
    (picture
     0 0
     `((place 0 0 ,(arrow-line dx dy size))
       (place ,dx ,dy ,(arrow-line (- dx) (- dy) size)))))

  (define (circle size) (ellipse size size))
  
  (define (ellipse width height)
    (dc (lambda (dc x y)
	  (let ([b (send dc get-brush)])
	    (send dc set-brush (send the-brush-list find-or-create-brush
				     "white" 'transparent))
	    (send dc draw-ellipse x y width height)
	    (send dc set-brush b)))
	width height 0 0))

  (define (disk size) (filled-ellipse size size))
  
  (define (filled-ellipse width height)
    (dc (lambda (dc x y)
	  (send dc draw-ellipse x y width height))
	width height 0 0))

  (define cloud
    (case-lambda
     [(w h) (cloud w h "gray")]
     [(w h color)
      (dc
       (lambda (dc x y)
	 (let ([b (send dc get-brush)]
	       [p (send dc get-pen)])
	   (send dc set-pen (send the-pen-list
				  find-or-create-pen
				  "white" 0 'transparent))
	   (send dc set-brush (send the-brush-list
				    find-or-create-brush
				    color
				    'solid))
	   (send dc draw-ellipse
		 x (+ y (* 1/4 h))
		 (* 1/2 w) (* 1/2 h))
	   (send dc draw-ellipse
		 (+ x (* 1/5 w)) y
		 (* 3/5 w) (add1 (* 2/5 h)))
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
       w h 0 0)]))

  (define file-icon
    (opt-lambda (w h gray [fancy? #f])
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

	   (let* ([bg-color (cond
                                 [(or (string? gray) (is-a? gray color%)) gray]
                                 [gray (make-object color% 200 200 255)]
                                 [else "white"])]
                  [line-color (if fancy?
                                  (scale-color 0.6 bg-color)
                                  "black")]
                  [color (send the-brush-list
                               find-or-create-brush
                               bg-color
                               'solid)])

	     (send dc set-pen (send the-pen-list 
				    find-or-create-pen 
                                    line-color
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

             (send dc draw-line (+ x (sw 90)) (+ y 1) (+ x (sw 90)) (+ y (sh 20)))
             (send dc draw-line (+ x (sw 90)) (+ y (sh 20)) (+ x (sw 110) -1) (+ y (sh 20))))
	   
           (send dc set-brush b)
	   (send dc set-pen p)))
       w h 0 0)))

  (define angel-wing
    (opt-lambda (w h left?)
      (dc
       (lambda (dc x y)
	 (let-values ([(sx sy) (send dc get-scale)]
		      [(dx dy) (send dc get-origin)])
	   (let ([nsx (* sx (/ w 54))]
		 [nsy (* sy (/ h 110))])
	     (send dc set-origin (+ dx (* x sx) (* (- 16) nsx)) (+ dy (* y sy) (* (- 20) nsy)))
	     (send dc set-scale nsx nsy)

	     (let ([wing
		    (list
		     
		     (list 70 (+ 50 40)  35 65     20 20)
		     (list 20 20    (- 20 5) (+ 20 30)    (+ 20 5) (+ 20 60))
		     (list (+ 20 5) (+ 20 60)    50 100    70 (+ 50 45))
		     
		     (list 22 70   (- 30 5) (+ 65 30)   (+ 30 5) (+ 65 40))
		     (list (+ 30 5) (+ 65 40)  50 110     70 (+ 50 50))
		     
		     (list 32 102   (- 40 5) (+ 65 50)   (+ 40 5) (+ 65 58))
		     (list (+ 40 5) (+ 65 58)   60  130    70 (+ 50 52)))])
	       (when left?
		 (for-each
		  (lambda (spline)
		    (send dc draw-spline . spline))
		  wing))
	       (unless left?
		 (for-each
		  (lambda (spline)
		    (let-values ([(x1 y1 x2 y2 x3 y3) (apply values spline)])
		      (send dc draw-spline (- 87 x1) y1 (- 86 x2) y2 (- 86 x3) y3)))
		  wing)))

	     (send dc set-origin dx dy)
	     (send dc set-scale sx sy))))
       w h 0 0)))

  (define desktop-machine
    (opt-lambda (s [style null])
      (let ([bm (if (memq 'plt style)
		    (make-object bitmap% (build-path (collection-path "icons") "plt-small-shield.gif"))
		    #f)])
	(dc (lambda (dc x y)
	      (let-values ([(sx sy) (send dc get-scale)]
			   [(dx dy) (send dc get-origin)]
			   [(op) (send dc get-pen)]
			   [(ob) (send dc get-brush)])
		(send dc set-origin (+ dx x (* s sx 10)) (+ dy y (* s sy 15)))
		(send dc set-scale (* sx s) (* sy s))
		
		(let ([gray (send the-brush-list
				  find-or-create-brush
				  "gray"
				  'solid)])
		  (send dc set-brush gray)
		  (send dc draw-polygon (list
					 (make-object point% 10 60)
					 (make-object point% 0 80)
					 (make-object point% 80 80)
					 (make-object point% 100 60)
					 (make-object point% 100 0)
					 (make-object point% 20 0)
					 (make-object point% 10 5))))
		(send dc draw-line 80 80 90 60)
		(send dc draw-rectangle 10 5 80 55)
		(send dc set-brush (send the-brush-list
					 find-or-create-brush
					 "white"
					 'solid))
		(send dc draw-rounded-rectangle 15 10 70 45 5)

		(when (memq 'devil style)
		  (send dc set-font (make-object font% 12 'modern 'normal 'normal))
		  (let-values ([(w h d a) (send dc get-text-extent "101010")])
		    (let ([dx (+ (/ (- 70 w) 2) 15)]
			  [dy (+ (/ (- 45 (* 2 h) 2) 2) 10)])
		      (send dc draw-text "101010" dx dy)
		      (send dc draw-text "010101" dx (+ dy h 2))))
		  
		  (send dc set-brush (send the-brush-list
					   find-or-create-brush
					   "red"
					   'solid))
		  (let ([horn (list
			       (make-object point% 0 17)
			       (make-object point% 2 0)
			       (make-object point% 4 17))])
		    (send dc draw-polygon horn 30 -15)
		    (send dc draw-polygon horn 70 -15))
		  
		  (send dc draw-polygon (list
					 (make-object point% 0 0)
					 (make-object point% 10 2)
					 (make-object point% 0 6))
			115 32)
		  
		  (send dc set-pen (send the-pen-list
					 find-or-create-pen
					 "red"
					 2
					 'solid))
		  (send dc draw-line 101 55 110 55)
		  (send dc draw-spline 110 55   130 50    110  45)
		  (send dc draw-spline 110 45   90 40    115  35))

		(send dc set-origin dx dy)
		(send dc set-scale sx sy)

		(send dc set-pen op)
		(send dc set-brush ob)

		(when (memq 'plt style)
		  (when (send bm ok?)
		    (let ([w (send bm get-width)]
			  [h (send bm get-height)])
		      (send dc draw-bitmap bm 
			    (+ x (/ (- (* s 70) w) 2) (* s 25)) 
			    (+ y (/ (- (* s 45) h) 2) (* s 25))))))))
	(* s 120) (* s 115) 0 0))))

  (define jack-o-lantern
    (opt-lambda (size [pumpkin-color "orange"] [face-color "black"] [stem-color "brown"])
      (dc (lambda (dc x y)
	    (let ([b (send dc get-brush)]
		  [p (send dc get-pen)]
		  [set-brush (lambda (c)
			       (send dc set-brush 
				     (send the-brush-list
					   find-or-create-brush
					   c 'solid)))]
		  [r (make-object region% dc)]
		  [c (send dc get-clipping-region)])
	      (send dc set-pen (send the-pen-list
				     find-or-create-pen
				     "white" 1 'transparent))

	      ;; Stem ----------------------------------------
	      (send r set-arc
		    (+ x (* 0.42 size)) (- y (*  0.2 size))
		    size size
		    (* 0.8 pi) pi)
	      (send dc set-clipping-region r)
	      (set-brush stem-color)
	      (send dc draw-rectangle x y size size)
	      
	      (send r set-arc
		    (+ x (* 0.52 size)) (- y (* 0.1 size))
		    (* 0.8 size) (* 0.8 size)
		    (* 0.49 pi) (* 1.1 pi))
	      (send dc set-clipping-region r)
	      (set-brush "white")
	      (send dc draw-rectangle x y size size)

	      ;; Body ----------------------------------------
	      (send dc set-clipping-region c)
	      (set-brush pumpkin-color)

	      (send dc draw-ellipse 
		    x (+ y (* 0.2 size))
		    (* 0.4 size) (* 0.8 size))
	      (send dc draw-ellipse 
		    (+ x (* 0.6 size)) (+ y (* 0.2 size))
		    (* 0.4 size) (* 0.8 size))

	      (send dc draw-ellipse 
		    (+ x (* 0.2 size)) (+ y (* 0.15 size))
		    (* 0.4 size) (* 0.9 size))
	      (send dc draw-ellipse 
		    (+ x (* 0.4 size)) (+ y (* 0.15 size))
		    (* 0.4 size) (* 0.9 size))

	      ;; Smile ----------------------------------------

	      (send r set-rectangle x (+ y (* 0.4 size)) size (* 0.7 size))
	      (send dc set-clipping-region r)

	      (set-brush face-color)
	      (send dc draw-ellipse
		    (+ x (* 0.15 size)) (+ y (* 0.2 size))
		    (* 0.7 size) (* 0.7 size))
	      
	      (set-brush pumpkin-color)
	      (send dc draw-ellipse
		    (+ x (* 0.15 size)) (sub1 (+ y (* 0.2 size)))
		    (* 0.7 size) (* 0.5 size))
	      (send dc draw-rectangle
		    (+ x (* 0.35 size)) (+ y (* 0.55 size))
		    (* 0.1 size) (* 0.2 size))
	      
	      ;; Eyes ----------------------------------------
	      (send dc set-clipping-region c)
	      (set-brush face-color)

	      (send dc draw-ellipse
		    (+ x (* 0.25 size)) (+ y (* 0.3 size))
		    (* 0.175 size) (* 0.25 size))
	      (send dc draw-ellipse
		    (+ x (* (- 0.75 0.175) size)) (+ y (* 0.3 size))
		    (* 0.175 size) (* 0.25 size))

	      (set-brush pumpkin-color)

	      (send dc draw-polygon
		    (list
		     (make-object point%
				  (* 0.5 size)
				  (* 0.45 size))
		     (make-object point%
				  (* 0.2 size)
				  (* 0.25 size))
		     (make-object point%
				  (* 0.8 size)
				  (* 0.25 size)))
		    x y)
	      
	      (send dc set-brush b)
	      (send dc set-pen p)))
	  size (* 1.1 size) 0 0)))

  (define (-add-line base src find-src dest find-dest thickness color arrow-size arrow2-size)
    (let-values ([(sx sy) (find-src base src)]
                 [(dx dy) (find-dest base dest)])
      (cc-superimpose
       base
       (let ([p (cons-picture
		 (ghost (launder base))
		 `((connect ,sx ,sy ,dx ,dy)
		   ,@(if arrow-size
			 (let-values ([(arrow xo yo)
				       (arrowhead/delta
                                        (or thickness 0)
					arrow-size 
					(atan (- dy sy) 
					      (- dx sx)))])
			   `((place ,(+ dx xo) ,(+ dy yo) ,arrow)))
			 null)
		   ,@(if arrow2-size
			 (let-values ([(arrow xo yo)
				       (arrowhead/delta
                                        (or thickness 0)
					arrow-size 
					(atan (- sy dy) 
					      (- sx dx)))])
			   `((place ,(+ sx xo) ,(+ sy yo) ,arrow)))
			 null)))])
	 (let ([p2 (if thickness
		       (linewidth thickness p)
		       p)])
	   (if color
	       (colorize p2 color)
	       p2))))))

  (define add-line
    (case-lambda
     [(base src find-src dest find-dest)
      (add-line base src find-src dest find-dest #f #f)]
     [(base src find-src dest find-dest thickness)
      (add-line base src find-src dest find-dest thickness #f)]
     [(base src find-src dest find-dest thickness color)
      (-add-line base src find-src dest find-dest thickness color #f #f)]))

  (define add-arrow-line
    (case-lambda
     [(arrow-size base src find-src dest find-dest)
      (add-arrow-line arrow-size base src find-src dest find-dest #f #f)]
     [(arrow-size base src find-src dest find-dest thickness)
      (add-arrow-line arrow-size base src find-src dest find-dest thickness #f)]
     [(arrow-size base src find-src dest find-dest thickness color)
      (-add-line base src find-src dest find-dest thickness color arrow-size #f)]))

  (define add-arrows-line
    (case-lambda
     [(arrow-size base src find-src dest find-dest)
      (add-arrows-line arrow-size base src find-src dest find-dest #f #f)]
     [(arrow-size base src find-src dest find-dest thickness)
      (add-arrows-line arrow-size base src find-src dest find-dest thickness #f)]
     [(arrow-size base src find-src dest find-dest thickness color)
      (-add-line base src find-src dest find-dest thickness color arrow-size arrow-size)]))
  
  (define (bitmap filename)
    (let ([bm (if (filename . is-a? . bitmap%)
		  filename
		  (make-object bitmap% filename))])
      (let ([w (send bm get-width)]
	    [h (send bm get-height)])
	(dc
	 (lambda (dc x y)
	   (send dc draw-bitmap bm x y))
	 w h 0 0))))
  
  (define find-brush
    (opt-lambda (color [style 'solid])
      (send the-brush-list find-or-create-brush color style)))
  (define find-pen
    (opt-lambda (color [size 1] [style 'solid])
      (send the-pen-list find-or-create-pen color size style)))  

  (define (color-series dc steps dstep start-c end-c f pen? brush?)
    (let ([sr (send start-c red)]
          [sg (send start-c green)]
          [sb (send start-c blue)]
          [er (send end-c red)]
          [eg (send end-c green)]
          [eb (send end-c blue)]
          [c (make-object color%)]
          [s (lambda (start end i)
               (floor (+ start (* (- end start) (/ i steps)))))])
      (let loop ([i 0])
        (send c set (s sr er i) (s sg eg i) (s sb eb i))
        (when brush?
          (send dc set-brush (find-brush c)))
        (when pen?
          (send dc set-pen (find-pen c)))
        (f i)
        (unless (= i steps)
          (loop (+ dstep i))))))
  
  (define (scale-color s c)
    (let ([c (if (string? c)
                 (make-object color% c)
                 c)])
      (let ([s (lambda (v)
                 (if (> s 1)
                     (- 255 (inexact->exact (floor (/ (- 255 v) s))))
                     (min 255 (inexact->exact (floor (* v s))))))])
        (make-object color%
          (s (send c red))
          (s (send c green))
          (s (send c blue))))))
  
  (define scale
    (case-lambda
     [(p x-factor y-factor)
      (let ([drawer (make-pict-drawer p)])
	(dc
	 (lambda (dc x y)
	   (define (reset-pen)
	     (let ([p (send dc get-pen)])
	       (send dc set-pen (send the-pen-list
				      find-or-create-pen
				      "white" 1 'transparent))
	       (send dc set-pen p)))
	   (let-values ([(xs ys) (send dc get-scale)])
	     (send dc set-scale (* xs x-factor) (* ys y-factor))
	     (reset-pen)
	     (drawer dc
		     (/ x x-factor)
		     (/ y y-factor))
	     (send dc set-scale xs ys)
	     (reset-pen)))
	 (* (pict-width p) x-factor)
	 (* (pict-height p) y-factor)
	 (* (pict-ascent p) y-factor)
	 (* (pict-descent p) y-factor)))]
     [(p factor) (scale p factor factor)])))
