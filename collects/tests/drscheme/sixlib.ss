(require-library "macro.ss")
(require-library "graphic.ss" "graphics")

(open-graphics)

(let ([vp (open-pixmap "snip test" 100 100)])
  ((draw-string vp) (make-posn 20 30) "flipped rect")
  ((flip-solid-rectangle vp) (make-posn 10 10) 80 80)
  (viewport->snip vp))

(let ([v (open-viewport "Tester" 200 200)])
  ((draw-string v) (make-posn 0 20) "Reversed X; click to continue")
  ((draw-string v) (make-posn 0 40) "(busy-waiting right now!)")
  ((draw-line v) (make-posn 0 0) (make-posn 100 100))
  ((draw-line v) (make-posn 100 0) (make-posn 0 100))
  ((flip-viewport v))
  (let loop ()
    (unless (ready-mouse-click v)
      (loop)))

  ((clear-viewport v))
  ((draw-string v) (make-posn 0 20) "Cleared; click")
  (get-mouse-click v)

  (let ([rect-draw
	 (lambda (f)
	   (f (make-posn 20 20) 60 60))]
	[poly-draw
	 (lambda (f)
	   (f (list (make-posn 0 0) (make-posn 40 0) (make-posn 20 40)) (make-posn 20 20)))]
	[string-draw
	 (lambda (f)
	   (f (make-posn 10 20) "XXXXX"))]
	[shape
	 (lambda (do-draw draw clear flip name)
	   ((clear-viewport v))
	   ((draw-string v) (make-posn 0 20) (format "Centered ~s" name))
	   (do-draw (draw v))
	   (get-mouse-click v)

	   ((draw-string v) (make-posn 0 40) (format "Erased ~s" name))
	   (do-draw (clear v))
	   (get-mouse-click v)

	   ((clear-viewport v))
	   ((draw-string v) (make-posn 0 20) (format "Centered ~s" name))
	   (do-draw (draw v))
	   (get-mouse-click v)

	   ((draw-string v) (make-posn 0 40) (format "Flipped ~s" name))
	   (do-draw (flip v))
	   (get-mouse-click v)

	   ((draw-string v) (make-posn 0 40) (format "Flipped ~s back" name))
	   (do-draw (flip v))
	   (get-mouse-click v))])
    (shape rect-draw draw-rectangle clear-rectangle flip-rectangle "box")
    (shape rect-draw draw-solid-rectangle clear-solid-rectangle flip-solid-rectangle "solid box")
    (shape rect-draw draw-ellipse clear-ellipse flip-ellipse "circle")
    (shape rect-draw draw-solid-ellipse clear-solid-ellipse flip-solid-ellipse "solid circle")
    (shape poly-draw draw-polygon clear-polygon flip-polygon "polygon")
    (shape poly-draw draw-solid-polygon clear-solid-polygon flip-solid-polygon "solid polygon")
    (shape string-draw
	   draw-string
	   clear-string
	   flip-string
	   "string"))

  ((clear-viewport v))
  ((draw-string v) (make-posn 0 20) "Done; click")
  (get-mouse-click v)

  (close-viewport v))

(let ([v (open-viewport "Color Tester" 100 200)])
  ((draw-solid-rectangle v) (make-posn 10 10) 80 80 (make-rgb 1 0 0))
  ((draw-solid-ellipse v) (make-posn 10 10) 80 80 (make-rgb 0 1 0))
  ((draw-line v) (make-posn 10 10) (make-posn 90 90) (make-rgb 0 0 1))
  ((draw-string v) (make-posn 10 100) "red rectangle")
  ((draw-string v) (make-posn 10 120) "green ellipse")
  ((draw-string v) (make-posn 10 140) "blue line")
  (get-mouse-click v)

  ((draw-viewport v) (make-rgb 1 0 0))
  ((draw-string v) (make-posn 10 100) "solid red")
  (get-mouse-click v)

  ((draw-viewport v))
  ((clear-string v) (make-posn 10 100) "solid black")
  (get-mouse-click v)

  (close-viewport v))

(local [(define width 500)
	(define height 500)
	(define pixmap-filename (build-path (collection-path "icons") "plt.gif"))
	(define view-port (open-viewport "pixmap tests" width height))
	(define (line)
	  ((draw-line view-port) (make-posn 50 50) (make-posn 450 450)))
	(define (next desc)
	  ((draw-string view-port) (make-posn 0 (- height 50)) desc)
	  ((draw-string view-port) (make-posn 0 (- height 30)) "click to continue")
	  (get-mouse-click view-port)
	  ((clear-viewport view-port)))]
	
  (line)
  (((draw-pixmap-posn pixmap-filename) view-port) (make-posn 0 0))
  (next "draw line then draw-pixmap-posn")

  (line)
  ((draw-pixmap view-port) pixmap-filename (make-posn 0 0))
  (next "pixmap-functions: draw line then draw-pixmap")
  
  (close-viewport view-port))

(let* ([width 100]
       [height 100]
       [vs (open-viewport "viewport source" width height)]
       [vd (open-viewport "viewport dest" width height)])
  
  ((draw-ellipse vs) (make-posn 10 10) 80 80)
  ((draw-string vs) (make-posn 10 30) "Click")
  (get-mouse-click vs)
  (copy-viewport vs vd)
  ((clear-viewport vs))
  ((draw-string vs) (make-posn 10 30) "Cleared")
  (get-mouse-click vd)
  (void))

(close-graphics)

