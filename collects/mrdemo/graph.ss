
; Demonstrates how to define new kinds of `snips' for drawing arbitary
; graphic objects in editors.

; The snip classes here are loaded by the "editor" sample program,
; which contains "Insert Plain Box" and "Insert Graph" items in its
; "Edit" menu.

; NOTE: When the result of an expression in DrScheme's interactions
; window is a snip, DrScheme copies the snip (by calling its `copy'
; method) and inserts the copy into the interactions window. So these
; classes can be partly tested directly in DrScheme's editor. Cut and
; paste won't work, though, because the snip "class" for marshaling is
; not in DrScheme's implementation domain, where the editor resides.

(module graph mzscheme
  (require (lib "mred.ss" "mred")
	   (lib "class.ss")
	   (lib "string.ss")
	   (all-except "draw.ss" snip-class))

  (provide graph-snip%
	   (rename graph-snip-class snip-class))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; A snip class derived from draw-snip% that plots a function from 0 to
  ;; 1. The function is specified as an S-expression to permit
  ;; marshaling.

  ;; Try (make-object graph-snip% '(lambda (x) (* x x))) in DrScheme.

  (define graph-snip%
    (class* draw-snip% (readable-snip<%>)
      (init-field function-expression)
      (inherit set-snipclass)
      (inherit-field w h)
      (rename [super-draw draw])
      (field [function (eval function-expression)]
	     [x-start 0]
	     [x-end 1]
	     [y-start (function x-start)]
	     [y-end (function x-end)]
	     [lmargin 5] [rmargin 5]
	     [tmargin 5] [bmargin 5])
      (override*
       [draw
	(lambda (dc x y . other)
	  (super-draw dc x y)
	  (let* ([bottom (- (+ h y) bmargin)]
		 [top (+ y tmargin)]
		 [right (- (+ x w) rmargin)]
		 [left (+ x lmargin)]
		 [graph-w (- w lmargin rmargin)]
		 [graph-h (- h tmargin bmargin)]
		 [x-scale (/ (- x-end x-start) graph-w)]
		 [dx x-scale]
		 [y-inv-scale (/ graph-h (- y-end y-start))]
		 [dy (/ y-inv-scale)]
		 [x-to-pos
		  (lambda (x)
		    (+ (/ (- x x-start) x-scale) left))]
		 [y-to-pos
		  (lambda (y)
		    (- bottom (* y-inv-scale (- y y-start))))])

	    (if (<= x-start 0 x-end)
		(let ([x-pos (x-to-pos 0)])
		  (send dc draw-line x-pos bottom x-pos top)))
	    (if (<= y-start 0 y-end)
		(let ([y-pos (- bottom (* (- y-start) y-inv-scale))])
		  (send dc draw-line left y-pos right y-pos)))
	    (let loop ((i 0))
	      (if (< i graph-w)
		  (let* ((x0 (+ x-start (* i x-scale)))
			 (j (y-to-pos (function x0))))
		    (if (and (> j y) (< j bottom))
			(send dc draw-point (+ i left) j))
		    (loop (add1 i)))))))]
       [copy
	(lambda ()
	  (make-object graph-snip% function-expression))]
       [write
	(lambda (stream)
	  (send stream << (expr->string function-expression)))])

      (public*
       [read-one-special
	(lambda (index src line col pos)
	  (values function-expression 1 #t))])

      (super-instantiate (100 100))
      
      (set-snipclass (send (get-the-snip-class-list) find "(lib \"graph.ss\" \"mrdemo\")"))

      (when (= y-start y-end)
	(set! y-start (- y-start 100))
	(set! y-end (+ y-end 100)))
      (when (> y-start y-end)
	(let ((start y-start))
	  (set! y-start y-end)
	  (set! y-end start)))))

  (define graph-snip-class
    (make-object 
     (class snip-class%
       (inherit set-classname)
       (override*
	 [read
	  (lambda (stream)
	    (make-object graph-snip% 
			 (read-from-string (send stream get-string))))])
       (super-instantiate ())
       (set-classname "(lib \"graph.ss\" \"mrdemo\")")))))


