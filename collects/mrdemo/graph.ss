
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

(require-library "string.ss") ; defines string->expr

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A simple snip class that makes an empty square of a certain
; size. Try (make-object draw-snip% 100 100) in DrScheme to get an
; empty box (100 pixels x 100 pixles) as the result.

(define draw-snip%
  (class snip% (w-in h-in)
    (inherit get-admin set-snipclass set-count)
    (public 
      [w w-in]
      [h h-in])
    (override
     [get-extent  ; called by an editor to get the snip's size
      (lambda (dc x y wbox hbox descentbox spacebox lspacebox rspacebox)
	(when hbox
	  (set-box! hbox h))
	(when wbox
	  (set-box! wbox w))
	(when descentbox
	  (set-box! descentbox 0))
	(when spacebox
	  (set-box! spacebox 0))
	(when rspacebox
	  (set-box! rspacebox 0))
	(when lspacebox
	  (set-box! lspacebox 0)))]
     [draw  ; called by an editor to draw the snip
      (lambda (dc x y . other)
	(let* ((xw (sub1 (+ x w)))
	       (yh (sub1 (+ y h)))
	       (x (add1 x))
	       (y (add1 y)))
	  (send dc draw-line x y xw y)
	  (send dc draw-line xw y xw yh)
	  (send dc draw-line x yh xw yh)
	  (send dc draw-line x y x yh)))]
     [copy  ; clones the snip
      (lambda ()
	(make-object draw-snip% w h))]
     [write  ; marshals the snip to a text stream
      (lambda (stream)
	(send stream << w)
	(send stream << h))]
     [resize  ; called by a pasetboard editor to resize the snip
      (lambda (w-in h-in)
	(set! w w-in)
	(set! h h-in)
	; send resize notification to the editor containing the snip
	(let ([admin (get-admin)])
	  (when admin
	    (send admin resized this #t)))
	#t)])
    (sequence
      (super-init)
      ; Need to set the "class" for unmarshaling from text stream
      (set-snipclass (send (get-the-snip-class-list) find "emptydrawbox"))
      (set-count 1))))

; The snip "class" is used for unmarshaling a snip from a text stream
(define draw-snip-class
  (make-object 
   (class snip-class% ()
     (inherit set-classname)
     (override
       [read
	(lambda (stream)
	  (let ([w-box (box 0)]
		[h-box (box 0)])
	    (send stream >> w-box)
	    (send stream >> h-box)
	    (make-object draw-snip% (unbox w-box) (unbox h-box))))])
     (sequence
       (super-init)
       (set-classname "emptydrawbox")))))

; Register the snip class
(send (get-the-snip-class-list) add  draw-snip-class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A snip class derived from draw-snip% that plots a function from 0 to
; 1. The function is specified as an S-expression to permit
; marshaling.

;  Try (make-object graph-snip% '(lambda (x) (* x x))) in DrScheme.

(define graph-snip%
  (class draw-snip% (function-expression)
    (inherit w h set-snipclass)
    (rename [super-draw draw])
    (public [function (eval function-expression)]
	    [x-start 0]
	    [x-end 1]
	    [y-start (function x-start)]
	    [y-end (function x-end)]
	    [lmargin 5] [rmargin 5]
	    [tmargin 5] [bmargin 5])
    (override
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
	 (send stream << (expr->string functions)))])
    (sequence
      (super-init 100 100)
      (set-snipclass (send (get-the-snip-class-list) find "graph"))
      (when (= y-start y-end)
	(set! y-start (- y-start 100))
	(set! y-end (+ y-end 100)))
      (when (> y-start y-end)
	(let ((start y-start))
	  (set! y-start y-end)
	  (set! y-end start))))))

(define graph-snip-class
  (make-object 
   (class snip-class% ()
     (inherit set-classname)
     (override
       [read
	(lambda (stream)
	  (make-object graph-snip% 
		       (read-from-string (send stream get-string))))])
     (sequence
       (super-init)
       (set-classname "graph")))))

(send (get-the-snip-class-list) add graph-snip-class)

