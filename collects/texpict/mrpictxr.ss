(unit/sig ((open mrpict-extra^)
	   (open texpict-common-setup^))
  (import mred^
	  ((open texpict-common^)
	   (open texpict-internal^)))

  (define dc-for-text-size (make-parameter 
			    #f
			    (lambda (x)
			      (unless (or (not x)
					  (is-a? x dc<%>))
				(raise-type-error 'dc-for-parameter "dc<%> object or #f" x))
			      x)))

  (define (dc f w h a d)
    (make-pict `(prog ,f ,h) w h a d null))
  (define prog-picture dc)

  (define (memq* a l)
    (if (pair? l)
	(or (eq? (car l) a)
	    (memq* a (cdr l)))
	#f))

  (define text
    (case-lambda
     [(string) (text string '() 12)]
     [(string style) (text string style 12)]
     [(string orig-style size)
      (let ([font
	     (let loop ([style orig-style])
	       (cond
		[(null? style) 
		 (send the-font-list find-or-create-font
		       size 'default 'normal 'normal)]
		[(is-a? style font%)
		 style]
		[(memq style '(default decorative roman script swiss modern symbol system))
		 (send the-font-list find-or-create-font
		       size style 'normal 'normal)]
		[(string? style)
		 (send the-font-list find-or-create-font
		       size style 'default 'normal 'normal)]
		[(and (pair? style)
		      (memq (car style)
			    '(superscript 
			      subscript
			      bold italic)))
		 (let ([font (loop (cdr style))]
		       [style (car style)])
		   (cond
		    [(eq? style 'bold)
		     (send the-font-list find-or-create-font
			   (send font get-point-size)
			   (send font get-family)
			   (send font get-style)
			   'bold)]
		    [(eq? style 'italic)
		     (send the-font-list find-or-create-font
			   (send font get-point-size)
			   (send font get-family)
			   'italic
			   (send font get-weight))]
		    [else font]))]
		[else (raise-type-error 'text
					"style"
					orig-style)]))]
	    [sub? (memq* 'subscript orig-style)]
	    [sup? (memq* 'superscript orig-style)])
	(let ([s-font (if (or sub? sup?)
			  (send the-font-list find-or-create-font
				(floor (* 1/2 (send font get-point-size)))
				(send font get-family)
				(send font get-style)
				(send font get-weight))
			  font)]
	      [dc (dc-for-text-size)])
	  (unless dc
	    (error 'text "no dc<%> object installed for sizing"))
	  (let-values ([(w h d s) (send dc get-text-extent string s-font)])
	    (if (or sub? sup?)
		(let-values ([(ww wh wd ws) (send dc get-text-extent "Wy" font)])
		  (prog-picture (lambda (dc x y)
				  (let ([f (send dc get-font)])
				    (send dc set-font s-font)
				    (send dc draw-text string
					  x (if sub?
						(+ y (- wh h))
						y))
				    (send dc set-font f)))
				w wh (- wh wd) wd))
		(prog-picture (lambda (dc x y)
				(let ([f (send dc get-font)])
				  (send dc set-font font)
				  (send dc draw-text string x y)
				  (send dc set-font f)))
			      w h (- h d) d)))))]))

  (define caps-text
    (case-lambda
     [(string) (caps-text string '() 12)]
     [(string style) (caps-text string style 12)]
     [(string style size)
      (let ([strings
	     (let loop ([l (string->list string)][this null][results null][up? #f])
	       (if (null? l)
		   (reverse! (cons (reverse! this) results))
		   (if (eq? up? (char-upper-case? (car l)))
		       (loop (cdr l) (cons (car l) this) results up?)
		       (loop (cdr l) (list (car l)) (cons (reverse! this) results) (not up?)))))]
	    [cap-style
	     (let loop ([s style])
	       (cond
		[(pair? s) (cons (car s) (loop (cdr s)))]
		[(is-a? s font%) (send the-font-list find-or-create-font
				       (floor (* 8/10 (send s get-point-size)))
				       (send s get-family)
				       (send s get-style)
				       (send s get-weight))]
		[else s]))]
	    [cap-size (floor (* 8/10 size))])
	(let ([picts
	       (let loop ([l strings][up? #f])
		 (if (null? l)
		     null
		     (cons (text (list->string (map char-upcase (car l)))
				 (if up? style cap-style)
				 (if up? size cap-size))
			   (loop (cdr l) (not up?)))))])
	  (apply hbl-append 0 picts)))]))

  (define connect
    (case-lambda
     [(x1 y1 x2 y2) (connect x1 y1 x2 y2 #f)]
     [(x1 y1 x2 y2 arrow?) (~connect 'r +inf.0 x1 y1 x2 y2 arrow?)]))

  (define ~connect 
    (case-lambda
     [(exact close-enough x1 y1 x2 y2) (~connect exact close-enough x1 y1 x2 y2 #f)]
     [(exact close-enough x1 y1 x2 y2 arrow?)
      `((put ,x1 ,y1 (,(if arrow? 'vector 'line) ,(- x2 x1) ,(- y2 y1) 1)))]))

  (define (render dc w h l dx dy)
    (define b&w? #f)

    (define draw-line (ivar dc draw-line))
    (define draw-spline (ivar dc draw-spline))
    (define get-pen (ivar dc get-pen))
    (define get-brush (ivar dc get-brush))
    (define set-pen (ivar dc set-pen))
    (define set-brush (ivar dc set-brush))
    (define find-or-create-pen (ivar the-pen-list find-or-create-pen))
    (define find-or-create-brush (ivar the-brush-list find-or-create-brush))

    (set-brush (find-or-create-brush "black" 'solid))

    (let loop ([dx dx][dy dy][l l][color "black"])
      (unless (null? l)
	(let ([x (car l)])
	  (if (string? x)
	      (error 'draw-pict "how did a string get here?: ~s" x)
	      (case (car x)
		[(offset) (loop (+ dx (cadr x))
				(+ dy (caddr x))
				(cadddr x)
				color)]
		[(line vector)
		 (let ([xs (cadr x)]
		       [ys (caddr x)]
		       [len (cadddr x)])
		   (draw-line 
		    dx (- h dy)
		    (+ dx (* xs len)) (- h (+ dy (* ys len)))))]
		[(circle circle*)
		 (let ([size (cadr x)])
		   (send dc draw-ellipse 
			 dx (- h dy size)
			 size size))]
		[(oval)
		 (let ([b (get-brush)])
		   (set-brush (find-or-create-brush "BLACK" 'transparent))
		   (send dc draw-rounded-rectangle
			 (- dx (/ (cadr x) 2))
			 (- h dy (/ (caddr x) 2))
			 (cadr x) (caddr x)
			 -0.2)
		   (set-brush b))]
		[(bezier)
		 (draw-spline (+ dx (list-ref x 1))
			      (- h (+ dy (list-ref x 2)))
			      (+ dx (list-ref x 3))
			      (- h (+ dy (list-ref x 4)))
			      (+ dx (list-ref x 5))
			      (- h (+ dy (list-ref x 6))))]
		[(with-color)
		 (if b&w?
		     (loop dx dy (caddr x) color)
		     (let ([p (get-pen)]
			   [b (get-brush)])
		       (set-pen (find-or-create-pen (cadr x) (send p get-width) 'solid))
		       (set-brush (find-or-create-brush (cadr x) 'solid))
		       (loop dx dy (caddr x) (cadr x))
		       (set-pen p)
		       (set-brush b)))]
		[(with-thickness)
		 (let ([p (get-pen)])
		   (set-pen (find-or-create-pen (send p get-color) 
						(if (eq? (cadr x) 'thicklines)
						    1
						    0)
						'solid))
		   (loop dx dy (caddr x) color)
		   (set-pen p))]
		[(prog)
		 ((cadr x) dc dx (- h dy (caddr x)))]
		[else (error 'rander "unknown command: ~a~n" x)])))
	(loop dx dy (cdr l) color))))

  (define (draw-pict p dc dx dy)
    (render dc (pict-width p) (pict-height p)
	    (pict->command-list p) 
	    dx dy))

  )
