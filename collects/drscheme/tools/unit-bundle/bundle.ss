(define bundle<%>
  (interface ()
    create-view))

(define-struct rect (x y width height))

(define bundle%
  (class* object% (bundle<%>) (contents)
    (private
      [views null]
      [interior-height-addition 40]
      [calculate-tree-size
       (lambda ()
	 (let o-loop ([contents contents])
	   (cond
	    [(is-a? contents leaf-bundle%)
	     (let (;[admin (send (send contents get-editor) get-admin)]
		   [w (box 0)]
		   [h (box 0)])
	       ;(send admin get-view #f #f w h)
	       (set-box! w 30)
	       (set-box! h 30)
	       (send contents set-tree-width (unbox w))
	       (send contents set-tree-height (unbox h))
	       (values (unbox w) (unbox h)))]
	    [(is-a? contents node-bundle%)
	     (let i-loop ([bundle-contents (send contents get-bundle-contents)]
			  [width 0]
			  [height 0])
	       (cond
		[(null? bundle-contents)
		 (send contents set-tree-width width)
		 (send contents set-tree-height (+ height interior-height-addition))
		 (values width
			 (+ height interior-height-addition))]
		[else (let*-values ([(c-width c-height) (o-loop (car bundle-contents))]
				    [(i-width i-height)
				     (i-loop (cdr bundle-contents)
					     (+ c-width width)
					     (max c-height height))])
			(values (+ c-width i-width)
				(max c-height i-height)))]))]
	    [else (error 'position-view-contents "fell off cond: ~e~n" contents)])))]
      [position-snips
       (lambda (view)
	 (let o-loop ([contents-snip (send view get-contents-snip)]
		      [x 0]
		      [y 0])
	   (cond
	    [(is-a? contents-snip leaf-bundle-snip%)
	     (send view move-to contents-snip x y)]
	    [(is-a? contents-snip node-bundle-snip%)

	     ;; set this snips position
	     (let* ([bundle-contents (send contents-snip get-bundle-contents)]
		    [tree-width (send bundle-contents get-tree-width)]
		    [width (send contents-snip get-width)])
	       (send view move-to contents-snip (/ (- tree-width width) 2) y))

	     ;; loop over children
	     (let i-loop ([bundle-contents-snips (send contents-snip get-bundle-contents-snips)]
			  [x x])
	       (cond
		[(null? bundle-contents-snips) (void)]
		[else (let* ([bundle-content-snip (car bundle-contents-snips)]
			     [bundle-content (send bundle-content-snip get-bundle-contents)]

			     [tree-width (send bundle-content get-tree-width)]
			     [tree-height (send bundle-content get-tree-width)])
			(o-loop bundle-content-snip x (+ y interior-height-addition))
			(i-loop (cdr bundle-contents-snips)
				(+ x tree-width)))]))]
	    [else (error 'create-view "fell off cond: ~e~n" contents-snip)])))]
      [build-view-contents
       (lambda (view)
	 (let loop ([contents contents])
	   (cond
	    [(is-a? contents leaf-bundle%)
	     (let ([snip (make-object leaf-bundle-snip% contents)])
	       (send view insert snip)
	       snip)]
	    [(is-a? contents node-bundle%)
	     (let ([snip (make-object node-bundle-snip%
			   contents
			   (map loop (send contents get-bundle-contents)))])
	       (send view insert snip)
	       snip)]
	    [else (error 'create-view "fell off cond: ~e~n" contents)])))])
    (public
      [create-view
       (lambda ()
	 (let ([view (make-object bundle-pasteboard%)])
	   (send view set-contents-snip (build-view-contents view))
	   (calculate-tree-size)
	   (position-snips view)
	   (set! views (cons view views))
	   view))]
      [contents-changed
       (lambda ()
	 (calculate-tree-size)
	 (for-each (lambda (view)
		     (let ([snips 
			    (let loop ([snips (send view get-first-snip)])
			      (if snip
				  (cons snip (loop (send snip next)))
				  null))])
		       (for-each (lambda (snip) (send view delete snip))
				 snips)
		       (build-view-contents view)
		       (position-snips view)))
		   views))])
    (sequence
      (super-init)
      (send contents traverse
	    (lambda (c x) (send c set-bundle this))
	    (void)))))

(define bundle-contents<%>
  (interface ()
    set-bundle
    traverse ; ((bundle-contents A -> A) A -> A)
    ))

(define bundle-contents%
  (class* object% (bundle-contents<%>) ()

    (private
      [tree-width 0]
      [tree-height 0])
    (public
      [get-tree-width (lambda () tree-width)]
      [get-tree-height (lambda () tree-height)]
      [set-tree-width (lambda (w) (set! tree-width w))]
      [set-tree-height (lambda (h) (set! tree-height h))])

    (private
      [bundle #f])
    (public
      [get-bundle
       (lambda ()
	 bundle)]
      [set-bundle
       (lambda (b)
	 (unless (is-a? b bundle<%>)
	   (error 'set-bundle "expected a bundle<%>, got: ~e"
		  b))
	 (set! bundle b))]
      [traverse
       (lambda (f init)
	 (void))])
    (sequence
      (super-init))))
	  
(define leaf-bundle%
  (class bundle-contents% (_names)
    (private
      [names _names])
    (override
     [traverse
      (lambda (f init)
	(f this init))])
    (public
      [get-names
       (lambda ()
	 names)]
      [set-names
       (lambda (n)
	 (unless (and (list? n)
		      (andmap symbol? n))
	   (error 'set-names "expected a list of symbols, got: ~e"
		  n))
	 (set! names n))])
    (sequence
      (super-init)
      (set-names _names))))

(define node-bundle%
  (class bundle-contents% (_label _bundle-contents)
    (override
     [traverse
      (lambda (f init)
	(let loop ([contents bundle-contents]
		   [init init])
	  (cond
	   [(null? contents) (f this init)]
	   [else (loop
		  (cdr contents)
		  (send (car contents)
		       traverse
		       (lambda (object init) (f object init))
		       init))])))])
    (private
      [label _label]
      [bundle-contents _bundle-contents])
    (public
      [get-label
       (lambda ()
	 label)]
      [get-bundle-contents
       (lambda ()
	 bundle-contents)]
      [set-label
       (lambda (s)
	 (unless (symbol? s)
	   (error 'set-names "expected a list of symbols, got: ~e"
		  s))
	 (set! label s))]
      [set-bundle-contents
       (lambda (bc)
	 (unless (and (list? bc)
		      (andmap (lambda (x) (is-a? x bundle-contents<%>)) bc))
	   (error 'set-names "expected a list of symbols, got: ~e" bc))
	 (set! bundle-contents bc))])
    (sequence
      (super-init)
      (set-label _label)
      (set-bundle-contents _bundle-contents))))

(define bundle-pasteboard%
  (class pasteboard% ()
    (private
      [contents-snip #f])
    (public
      [get-contents-snip (lambda () contents-snip)]
      [set-contents-snip (lambda (c) (set! contents-snip c))])

    (inherit get-snip-location)
    (private
      [get-snip-x-location
       (lambda (snip)
	 (let ([xl (box 0)]
	       [xr (box 0)])
	   (get-snip-location snip xl #f #f)
	   (get-snip-location snip xr #f #t)
	   (floor (+ (unbox xl) (/ (- (unbox xr) (unbox xl)) 2)))))]
      [get-snip-top-location
       (lambda (snip)
	 (let ([yt (box 0)])
	   (get-snip-location snip #f yt #f)
	   (unbox yt)))]
      [get-snip-bottom-location
       (lambda (snip)
	 (let ([yb (box 0)])
	   (get-snip-location snip #f yb #t)
	   (unbox yb)))])
    (override
     [on-paint
      (lambda (before? dc left top right bottom dx dy draw-caret)
	(when (and contents-snip
		   (not before?))
	  (let ([pen (send dc get-pen)])
	    (set-dc-pen dc "BLUE" 1 'solid)
	    (let o-loop ([contents-snip contents-snip])
	      (cond
	       [(is-a? contents-snip leaf-bundle-snip%) (void)]
	       [(is-a? contents-snip node-bundle-snip%)
		(let ([x (get-snip-x-location contents-snip)]
		      [y (get-snip-bottom-location contents-snip)])
		  (let i-loop ([bundle-contents-snips
				(send contents-snip get-bundle-contents-snips)])
		    (cond
		     [(null? bundle-contents-snips) (void)]
		     [else
		      (let* ([bundle-content-snip (car bundle-contents-snips)])
			(let ([bx (get-snip-x-location bundle-content-snip)]
			      [by (get-snip-top-location bundle-content-snip)])
			  (send dc draw-line (+ x dx) (+ y dy) (+ bx dx) (+ by dy))
			  (o-loop bundle-content-snip)
			  (i-loop (cdr bundle-contents-snips))))])))]
	       [else (error 'create-view "fell off cond: ~e~n" contents-snip)]))
	    (send dc set-pen pen))))])
    (sequence
      (super-init))))
      
(define leaf-bundle-snip%
  (class editor-snip% (leaf-bundle)
    (public
      [get-bundle-contents
       (lambda ()
	 leaf-bundle)])
    (private
      [text (make-object text%)]
      [update-text
       (lambda ()
	 (send text begin-edit-sequence)
	 (let ([names (send leaf-bundle get-names)])
	   (unless (null? names)
	     (send text insert (symbol->string (car names)))
	     (for-each (lambda (name)
			 (send text insert #\newline)
			 (send text insert (symbol->string name)))
		       (cdr names))))
	 (send text end-edit-sequence))])
    (sequence
      (update-text)
      (super-init text))))
	
(define (set-box/f! b/f contents)
  (printf "set-box/f!: ~s ~s~n" b/f contents)
  (when (box? b/f)
    (set-box! b/f contents)))
(define (set-dc-pen dc color width style)
  (send dc set-pen (send the-pen-list find-or-create-pen color width style)))
(define (set-dc-brush dc color style)
  (send dc set-brush (send the-brush-list find-or-create-brush color style)))

(define node-bundle-snip%
  (class snip% (node-bundle bundle-contents-snips)

    (public
      [get-bundle-contents
       (lambda ()
	 node-bundle)]
      [get-bundle-contents-snips
       (lambda ()
	 bundle-contents-snips)])

    (private
      [width 10]
      [height 10])
    (public
      [get-width (lambda () width)]
      [get-height (lambda () height)])
    (override
     [get-extent
      (lambda (dc x y w h descent ascent space lspace rspace)
	(set-box/f! w width)
	(set-box/f! h height)
	(set-box/f! descent 0)
	(set-box/f! ascent 0)
	(set-box/f! space 0)
	(set-box/f! lspace 0)
	(set-box/f! rspace 0))]
     [draw
      (lambda (dc x y left top right bottom dx dy draw-caret)
	(let ([pen (send dc get-pen)]
	      [brush (send dc get-brush)])
	  (set-dc-pen dc "BLACK" 1 'solid)
	  (set-dc-brush dc "BLACK" 'solid)
	  (send dc draw-ellipse x y width height)
	  (send dc set-pen pen)
	  (send dc set-brush brush)))])
    (sequence
      (super-init))))

;; test
(define leaf1 (make-object leaf-bundle% '(a)))
(define leaf2 (make-object leaf-bundle% '(d)))
(define int1 (make-object node-bundle% 'z (list leaf1 leaf2)))
(define int2 (make-object node-bundle% 'y (list int1 leaf2)))
(define int3 (make-object node-bundle% 'x (list int2 leaf2)))
(printf "~s~n"
 	(send int3 traverse
 	      (lambda (c x)
 		(cond
 		 [(is-a? c leaf-bundle%)
 		  (append (send c get-names) x)]
 		 [(is-a? c node-bundle%)
 		  (map (lambda (x) (list (send c get-label) x))
 		       x)]
 		 [else (error)]))
 	      null))
(define bundle (make-object bundle% int3))
(define frame (make-object frame% "test" #f 400 400))
(define canvas (make-object editor-canvas%
		 frame
		 (send bundle create-view)))
(send frame show #t)

(define arrow-snip-class (make-object snip-class%))
(send arrow-snip-class set-classname "hier-arrow")
(define arrow-snip%
  (class snip% (click-callback)
    (inherit get-admin set-flags get-flags set-count set-snipclass get-style)
    (rename [super-get-extent get-extent])
    (private 
      [size #f]
      [width-fraction 1/2]
      [right-points #f]
      [down-points #f]
      [on? #f]
      [set-sizes
       (lambda (dc)
	 (let* ([s (get-style)]
		[h (send s get-text-height dc)]
		[d (send s get-text-descent dc)]
		[a (send s get-text-space dc)])
	   (set! size (- h d a))
	   (set! arrow-size (+ size 2))
	   (let* ([voffset (floor (/ d 2))]
		  [s (floor (- h d a))]
		  [sz (if (even? s) s (sub1 s))]
		  [offset (ceiling (* (/ (- 1 width-fraction) 2) sz))]
		  [width (floor (* width-fraction sz))])
	     (set! right-points (list (make-object point% offset voffset)
				      (make-object point% offset (+ voffset sz))
				      (make-object point% (+ offset width) (+ voffset (quotient sz 2)))))
	     (set! down-points 
		   (list (make-object point% 0 (+ voffset offset))
			 (make-object point% sz (+ voffset offset))
			 (make-object point% (quotient sz 2) (+ width offset voffset)))))))])
    (private
      [get-width (lambda () (+ 2 size))]
      [get-height (lambda () (+ 2 size))]
      [clicked? #f]
      [update
       (lambda ()
	 (send (get-admin) needs-update this 0 0 (get-width) (get-height)))])
    (override
     [get-extent (lambda (dc x y w h descent space lspace rspace)
		   (super-get-extent dc x y w h descent space lspace rspace)
		   (unless size (set-sizes dc))
		   (when w (set-box! w (get-width)))
		   (when h (set-box! h (get-height)))
		   (when descent (set-box! descent 2))
		   (when space (set-box! space 0)))]
     [partial-offset (lambda (dc x y len)
		       (unless size (set-sizes dc))
		       (if (zero? len)
			   0 
			   (get-width)))]
     [draw (lambda (dc x y left top right bottom dx dy draw-caret)
	     (unless size (set-sizes dc))
	     (let ([b (send dc get-brush)])
	       (send dc set-brush (if clicked? blue red))
	       (let ([points (if on? down-points right-points)])
		 (send dc draw-polygon points x y)
		 (send dc draw-line 
		       (+ x (send (car points) get-x))
		       (+ y (send (car points) get-y))
		       (+ x (send (cadr points) get-x))
		       (+ y (send (cadr points) get-y))))
	       (send dc set-brush b)))]
     [size-cache-invalid (lambda () (set! size #f))]
     [on-event
      (lambda (dc x y mediax mediay event)
	(let ([in-range?
	       (and (<= 0 (- (send event get-x) x) (get-width))
		    (<= 0 (- (send event get-y) y) (get-height)))])
	  (cond
	   [(send event button-down?)
	    (when in-range?
	      (unless clicked?
		(set! clicked? #t)
		(update)))]
	   [(send event button-up?)
	    (when clicked?
	      (set! clicked? #f)
	      (update))
	    (when in-range?
	      (on (not on?))
	      (click-callback this))]
	   [(send event dragging?)
	    (unless (or (and clicked? in-range?)
			(and (not clicked?) (not in-range?)))
	      (set! clicked? (not clicked?))
	      (update))]
	   [else (when clicked?
		   (set! clicked? #f)
		   (update))])))]
     [copy (lambda () (make-object arrow-snip% click-callback))])
    (public
      [on (case-lambda 
	   [(v) (set! on? v) (update)]
	   [() on?])])
    (sequence
      (super-init)
      (set-snipclass arrow-snip-class)
      (set-count 1)
      (set-flags (cons 'handles-events (get-flags))))))

(define arrow-whitespace-snip%
  (class arrow-snip% ()
    (override [draw void])
    (sequence (super-init void))))

(define leaf-bundle%
  (class snip% (names)
    (private
      [width #f])
    (public
      [get-min-width
       (lambda ()
	 10)]
      [set-width
       (lambda (w)
	 (set! width w))])
    (override
     [draw
      (lambda (dc x y left top right bottom dx dy draw-caret)
	(void))])))
