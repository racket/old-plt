(define bundle-view<%>
  (interface ()
    update))

(define bundle<%>
  (interface ()))

(define bundle%
  (class* object% (bundle<%>) (contents)
    (private
      [views null])
    (public
      [add-view
       (lambda (view)
	 (unless (is-a? view bundle-view<%>)
	   (error 'add-view
		  "first argument must implement bundle-view<%> interface, got: ~e"
		  view))
	 (set! views (cons view views)))]
      [contents-changed
       (lambda ()
	 (for-each (lambda (view) (send view update))
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

;; test
; (define leaf1 (make-object leaf-bundle% '(a)))
; (define leaf2 (make-object leaf-bundle% '(d)))
; (define int1 (make-object node-bundle% 'z (list leaf1 leaf2)))
; (define int2 (make-object node-bundle% 'y (list int1 leaf2)))
; (define int3 (make-object node-bundle% 'x (list int2 leaf2)))
; (printf "~s~n"
; 	(send int3 traverse
; 	      (lambda (c x)
; 		(cond
; 		 [(is-a? c leaf-bundle%)
; 		  (append (send c get-names) x)]
; 		 [(is-a? c node-bundle%)
; 		  (map (lambda (x) (list (send c get-label) x))
; 		       x)]
; 		 [else (error)]))
; 	      null))
; (define bundle (make-object bundle% int3))

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
