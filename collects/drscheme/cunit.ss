
(define drscheme:compound-unit@
  (unit/sig drscheme:compound-unit^
    (import [mred : mred^]
	    [mzlib : mzlib:core^]
	    [drscheme:unit : drscheme:unit^]
	    [drscheme:frame : drscheme:frame^])

    (mred:debug:printf 'invoke "drscheme:compound-unit@")

    (define project-pasteboard%
      (class-asi mred:pasteboard%
	(inherit find-first-snip get-snip-location get-frame get-canvas
		 find-next-selected-snip get-dc find-snip
		 invalidate-bitmap-cache begin-write-header-footer-to-file
		 end-write-header-footer-to-file)
	(rename [super-on-default-event on-default-event]
		[super-on-paint on-paint]
		[super-write-footers-to-file write-footers-to-file]
		[super-read-footer-from-file read-footer-from-file]
		[super-on-move-to on-move-to])
	
	(private
	  [size-footer-string "drscheme:compound-unit:pasteboard%-size"]
	  [children-footer-string "drscheme:compound-unit:pasteboard%-children"])
	(public
	  [read-footer-from-file
	   (lambda (stream footer)
	     (cond
	       [(string=? footer children-footer-string)
		(let* ([s (send stream get-string)]
		       [ht (make-hash-table)]
		       [info (mzlib:string@:read-string s)])
		  (let loop ([snip (find-first-snip)])
		    (unless (null? snip)
		      (hash-table-put! ht (string->symbol (ivar snip name)) snip)
		      (loop (send snip next))))
		  (for-each (lambda (piece)
			      (let/ec k
				(let* ([snip-name (car piece)]
				       [snip (hash-table-get 
					      ht (string->symbol snip-name)
					      (lambda ()
						(wx:message-box (format 
								 "Extraneous snip in file: ~a"
								 snip-name))
						(k #f)))]
				       [snip-children (cadr piece)])
				  (for-each (lambda (child-name)
					      (let/ec k
						(let ([child (hash-table-get
							      ht (string->symbol child-name)
							      (lambda ()
								(wx:message-box
								 (format "Extraneous snip in file: ~a" 
									 child-name))
								(k #f)))])
						  (send snip add-child child)
						  (send child add-parent snip))))
					    snip-children))))
			    info))]
	       [(string=? footer size-footer-string)
		(let* ([s (send stream get-string)]
		       [l (mzlib:string@:read-string s)])
		  '(send (get-frame) set-size -1 -1 (car l) (cadr l))
		  #t)]
	       [else (super-read-footer-from-file stream footer)]))]
	  [write-footers-to-file
	   (lambda (stream)
	     (let* ([b (box 0)]
		    [write-footer
		     (lambda (s t)
		       (dynamic-wind (lambda ()
				       (begin-write-header-footer-to-file stream s b))
				     t
				     (lambda ()
				       (end-write-header-footer-to-file stream (unbox b)))))])
	       (write-footer
		size-footer-string
		(lambda ()
		  (send stream put
			(format "~s" 
				(let ([f (get-frame)])
				  (list (send f get-width)
					(send f get-height)))))))
	       (write-footer
		children-footer-string
		(lambda ()
		  (send stream put
			(format 
			 "~s" 
			 (let loop ([snip (find-first-snip)])
			   (if (null? snip)
			       null
			       (cons (list (ivar snip name)
					   (map (lambda (x) (ivar x name))
						(ivar snip children)))
				     (loop (send snip next)))))))))
	       (super-write-footers-to-file stream)))])

	(public
	 [invalidate-snip&parents&children
	  (lambda (snip)
	    (let ([relatives (append (ivar snip parents) (ivar snip children))]
		  [leftbox (box 0)]
		  [rightbox (box 0)]
		  [topbox (box 0)]
		  [bottombox (box 0)])
	      (get-snip-location snip leftbox topbox #f)
	      (get-snip-location snip topbox bottombox #t)
	      (let loop ([relatives relatives]
			 [left (unbox leftbox)] [top (unbox topbox)]
			 [right (unbox rightbox)] [bottom (unbox bottombox)])
		(cond
		 [(null? relatives)
		  '(printf "invalidating: ~a~n" (list left top right bottom))
		  (invalidate-bitmap-cache left top
					   (- right left) (- bottom top))
		  '(invalidate-bitmap-cache 0 0 -1 -1)]
		 [else 
		  (let ([relative (car relatives)])
		    (get-snip-location relative leftbox topbox #f)
		    (get-snip-location relative rightbox bottombox #t)
		    (loop (cdr relatives)
			  (min (unbox leftbox) left)
			  (min (unbox topbox) top)
			  (max (unbox rightbox) right)
			  (max (unbox bottombox) bottom)))]))))]
	 [on-move-to
	  (lambda (snip x y dragging?)
	    (invalidate-snip&parents&children snip)
	    (super-on-move-to snip x y dragging?))]
	 [on-default-event
	  (let* ([from-x #f]
		 [from-y #f]
		 [last-x 0]
		 [last-y 0]
		 [orig-snip #f]
		 [restore-dc
		  (lambda (f)
		    (let* ([dc (get-dc)]
			   [old-function (send dc get-logical-function)])
		      (f dc)
		      (send dc set-logical-function old-function)))]
		 [restore-drawing-state
		  (lambda ()
		    (restore-dc
		     (lambda (dc)
		       (send dc set-logical-function wx:const-xor)
		       (send dc draw-line from-x from-y last-x last-y)
		       (set! from-x #f)
		       (set! from-y #f))))])
	    (lambda (evt)
	      '(printf "evt: ~a dragging? ~a button-up? ~a button-down? ~a button3 ~a~n"
		       evt (send evt dragging?) (send evt button-up?) (send evt button-down?)
		       (send evt get-right-down))
	      (let ([x (send evt get-x)]
		    [y (send evt get-y)])
		(cond
		 [(send evt button-down? 3)
		  '(printf "dragging.1~n")
		  (let ([s (find-snip x y)])
		    (unless (null? s)
		      (set! orig-snip s)
		      (set! from-x x)
		      (set! from-y y)
		      (set! last-x x)
		      (set! last-y y)))]
		 [(and from-x (send evt dragging?))
		  (restore-dc
		   (lambda (dc)
		     (send dc set-logical-function wx:const-xor)
		     (send dc draw-line from-x from-y last-x last-y)
		     (set! last-x x)
		     (set! last-y y)
		     (send dc draw-line from-x from-y last-x last-y)))]
		 [(and from-x (send evt button-up? 3))
		  (restore-drawing-state)
		  (let ([s (find-snip x y)])
		    (unless (null? s)
		      (send s add-parent orig-snip)
		      (send orig-snip add-child s)))]
		 [else
		  (when from-x
		    (restore-drawing-state))
		  (super-on-default-event evt)]))))]
	 [on-paint
	  (letrec*
	      ([get-center 
		(lambda (x1 y1 x2 y2)
		  (values (/ (+ x1 x2) 2) (/ (+ y1 y2) 2)))]
	       [distance
		(lambda (x1 x2 y1 y2)
		  (sqrt (+ (expt (- y2 y1) 2) (expt (- x2 x1) 2))))]
	       [slope
		(lambda (x1 y1 x2 y2)
		  (if (= x2 x1) 
		      'infinite
		      (/ (- y2 y1)
			 (- x2 x1))))]
	       [find-intersection
		(lambda (left top right bottom x y)
		  '(printf "~nfind-intersection; left: ~a top: ~a right: ~a bottom: ~a x: ~a y: ~a~n"
			  left top right bottom x y)
		  (let*-values ([(cx cy) (get-center left top right bottom)]
				[(m) (slope cx cy x y)])
		    '(printf "x: ~a y: ~a: cx: ~a cy: ~a m: ~a~n" x y cx cy m)
		    (if (eq? 'infinite m)
			(values x
				(if (<= (abs (- x top))
					(abs (- x bottom)))
				    top
				    bottom))
			(let*-values
			    ([(get-x)
			      (lambda (yp)
				(values (+ x (/ (- yp y) m)) yp))]
			     [(get-y)
			      (lambda (xp)
				(values xp (+ y (* m (- xp x)))))]
			     [(hx hy) (get-x (if (<= (abs (- top y))
						     (abs (- bottom y)))
						 top
						 bottom))]
			     [(vx vy) (get-y (if (<= (abs (- right x))
						     (abs (- left x)))
						 right
						 left))]
			     [(dh) (distance hx hy x y)]
			     [(dv) (distance vx vy x y)]
			     [(h-good) (<= left hx right)]
			     [(v-good) (<= top vy bottom)])
			  '(printf "hx: ~a hy: ~a vx: ~a vy: ~a~n" hx hy vx vy)
			  (cond
			    [(and h-good v-good (<= dh dv))
			     '(printf "all good, choosing h~n")
			     (values hx hy)]
			    [(and h-good v-good)
			     `(printf "all good, choosing v~n")
			     (values vx vy)]
			    [(and v-good)
			     '(printf "h no good, choosing v~n")
			     (values vx vy)]
			    [(and h-good)
			     '(printf "v no good, choosing h~n")
			     (values hx hy)]
			    [else 
			     '(printf "otherwise, choosing (0,0)~n")
			     (values 0 0)])))))]
	       [pi 3.1415926535]
	       [move 
		(lambda (d theta x y)
		  (values theta
			  (+ x (* (cos theta) d))
			  (+ y (* (sin theta) d))))]
	       [turn
		(lambda (alpha theta x y)
		  (values (+ alpha theta) x y))]
	       [draw-arrow
		(lambda (dc dx dy x-orig y-orig x2 y2)
		  (let*-values ([(head-width-angle) (* 7/8 pi)]
				[(head-major-length) 20]
				[(head-minor-length) 10]
				[(theta-orig) 
				 (let ([arctangent (atan (/ (- y-orig y2)
							    (- x-orig x2)))])
				   (cond
				     [(and (= x-orig x2) (<= y-orig y2)) (- (/ pi 2))]
				     [(= x-orig x2) (/ pi 2)]
				     [(< x2 x-orig) 
				      '(printf "arctangent.1: ~a~n" arctangent)
				      arctangent]
				     [else
				      '(printf "arctangent.2: ~a~n" arctangent)
				      (+ arctangent pi)]))]
				[(theta x y) (turn head-width-angle theta-orig x-orig y-orig)]
				[(theta x1 y1) (move head-major-length theta x y)]
				
				[(theta x y) (turn pi theta-orig x-orig y-orig)]
				[(theta x2 y2) (move head-minor-length theta x y)]
				
				[(theta x y) (turn (- head-width-angle) theta-orig x-orig y-orig)]
				[(theta x3 y3) (move head-major-length theta x y)]
				
				[(brush) (send dc get-brush)])
		    (send* dc
		      (set-brush (make-object wx:brush% "BLACK" wx:const-solid))
		      (draw-polygon (list (make-object wx:point% x-orig y-orig)
					  (make-object wx:point% x1 y1)
					  (make-object wx:point% x2 y2)					
					  (make-object wx:point% x3 y3))
				    dx 
				    dy)
		      (set-brush brush))))]
	       [get-rectangle 
		(lambda (snip)
		  (let ([x (box 0)]
			[y (box 0)]
			[x2 (box 0)]
			[y2 (box 0)]
			[extra-space 4])
		    (get-snip-location snip x y #f)
		    (get-snip-location snip x2 y2 #t)
		    (values (- (unbox x) extra-space)
			    (- (unbox y) extra-space)
			    (+ (unbox x2) extra-space)
			    (+ (unbox y2) extra-space))))]
	       [draw-self-loop
		(lambda (left top right bottom)
		  (void))])
	    (lambda (before dc left top right bottom dx dy draw-caret)
	      (unless before
		(let* ([draw-children
			(lambda (snip)
			  (let*-values ([(sl st sr sb) (get-rectangle snip)]
					[(scx scy) (get-center sl st sr sb)])
			    (for-each 
			     (lambda (parent)
			       (if (eq? parent snip)
				   (draw-self-loop sl st sr sb)
				   (let*-values ([(pl pt pr pb) (get-rectangle parent)]
						 [(pcx pcy) (get-center pl pt pr pb)]
						 [(i1x i1y) (find-intersection sl st sr sb pcx pcy)]
						 [(i2x i2y) (find-intersection pl pt pr pb scx scy)])
				     '(printf "found: (~a,~a) snip center: (~a,~a)~n" i1x i1y scx scy)
				     (when (and i1x i1y i2x i2y)
				       (draw-arrow dc dx dy i1x i1y i2x i2y)
				       (send dc draw-line (+ i1x dx) (+ i1y dy) (+ i2x dx) (+ i2y dy))))))
			     (ivar snip parents))))])
		  (let loop ([s (find-first-snip)])
		    (unless (null? s)
		      (draw-children s)
		      (loop (send s next))))))
	      (super-on-paint before dc left top right bottom dx dy draw-caret)))]
	 [on-double-click
	  (lambda (snip evt)
	    (let loop ([s (find-next-selected-snip null)])
	      (unless (null? s)
		'(printf "evt: button-d-click? ~a button-down? ~a button-up? ~a
button? 1 ~a button? 2 ~a button? 3 ~a
dragging? ~a entering? ~a get-alt-down ~a get-control-down ~a
get-left-down ~a get-meta-down ~a get-middle-down ~a get-right-down ~a
get-shift-down ~a  get-time-stamp ~a  get-x ~a  get-y ~a  
is-button? ~a  leaving? ~a  moving?~a~n"
			(send evt button-d-click?) (send evt button-down?) (send evt button-up?)
			(send evt button? 1) (send evt button? 2) (send evt button? 3)
			(send evt dragging?) (send evt entering?) (send evt get-alt-down) (send evt get-control-down)
			(send evt get-left-down) (send evt get-meta-down) (send evt get-middle-down) (send evt get-right-down)
			(send evt get-shift-down) (send evt get-time-stamp) (send evt get-x) (send evt get-y)
			(send evt is-button?) (send evt leaving?) (send evt moving?))
		'(printf "get-event-class ~a get-event-object ~a~n get-event-type ~a~n~n"
			(send evt get-event-class)
			(send evt get-event-object)
			(send evt get-event-type))
		(send s open)
		(loop (find-next-selected-snip s)))))])))

    (define frame%
      (class drscheme:frame:frame% (fn [snip #f] [show? #t])
	(inherit show get-edit show-menu panel group)
	(public
	  [on-close
	   (lambda ()
	     (when snip
	       (send snip on-close-frame (send (get-edit) get-filename))))]
	  [filename (if fn
			fn
			"Untitled")])
	  
	(rename [super-make-menu-bar make-menu-bar]
		[super-update-shown update-shown])
	(private
	  [evaluation-order-id #f])
	(public
	  [update-shown
	   (lambda ()
	     (super-update-shown)
	     (send panel change-children
		   (lambda (l)
		     (let ([removed (mzlib:function@:remq eval-panel l)])
		       (if (send show-menu checked? evaluation-order-id)
			   (cons eval-panel removed)
			   removed)))))]
	  [make-menu-bar
	   (lambda ()
	     (let ([mb (super-make-menu-bar)]
		   [add-menu (make-object mred:menu%)])
	       (set! evaluation-order-id
		     (send show-menu append-item
			   "Evaluation Order"
			   (lambda () (update-shown))
			   "Show or hide the evaluation order list"
			   #t))
	       (send show-menu check evaluation-order-id #t)

	       (send mb append add-menu "Add")
	       (send add-menu append-item "Unit..."
		     (lambda ()
		       (let ([name (wx:get-text-from-user "Name of unit" "New Unit")])
			 (unless (null? name)
			   (send (get-edit) insert 
				 (make-object drscheme:unit:snip% name #f))))))
	       (send add-menu append-item "Compound Unit..."
		     (lambda ()
		       (let ([name (wx:get-text-from-user "Name of compound unit"
							  "New Compound Unit")])
			 (unless (null? name)
			   (send (get-edit) insert 
				 (make-object snip% name #f))))))
	       mb))])
	(public
	  [get-panel% (lambda () mred:horizontal-panel%)]
	  [get-edit% (lambda () project-pasteboard%)])

	(sequence
	  (super-init filename snip))
	(private
	  [eval-panel (make-object mred:vertical-panel% panel)]
	  [eval-msg (make-object mred:message% eval-panel "Evaluation Order")]
	  [eval-list (make-object mred:list-box% eval-panel null "")])

	(sequence
	  (update-shown)
	  (send eval-list stretchable-in-x #f)
	  (send eval-panel stretchable-in-x #f)
	  (send (get-edit) set-filename filename)
	  (when (file-exists? filename)
	    (send (get-edit) load-file filename))
	  (send group insert this)
	  (when show?
	    (show #t)))))

    (define snip%
      (let ([f% frame%])
	(class-asi drscheme:unit:snip%
	  (inherit width height)
	  (rename [super-draw draw])
	  (public
	    [snipclassq compound-unit-snipclass]
	    [frame% f%]
	    [this% snip%]
	    [draw
	     (lambda (dc x y left top right bottom dx dy draw-caret)
	       (let ([space 2])
		 (send dc draw-rectangle x y width height)
		 (set! width (- width (* 2 space)))
		 (set! height (- height (* 2 space)))
		 (super-draw dc (+ x space) (+ y space) left top right bottom dx dy draw-caret)
		 (set! width (+ width (* 2 space)))
		 (set! height (+ height (* 2 space)))))]))))

    (define snip-class%
      (let ([s% snip%])
	(class-asi drscheme:unit:snip-class%
	  (public
	    [snip% s%]
	    [version 1]
	    [classname "drscheme:compound-unit:snip%"]))))

    (define compound-unit-snipclass (make-object snip-class%))

    (mred:insert-format-handler "Compound Units"
				(list "cut")
				(opt-lambda ([name null] [group #f])
				  (make-object frame% name group #f)))))


