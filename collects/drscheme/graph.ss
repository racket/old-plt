(unit/sig drscheme:graph^
  (import [fw : framework^]
	  [mred : mred^]
	  [mzlib:string : mzlib:string^]
	  [mzlib:function : mzlib:function^])
	  
  (define WHITE-BRUSH (send mred:the-brush-list find-or-create-brush "WHITE" 'solid))
  (define RED-BRUSH (send mred:the-brush-list find-or-create-brush "RED" 'solid))
  (define BLACK-PEN (send mred:the-pen-list find-or-create-pen "BLACK" 1 'solid))
  (define XOR-PEN (send mred:the-pen-list find-or-create-pen "BLACK" 1 'xor))
  (define BLACK (make-object mred:color% "BLACK"))
  (define WHITE (make-object mred:color% "WHITE"))

  (define make-graph-pasteboard%
    (lambda (super%)
      (class-asi super%
	(inherit find-first-snip get-snip-location get-frame get-canvas
		 find-next-selected-snip get-dc find-snip
		 invalidate-bitmap-cache begin-write-header-footer-to-file
		 end-write-header-footer-to-file)
	
	(rename [super-read-footer-from-file read-footer-from-file]
		[super-write-footers-to-file write-footers-to-file])
	(private
	  [size-footer-string "graph-pasteboard%-size"]
	  [children-footer-string "graph-pasteboard%-children"])
	(public
	  [read-footer-from-file
	   (lambda (stream footer)
	     (cond
	       [(string=? footer children-footer-string)
		(let* ([s (send stream get-string)]
		       [ht (make-hash-table)]
		       [info (mzlib:string:read-from-string s)])
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
						(mred:message-box
						 (format 
						  "Extraneous snip in file: ~a"
						  snip-name))
						(k #f)))]
				       [snip-children (cadr piece)])
				  (for-each (lambda (child-name)
					      (let/ec k
						(let ([child (hash-table-get
							      ht (string->symbol child-name)
							      (lambda ()
								(mred:message-box
								 (format "Extraneous snip in file: ~a" 
									 child-name))
								(k #f)))])
						  (send snip add-child child)
						  (send child add-parent snip))))
					    snip-children))))
			    info))]
	       [(string=? footer size-footer-string)
		(let* ([s (send stream get-string)]
		       [l (mzlib:string:read-from-string s)])
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
			       (cons (list (send snip get-name)
					   (map (lambda (x) (send x get-name))
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
			    (max (unbox bottombox) bottom)))]))))])
	(rename [super-after-delete after-delete]
		[super-on-move-to on-move-to]
		[super-on-default-event on-default-event]
		[super-on-paint on-paint])
	(public
	  [after-delete
	   (lambda (snip)
	     (super-after-delete snip)
	     (let ([parents (ivar snip parents)]
		   [children (ivar snip children)])
	       (for-each (lambda (parent) (send parent remove-child snip))
			 parents)
	       (for-each (lambda (child) (send child remove-parent snip))
			 children)))]
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
		  [old-pen #f]
		  [save-dc
		   (lambda ()
		     (set! old-pen (send (get-dc) get-pen)))]
		  [restore-dc
		   (lambda ()
		     (send (get-dc) set-pen old-pen))]
		  [restore-drawing-state
		   (lambda ()
		     (let ([dc (get-dc)])
		       (save-dc)
		       (send dc set-pen XOR-PEN)
		       (send dc draw-line from-x from-y last-x last-y)
		       (set! from-x #f)
		       (set! from-y #f)
		       (restore-dc)))])
	     (lambda (evt)
	       '(printf "evt: ~a dragging? ~a button-up? ~a button-down? ~a button3 ~a~n"
			evt (send evt dragging?) (send evt button-up?) (send evt button-down?)
			(send evt get-right-down))
	       (let ([x (send evt get-x)]
		     [y (send evt get-y)]
		     [dc (get-dc)])
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
		    (save-dc)
		    (send dc set-pen XOR-PEN)
		    (send dc draw-line from-x from-y last-x last-y)
		    (set! last-x x)
		    (set! last-y y)
		    (send dc draw-line from-x from-y last-x last-y)
		    (restore-dc)]
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
	   (letrec ([get-center 
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
				     (if (<= (abs (- y top))
					     (abs (- y bottom)))
					 top
					 bottom))
			     (let*-values
				 ([(xp) (if (<= (abs (- right x))
						(abs (- left x)))
					    right
					    left)]
				  [(yp) (if (<= (abs (- top y))
						(abs (- bottom y)))
					    top
					    bottom)]
				  [(hx hy) (values (+ x (/ (- yp y) m)) yp)]
				  [(vx vy) (values xp (+ y (* m (- xp x))))]
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
				  '(printf "all good, choosing v~n")
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
		     (let* ([one (make-object mred:point% 0 0)]
			    [two (make-object mred:point% 0 0)]
			    [three (make-object mred:point% 0 0)]					
			    [four (make-object mred:point% 0 0)]
			    [poly (list one two three four)]
			    [head-width-angle (* 7/8 pi)]
			    [head-major-length 20]
			    [head-minor-length 10])
		       (lambda (dc dx dy x-orig y-orig x2 y2)
			 (let*-values ([(theta-orig) 
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
				       [(theta x3 y3) (move head-major-length theta x y)])
			   (send* one 
				  (set-x x-orig)
				  (set-y y-orig))
			   (send* two
				  (set-x x1)
				  (set-y y1))
			   (send* three
				  (set-x x2)
				  (set-y y2))
			   (send* four
				  (set-x x3)
				  (set-y y3))
			   (send dc draw-polygon poly dx dy))))]
		    [get-rectangle 
		     (let ([x (box 0)]
			   [y (box 0)]
			   [x2 (box 0)]
			   [y2 (box 0)])
		       (lambda (snip)
			 (let ([extra-space 4])
			   (get-snip-location snip x y #f)
			   (get-snip-location snip x2 y2 #t)
			   (values (- (unbox x) extra-space)
				   (- (unbox y) extra-space)
				   (+ (unbox x2) extra-space)
				   (+ (unbox y2) extra-space)))))]
		    [draw-self-loop
		     (lambda (left top right bottom)
		       (void))])
	     (lambda (before dc left top right bottom dx dy draw-caret)
	       (unless before
		 (send dc set-pen BLACK-PEN)
		 (send dc set-brush RED-BRUSH)
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
					(send dc draw-line (+ i1x dx) (+ i1y dy) (+ i2x dx) (+ i2y dy))
					(draw-arrow dc dx dy i1x i1y i2x i2y)))))
			      (ivar snip parents))))])
		   (let loop ([s (find-first-snip)])
		     (unless (null? s)
		       (draw-children s)
		       (loop (send s next))))))
	       (super-on-paint before dc left top right bottom dx dy draw-caret)))]))))

  (define graph-pasteboard% (make-graph-pasteboard% fw:pasteboard:backup-autosave%))

  (define get-next-node-snip-name
    (let ([n 0])
      (lambda ()
	(set! n (add1 n))
	(format "node-snip~a" n))))

  ;; this needs to be all fixed up....
  (define node-snip-class%
    (class mred:snip-class% args
      (inherit set-classname set-version)
      (public
	[snip% node-snip%]
	[classname "drscheme:unit:snip%"]
	[version 3]
	[write-header
	 (lambda (p)
	   (send p put "h"))]
	[read-header
	 (lambda (p)
	   (send p get-string (box 0)))]
	[read
	 (lambda (p)
	   (let ([l (mzlib:string:read-from-string (send p get-string null))])
	     (make-object snip% (car l) (cadr l))))])
      (sequence
	(apply super-init args)
	(set-classname classname)
	(set-version version)
	(send (mred:get-the-snip-class-list) add this))))

  (define node-snip-class (make-object node-snip-class%))

  (define make-node-snip%
    (lambda (super%)
      (class super% ()
	(inherit get-admin set-snipclass)
	(rename [super-get-flags get-flags])
	(public
	  [invalidate-to
	   (lambda (c)
	     (let-values ([(left1 top1 right1 bottom1) (get-pos)]
			  [(left2 top2 right2 bottom2) (send c get-pos)])
	       (let ([media (send (get-admin) get-media)]
		     [top (min top1 top2)]
		     [left (min left1 left2)]
		     [bottom (max bottom1 bottom2)]
		     [right (max right1 right2)])
		 (send media invalidate-bitmap-cache
		       left top (- right left) (- bottom top)))))])
	(public
	  [width 70] 
	  [set-width (lambda (v) (set! width v))]
	  [height 30] 
	  [set-height (lambda (v) (set! height v))])

	(public
	  [children null]
	  [parents null]
	  [remove-child
	   (lambda (c)
	     (when (member c children)
	       (set! children (mzlib:function:remq c children))))]
	  [remove-parent
	   (lambda (p)
	     (when (member p parents)
	       (set! parents (mzlib:function:remq p parents))
	       (invalidate-to p)
	       (set-buffer-modified)))]
	  [add-child
	   (lambda (c)
	     (unless (or (eq? c this)
			 (member c children))
	       (set! children (cons c children))
	       (set-buffer-modified)
	       (invalidate-to c)))]
	  [add-parent
	   (lambda (c)
	     (unless (or (member c parents)
			 (eq? c this))
	       (set! parents (cons c parents))
	       (invalidate-to c)
	       (set-buffer-modified)))])
	(private [name #f])
	(public 
	  [set-name! (lambda (x) (set! name x))]
	  [get-name
	   (lambda ()
	     (unless name
	       (set-name! (get-next-node-snip-name)))
	     name)])
	(public
	  [get-pos
	   (lambda ()
	     (let ([media (send (get-admin) get-media)])
	       (let ([lbox (box 0)]
		     [rbox (box 0)]
		     [tbox (box 0)]
		     [bbox (box 0)])
		 (send media get-snip-location this lbox tbox #f)
		 (send media get-snip-location this rbox bbox #t)
		 (values (unbox lbox) (unbox tbox)
			 (unbox rbox) (unbox bbox)))))]
	  [set-buffer-modified
	   (lambda ()
	     (let ([admin (get-admin)])
	       (unless (null? admin)
		 (send (send admin get-media) set-modified #t))))]
	  [write (lambda (s) 
		   (let* ([string (format "~s" (get-name))])
		     (send s put string)))]
	  [min-width 10]
	  [min-height 10]
	  [resize
	   (lambda (w h)
	     (if (and (>= w min-width)
		      (>= h min-height))
		 (begin (set! width w)
			(set! height h)
			(send (get-admin) resized this #t)
			#t)
		 #f))]
	  [draw-border
	   (lambda (dc x y width height)
	     (send dc draw-rectangle x y width height))]
	  [draw
	   (let ([xbox (box 0)]
		 [ybox (box 0)]
		 [wbox (box 0)]
		 [hbox (box 0)])
	     (lambda (dc x y left top right bottom dx dy draw-caret)
	       (let ([name (get-name)])
		 (let-values ([(old-left old-top old-width old-height)
			       (begin (send dc get-clipping-region 
					    xbox ybox wbox hbox)
				      (values (unbox xbox)
					      (unbox ybox)
					      (unbox wbox)
					      (unbox hbox)))]
			      [(text-width text-height)
			       (begin (send dc get-text-extent name wbox hbox)
				      (values (unbox wbox) (unbox hbox)))]
			      [(old-pen) (send dc get-pen)]
			      [(old-brush) (send dc get-brush)]
			      [(old-text-foreground) (send dc get-text-foreground)]
			      [(old-text-background) (send dc get-text-background)])
		   (send dc set-pen BLACK-PEN)
		   (send dc set-brush WHITE-BRUSH)
		   (send dc set-text-foreground BLACK)
		   (send dc set-text-background WHITE)
		   (if (< old-width 0)
		       (send dc set-clipping-region x y width height)
		       (let* ([old-right (+ left old-width)]
			      [old-bottom (+ top old-height)]
			      [new-left (max old-left x)]
			      [new-top (max old-top y)]
			      [new-width (- (min old-right (+ x width)) new-left)]
			      [new-height (- (min old-bottom (+ y height)) new-top)])
			 (send dc set-clipping-region 
			       new-left new-top new-width new-height)))
		   (draw-border dc x y width height)
		   (send dc draw-text name 
			 (+ x (/ (- width text-width) 2))
			 (+ y (/ (- height text-height) 2)))
		   (send dc set-pen old-pen)
		   (send dc set-brush old-brush)
		   (send dc set-text-foreground old-text-foreground)
		   (send dc set-text-background old-text-background)
		   (if (< old-width 0)
		       (send dc destroy-clipping-region)
		       (send dc set-clipping-region 
			     old-left old-top old-width old-height))))))]
	  [get-extent
	   (opt-lambda (dc x y
			   [width-box null] [height-box null]
			   [descent-box null] [space-box null]
			   [lspace-box null] [rspace-box null])
	     (let ([size
		    (lambda (v)
		      (lambda (x)
			(unless (null? x)
			  (set-box! x v))))])
	       ((size width) width-box)
	       ((size height) height-box)
	       (for-each (size 3) 
			 (list descent-box space-box lspace-box rspace-box))))]
	  [snipclass node-snip-class])
	(sequence
	  (super-init)
	  (set-snipclass snipclass)))))
  
  (define node-snip% (make-node-snip% mred:snip%)))