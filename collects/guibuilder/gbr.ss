
(unit/sig gui-builder^
  (import mzlib:function^
	  mzlib:pretty-print^
	  mzlib:file^
	  (mred : mred^))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Snips
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define GB:SNIP-VERSION 2)
  
  (define make-one-line/callback-edit
    (let ([edit%
	   (class mred:media-edit% (cb)
	     (rename [super-on-change on-change])
	     (inherit get-text last-position)
	     (public
	       [on-change
		(lambda args
		  (apply super-on-change args)
		  (cb (get-text 0 (last-position))))])
	     (sequence
	       (super-init)))]
	  [ignore-return-keymap
	   (let ([k (make-object wx:keymap%)])
	     (send k add-key-function "noop" void)
	     (send k map-function "RETURN" "noop")
	     k)])
      (opt-lambda (parent label cb [v ""])
	(let* ([p (make-object mred:horizontal-panel% parent)]
	       [l (make-object mred:message% p label)]
	       [c (make-object mred:one-line-canvas% p)]
	       [e (make-object edit% cb)])
	  (send c set-media e)
	  (send e insert v)
	  (send e set-auto-set-wrap #f)
	  (send (send e get-keymap) chain-to-keymap ignore-return-keymap #f)
	  e))))
  
  (define make-number-control
    (lambda (parent label value get-min get-max set-v)
      (let* ([p (make-object mred:horizontal-panel% parent)]
	     [l (make-object mred:message% p label)]
	     [vl (make-object mred:message% p "999999")]
	     [set-value
	      (lambda (n)
		(set! value n)
		(send vl set-label (number->string n))
		(set-v n))]
	     [b (make-object mred:button%
			     p
			     (lambda (b e)
			       (let ([v (mred:get-text-from-user 
					 (format "~a, in [~a, ~a]:" label (get-min) (get-max))
					 label
					 (number->string value))])
				 (when v
				   (let ([n (string->number v)])
				     (if (and (integer? n) (exact? n) (>= n (get-min)) (<= n (get-max)))
					 (set-value n)
					 (wx:message-box "Bad value"))))))
			     "Set...")])
	(send vl set-label (number->string value))
	(make-object (class null ()
		       (public
			 [get-val (lambda () value)]
			 [check (lambda () 
				  (when (< value (get-min))
				    (set-value (get-min)))
				  (when (> value (get-max))
				    (set-value (get-max))))]))))))
  
  (define new-name (lambda (base) (symbol->string (gensym base))))
  
  (define (stream-write-list stream l)
    (send stream << (length l))
    (for-each
     (lambda (i)
       (send stream << i))
     l))
  
  (define (stream-read-list stream)
    (let ([n (send stream get-exact)])
      (let loop ([n n])
	(if (zero? n)
	    null
	    (cons (send stream get-string) (loop (sub1 n)))))))
  
  (define gb:snip%
    (class wx:snip% ([lm 5][tm 5][rm 5][bm 5])
      (inherit get-admin set-snipclass set-count)
      (private 
	[need-recalc? #t]
	[prev-min-w 0]
	[prev-min-h 0])
      (public 
	[x 0] [stable-x 0]
	[y 0] [stable-y 0]
	[h (+ lm rm)]
	[w (+ tm bm)]
	[spacing 3]
	[hilited? #f]
	[spacing-+
	 (lambda args
	   (+ (apply + args)
	      (let ([c (let loop ([l args])
			 (cond
			   [(null? l) 0]
			   [(zero? (car l)) (loop (cdr l))]
			   [else (add1 (loop (cdr l)))]))])
		(if (positive? c)
		    (* spacing (sub1 c))
		    0))))]
	[horizontal-child-alignment 2]
	[vertical-child-alignment 2]
	[old-horizontal-child-alignment 2]
	[old-vertical-child-alignment 2]
	[with-border? #f]
	[dialog #f]
	[classname "gb:core"]
	[name (new-name "item")]
	[id #f]
	[original-id #f]
	[original-children-ids #f])
      (public
	(parent #f)
	(pb #f)
	(children null)
	(y-stretch? #t)
	(x-stretch? #t)
	(container? #t)
	(set-id (lambda (x) (set! id x)))
	(set-horizontal-child-alignment
	 (lambda (v) (set! horizontal-child-alignment v)))
	(set-vertical-child-alignment
	 (lambda (v) (set! vertical-child-alignment v)))
	(set-with-border
	 (lambda (v) (set! with-border? v)))

	(get-frame%
	 (lambda ()
	   (class mred:frame% (do-on-close)
	     (inherit show)
	     (public
	       [kind "Panel"])
	     (sequence
	       (super-init null (format "~a Settings" kind) -1 -1 200 10))
	     (private
	       [main (make-object mred:vertical-panel% this)])
	     (public
	       [name-edit (make-one-line/callback-edit main "Scheme Name: "
						       (lambda (txt)
							 (set! name txt))
						       name)]
	       [controls (make-object mred:vertical-panel% main)]
	       [on-close (lambda ()
			   (do-on-close)
			   #t)])
	     (sequence
	       (send controls minor-align-left)
	       (let* ([p (make-object mred:vertical-panel% main)]
		      [make-sc
		       (lambda (name set)
			 (make-object mred:check-box% 
				      p
				      (lambda (c e)
					(set (send c get-value))
					(gb-need-recalc-size))
				      name))]
		      [xsc (make-sc "Can Stretch Horizontally"
				    (lambda (on?) (set! x-stretch? on?)))]
		      [ysc (make-sc "Can Stretch Vertically"
				    (lambda (on?) (set! y-stretch? on?)))])
		 (send p minor-align-left)
		 (send xsc set-value x-stretch?)
		 (send ysc set-value y-stretch?)
		 (let ([p (make-object mred:vertical-panel% p)])
		   (send p stretchable-in-y #f)
		   (make-object mred:button% p
				(lambda (b e) (show #f) (do-on-close))
				"Close")))))))
	
	(gb-add-child 
	 (case-lambda 
	  [(c) (gb-add-child c (length children))]
	  [(c pos)
	   (set! children 
		 (let loop ([l children][p pos])
		   (cond
		     [(or (zero? p) (null? l)) (cons c l)]
		     [else (cons (car l) (loop (cdr l) (sub1 p)))]))) 
	   (when pb
	     (send c gb-install pb this)
	     (send pb insert c x (+ y h)))
	   (gb-need-recalc-size)]))
	(gb-remove-child 
	 (lambda (c) 
	   (set! children (remq c children))
	   (gb-need-recalc-size)))
	
	(gb-need-recalc-size
	 (lambda ()
	   (set! need-recalc? #t)
	   (resized)))
	
	(gb-install
	 (lambda (pb-in parent-in)
	   (set! parent parent-in)
	   (if pb
	       (when parent
		 (send pb set-before this parent))
	       (set! pb pb-in))
	   (set! id (send pb new-id))
	   (for-each
	    (lambda (c)
	      (send pb insert c x (+ y h))
	      (send c gb-install pb this))
	    children)))
	
	(gb-get-child-x-start
	 (lambda (mw mh w h) 
	   0))
	(gb-get-child-y-start
	 (lambda (mw mh w h)
	   (if (or (= vertical-child-alignment 1)
		   (ormap (lambda (c) (ivar c y-stretch?)) children))
	       0
	       (case vertical-child-alignment
		 [(2) (quotient (- h mh) 2)]
		 [(3) (- h mh)]))))
	(gb-combine-child-width max)
	(gb-combine-child-height spacing-+)
	
	(gb-compute-child-x-pos
	 (lambda (dc c w)
	   (if (ivar c x-stretch?)
	       0
	       (case horizontal-child-alignment
		 [(2) (let-values ([(cw ch) (send c gb-get-min-size dc)])
			 (/ (- w cw) 2))]
		 [(1) 0]
		 [(3) (let-values ([(cw ch) (send c gb-get-min-size dc)])
			 (- w cw))]))))
	(gb-compute-child-y-pos
	 (lambda (dc c h)
	   0))
	(gb-compute-child-width
	 (lambda (dc c w xsc dw)
	   (if (ivar c x-stretch?)
	       w
	       (let-values ([(cw ch) (send c gb-get-min-size dc)])
		 cw))))
	(gb-compute-child-height
	 (lambda (dc c h ysc dh)
	   (let-values ([(cw ch) (send c gb-get-min-size dc)])
	     (if (ivar c y-stretch?)
		 (+ ch (/ dh ysc))
		 ch))))
	
	(gb-combine-child-x-offset (lambda (a b) a))
	(gb-combine-child-y-offset spacing-+)
	
	(gb-get-min-size
	 (lambda (dc)
	   (let loop ([lw 0][lh 0][l children])
	     (cond
	       [(null? l) (let ([w (+ lw lm rm)]
				[h (+ lh tm bm)])
			    (set! prev-min-w w)
			    (set! prev-min-h h)
			    (values w h))]
	       [else
		(let ([c (car l)])
		  (let-values ([(cw ch) (send c gb-get-min-size dc)])
		    (loop (gb-combine-child-width lw cw) 
			  (gb-combine-child-height lh ch) 
			  (cdr l))))]))))
	(gb-set-shape
	 (lambda (dc x-in y-in w-in h-in)
	   (let*-values ([(xsc) (apply + (map 
					  (lambda (c) (if (ivar c x-stretch?) 1 0))
					  children))] 
			 [(ysc) (apply + (map 
					  (lambda (c) (if (ivar c y-stretch?) 1 0))
					  children))]
			 [(mw mh) (gb-get-min-size dc)]
			 [(ew eh) (values (- w-in lm rm) (- h-in tm bm))]
			 [(dw dh) (values (- w-in mw) (- h-in mh))])
	     (let loop ([lx (+ lm x-in (gb-get-child-x-start mw mh w-in h-in))]
			[ly (+ tm y-in (gb-get-child-y-start mw mh w-in h-in))]
			[l children])
	       (cond
		 [(null? l) 0]
		 [else
		  (let ([c (car l)])
		    (let-values ([(cw ch)
				  (send c gb-set-shape dc
					(+ lx (gb-compute-child-x-pos dc c ew))
					(+ ly (gb-compute-child-y-pos dc c eh))
					(gb-compute-child-width dc c ew xsc dw)
					(gb-compute-child-height dc c eh ysc dh))])
		      (loop (gb-combine-child-x-offset lx cw) 
			    (gb-combine-child-y-offset ly ch) 
			    (cdr l))))])))
	   (set! x x-in)
	   (set! y y-in)
	   (set! w w-in)
	   (set! h h-in)
	   (resized)
	   (when pb
	     (send pb move-to this x-in y-in))
	   (values w-in h-in)))
	
	(find-position-<
	 (lambda (fx fy cx cy)
	   (< fy cy)))
	(gb-find-position
	 (lambda (fx fy)
	   (let loop ([l children][pos 0])
	     (if (null? l)
		 pos
		 (let*-values ([(c) (car l)]
			       [(cx) (send c gb-get-x)]
			       [(cy) (send c gb-get-y)]
			       [(w h) (send c gb-get-size)])
		   (if (find-position-< fx fy (+ cx w) (+ cy h))
		       pos
		       (loop (cdr l) (add1 pos))))))))
	(gb-get-child-pos
	 (lambda (c)
	   (let loop ([l children][pos 0])
	     (cond
	       [(null? l) pos]
	       [(eq? (car l) c) pos]
	       [else (loop (cdr l) (add1 pos))]))))
	
	(gb-get-saved-min-size
	 (lambda ()
	   (values prev-min-w prev-min-h)))
	
	(gb-recalc-size
	 (lambda (dc)
	   (if parent
	       (send parent gb-recalc-size dc)
	       (let-values ([(mw mh) (gb-get-min-size dc)]
			    [(xb) (box 0)]
			    [(yb) (box 0)])
		 (when pb (send pb get-snip-location this xb yb #f))
		 (gb-set-shape dc (unbox xb) (unbox yb) 
			       (max w mw)
			       (max h mh))))))
	
	(gb-hilite
	 (lambda (on?)
	   (unless (eq? on? hilited?)
	     (set! hilited? on?)
	     (refresh))))
	
	(gb-get-parent
	 (lambda () parent))
	(gb-get-children
	 (lambda () children))
	(gb-get-size
	 (lambda () (values w h)))
	(gb-get-x (lambda () x))
	(gb-get-y (lambda () y))
	(gb-get-stable-x (lambda () stable-x))
	(gb-get-stable-y (lambda () stable-y))
	(gb-get-position-and-size
	 (lambda () (values x y w h)))
	
	(gb-set-stable-position
	 (lambda ()
	   (set! stable-x x)
	   (set! stable-y y)))
	
	(gb-drag-children-along
	 (lambda (new-x new-y)
	   (when (not (and (= x new-x) (= y new-y)))
	     (for-each
	      (lambda (c)
		(let ([cx (+ new-x (- (send c gb-get-stable-x) stable-x))]
		      [cy (+ new-y (- (send c gb-get-stable-y) stable-y))])
		  (send pb move-to c cx cy)
		  (send c gb-drag-children-along cx cy)))
	      children)
	     (set! x new-x)
	     (set! y new-y))))
	
	(gb-open-dialog
	 (lambda ()
	   (if dialog
	       (send dialog show #t)
	       (let ([f (make-object (get-frame%) (lambda () (set! dialog #f)))])
		 (set! dialog f)
		 (send f show #t)))))
	
	(gb-reconnect-to-original-children
	 (lambda ()
	   (if original-children-ids
	       (let ([sl (map
			  (lambda (id) (send pb find-snip-by-original-id id))
			  original-children-ids)])
		 (set! original-children-ids #f)
		 (for-each
		  (lambda (s)
		    (when s
		      (gb-add-child s)
		      (send pb remove-selected s)))
		  sl)
		 #t)
	       #f)))
	(gb-forget-original-id
	 (lambda ()
	   (set! original-id #f)
	   (set! original-children-ids #f)))
	
	(gb-local-instantiate
	 (lambda (p)
	   `(let ([v (make-object mred:vertical-panel% ,p)])
	      (send v ,(case horizontal-child-alignment
			 [(1) 'minor-align-left]
			 [(2) 'minor-align-center]
			 [(3) 'minor-align-right]))
	      (send v ,(case vertical-child-alignment
			 [(1) 'major-align-top]
			 [(2) 'major-align-center]
			 [(3) 'major-align-bottom]))
	      v)))
	(gb-aux-instantiate
	 (lambda ()
	   null))
	(gb-instantiate
	 (lambda (parent)
	   (let ([v (gb-local-instantiate parent)]
		 [name (string->symbol name)])
	     `([,name (let ([v ,v])
			(send v stretchable-in-x ,x-stretch?)
			(send v stretchable-in-y ,y-stretch?)
			v)]
	       ,@(gb-aux-instantiate)
	       ,@(apply append
			(map (lambda (c) (send c gb-instantiate name)) children))))))
	
	(get-extent
	 (lambda (dc x y wbox hbox descentbox spacebox
		     lspacebox rspacebox)
	   (when need-recalc?
	     (set! need-recalc? #f)
	     (gb-recalc-size dc))
	   (if (not (null? hbox))
	       (set-box! hbox h))
	   (if (not (null? wbox))
	       (set-box! wbox w))
	   (if (not (null? descentbox))
	       (set-box! descentbox 0))
	   (if (not (null? spacebox))
	       (set-box! spacebox 0))
	   (if (not (null? rspacebox))
	       (set-box! rspacebox 0))
	   (if (not (null? lspacebox))
	       (set-box! lspacebox 0))))
	(draw
	 (lambda (dc x y . other)
	   (draw-box dc x y w h)
	   (when (or with-border? hilited?)
	     (draw-box dc (add1 x) (add1 y) (- w 2) (- h 2)))
	   (when (and with-border? hilited?)
	     (draw-box dc (+ 2 x) (+ 2 y) (- w 4) (- h 4)))))
	(draw-box
	 (lambda (dc x y w h)
	   (let* ((xw (sub1 (+ x w)))
		  (yh (sub1 (+ y h)))
		  (x (add1 x))
		  (y (add1 y)))
	     (send dc draw-line x y xw y)
	     (send dc draw-line xw y xw yh)
	     (send dc draw-line x yh xw yh)
	     (send dc draw-line x y x yh))))
	
	(base-setup
	 (lambda (xs? ys? nw nh hca vca wb? id children-ids)
	   (set! x-stretch? xs?)
	   (set! y-stretch? ys?)
	   (set! w nw)
	   (set! h nh)
	   (set! horizontal-child-alignment hca)
	   (set! vertical-child-alignment vca)
	   (set! with-border? wb?)
	   (set! original-id id)
	   (set! original-children-ids children-ids)))
	(copy
	 (lambda ()
	   (let ([o (make-object (object-class this) lm tm rm bm)])
	     (send o base-setup x-stretch? y-stretch? w h 
		   horizontal-child-alignment
		   vertical-child-alignment
		   with-border?
		   (or original-id id)
		   (or original-children-ids 
		       (and (pair? children)
			    (map (lambda (child) (ivar child id)) children))))
	     o)))
	(write
	 (lambda (stream)
	   (send stream << (if x-stretch? 1 0))
	   (send stream << (if y-stretch? 1 0))
	   (send stream << (inexact->exact w))
	   (send stream << (inexact->exact h))
	   (send stream << horizontal-child-alignment)
	   (send stream << vertical-child-alignment)
	   (send stream << (if with-border? 1 0))
	   (send stream << id)
	   (stream-write-list stream (map (lambda (c) (ivar c id)) children))))
	(read
	 (lambda (stream version)
	   (base-setup
	    (positive? (send stream get-exact))
	    (positive? (send stream get-exact))
	    (send stream get-exact) ; w
	    (send stream get-exact) ; h
	    (if (>= version 2) (send stream get-exact) horizontal-child-alignment) ; hca
	    (if (>= version 2) (send stream get-exact) vertical-child-alignment) ; vca
	    (if (>= version 2) (positive? (send stream get-exact)) #f) ; with-border?
	    (send stream get-string)
	    (let ([v (stream-read-list stream)])
	      (if (null? v) #f v)))))
	(refresh
	 (lambda ()
	   (let ([admin (get-admin)])
	     (unless (null? admin)
	       (send admin needs-update this 0 0 w h)))))
	(resized
	 (lambda ()
	   (let ([admin (get-admin)])
	     (unless (null? admin)
	       (send admin resized this #t)))))
	(resize 
	 (lambda (w-in h-in)
	   (if (not parent) 
	       (let-values ([(mw mh) (values prev-min-w prev-min-h)])
		 (if (or (and (> w-in mw) x-stretch?)
			 (and (> h-in mh) y-stretch?))
		     (begin
		       (when x-stretch? (set! w (max mw w-in)))
		       (when y-stretch? (set! h (max mh h-in)))
		       (gb-need-recalc-size)
		       #t)
		     #f))
	       #f))))
      (sequence
	(super-init)
	(set-snipclass (send (wx:get-the-snip-class-list) find classname))
	(set-count 1))))
  
  (define register-class
    (lambda (class% classname)
      (let ([snipclass
	     (make-object 
	      (class wx:snip-class% ()
		(inherit set-classname set-version)
		(public
		  [read
		   (lambda (stream)
		     (let ([o (make-object class%)]
			   [scl (wx:get-the-snip-class-list)])
		       (send o read stream (send scl reading-version this))
		       o))])
		(sequence
		  (super-init)
		  (set-classname classname)
		  (set-version GB:SNIP-VERSION))))])
	(send (wx:get-the-snip-class-list) add snipclass))))
  
  (register-class gb:snip% "gb:core")
  
  (define gb:make-panel-params-snip%
    (lambda (cl)
      (class-asi cl
	(inherit horizontal-child-alignment set-horizontal-child-alignment
		 vertical-child-alignment  set-vertical-child-alignment
		 with-border? set-with-border
		 gb-need-recalc-size)
	(rename [super-get-frame% get-frame%])
	(public
	  [get-frame%
	   (lambda ()
	     (class (super-get-frame%) args
	       (inherit controls)
	       (sequence
		 (apply super-init args))
	       (public
		[border-check
		 (make-object mred:check-box% controls
			      (lambda (c e)
				(set-with-border (send c get-value))
				(gb-need-recalc-size))
			      "Use Border")])
	       (sequence
		 (send controls set-label-position wx:const-vertical))
	       (public
		 [hca-radio
		  (make-object mred:radio-box% controls 
			       (lambda (r e)
				 (set-horizontal-child-alignment
				  (add1 (send r get-selection)))
				 (gb-need-recalc-size))
			       "Horizontal Align Children: "
			       -1 -1 -1 -1
			       '("Left" "Center" "Right")
			       0 wx:const-horizontal)]
		 [vca-radio
		  (make-object mred:radio-box% controls 
			       (lambda (r e)
				 (set-vertical-child-alignment
				  (add1 (send r get-selection)))
				 (gb-need-recalc-size))
			       "Vertical Align Children: "
			       -1 -1 -1 -1
			       '("Top" "Center" "Bottom")
			       0 wx:const-horizontal)])
	       (sequence
		 (send controls set-label-position wx:const-horizontal))
	       (sequence
		 (send hca-radio set-selection (sub1 horizontal-child-alignment))
		 (send vca-radio set-selection (sub1 vertical-child-alignment))
		 (send border-check set-value with-border?))))]))))

  (define gb:vertical-panel-snip% 
    (class-asi (gb:make-panel-params-snip% gb:snip%)
      (public
        [old-vertical-child-alignment 1]
	[classname "gb:vertical-panel"]
	[name (new-name "vpanel")])))
  
  (register-class gb:vertical-panel-snip% "gb:vertical-panel")

  ; Used by top-level panel:
  (define gb:panel-snip%
    (class-asi gb:vertical-panel-snip%
      (public
	[classname "gb:panel"])))
  
  (register-class gb:panel-snip% "gb:panel")
  

  (define gb:horizontal-panel-snip%
    (class-asi (gb:make-panel-params-snip% gb:snip%)
      (inherit spacing-+ horizontal-child-alignment vertical-child-alignment
	       children)
      (private
	(sp-+ (lambda args (apply spacing-+ args))))
      (public
        [old-horizontal-child-alignment 1]
	[classname "gb:horizontal-panel"]
	[name (new-name "hpanel")]

	(gb-get-child-x-start
	 (lambda (mw mh w h)
	   (if (or (= horizontal-child-alignment 1)
		   (ormap (lambda (c) (ivar c x-stretch?)) children))
	       0
	       (case horizontal-child-alignment
		 [(2) (quotient (- w mw) 2)]
		 [(3) (- w mw)]))))
	(gb-get-child-y-start
	 (lambda (mw mh w h) 
	   0))

	(gb-combine-child-width sp-+)
	(gb-combine-child-height max)
	
	(gb-compute-child-x-pos
	 (lambda (dc c w)
	   0))
	(gb-compute-child-y-pos
	 (lambda (dc c h)
	   (if (ivar c y-stretch?)
	       0
	       (case vertical-child-alignment
		 [(2) (let-values ([(cw ch) (send c gb-get-min-size dc)])
			 (/ (- h ch) 2))]
		 [(1) 0]
		 [(3) (let-values ([(cw ch) (send c gb-get-min-size dc)])
			 (- h ch))]))))
	(gb-compute-child-width
	 (lambda (dc c w xsc dw)
	   (let-values ([(cw ch) (send c gb-get-min-size dc)])
	     (if (ivar c x-stretch?)
		 (+ cw (/ dw xsc))
		 cw))))
	(gb-compute-child-height
	 (lambda (dc c h ysc dh)
	   (if (ivar c y-stretch?)
	       h
	       (let-values ([(cw ch) (send c gb-get-min-size dc)])
		 ch))))
	
	(gb-combine-child-x-offset sp-+)
	(gb-combine-child-y-offset (lambda (a b) a))
	
	(find-position-<
	 (lambda (fx fy cx cy)
	   (< fx cx)))
	
	[gb-local-instantiate
	 (lambda (p)
	   `(let ([v (make-object mred:horizontal-panel% ,p)])
	      (send v ,(case horizontal-child-alignment
			 [(1) 'major-align-left]
			 [(2) 'major-align-center]
			 [(3) 'major-align-right]))
	      (send v ,(case vertical-child-alignment
			 [(1) 'minor-align-top]
			 [(2) 'minor-align-center]
			 [(3) 'minor-align-bottom]))
	      v))])))

  (register-class gb:horizontal-panel-snip% "gb:horizontal-panel")
  
  (define gb:atomic-snip%
    (class-asi gb:snip%
      (public
	(x-stretch? #f)
	(y-stretch? #f)
	(container? #f))))
  
  (define gb:make-text-label-snip%
    (lambda (cl deflabel)
      (class-asi cl
	(inherit get-style gb-need-recalc-size)
	(rename [super-get-frame% get-frame%]
		[super-copy copy]
		[super-write write]
		[super-read read])
	(public
	  [get-frame%
	   (lambda ()
	     (class (super-get-frame%) args
	       (inherit controls)
	       (public
		 [kind deflabel])
	       (sequence
		 (apply super-init args))
	       (public
		 [label-buffer (make-one-line/callback-edit controls "Label: "
							    (lambda (txt)
							      (set! label txt)
							      (gb-need-recalc-size))
							    label)])))]
	  [label deflabel]
	  [get-label
	   (lambda ()
	     label)]
	  [get-label-size
	   (lambda (dc)
	     (let ([xb (box 0.0)]
		   [yb (box 0.0)])
	       (send dc get-text-extent label xb yb null null
		     (send (get-style) get-font))
	       (values (unbox xb) (unbox yb))))]
	  [draw-label
	   (lambda (dc x y)
	     (send dc draw-text label x y))]
	  
	  [label-install
	   (lambda (n)
	     (set! label n))]
	  [copy
	   (lambda ()
	     (let ([o (super-copy)])
	       (send o label-install label)
	       o))]
	  [write
	   (lambda (stream)
	     (super-write stream)
	     (send stream << label))]
	  [read
	   (lambda (stream version)
	     (super-read stream version)
	     (label-install (send stream get-string)))]))))
  
  (define gb:make-message-snip%
    (lambda (cl cn)
      (class-asi cl
	(inherit w h get-label get-label-size draw-label)
	(public
	  [classname cn]
	  [name (new-name "message")]
	  [gb-get-min-size
	   (lambda (dc)
	     (get-label-size dc))]
	  [draw
	   (lambda (dc x y . other)
	     (draw-label dc x y))]
	  [gb-local-instantiate
	   (lambda (p)
	     `(make-object mred:message% ,p ,(get-label)))]))))
  
  (define gb:message-snip% (gb:make-message-snip%
			    (gb:make-text-label-snip% gb:atomic-snip% 
						      "Message")
			    "gb:message"))
  
  (register-class gb:message-snip% "gb:message")
  
  (define gb:make-callback-snip%
    (lambda (cl)
      (class-asi cl
	(inherit name)
	(rename [super-gb-aux-instantiate gb-aux-instantiate])
	(public
	  [callback-kinds (list "-callback")]
	  [callback-code (map (lambda (x) 'void) callback-kinds)]
	  [get-callback-names
	   (lambda ()
	     (map
	      (lambda (ct)
		(string->symbol (string-append name ct)))
	      callback-kinds))]
	  [gb-aux-instantiate
	   (lambda ()
	     (append
	      (map (lambda (n c) `[,n ,c]) (get-callback-names) callback-code)
	      (super-gb-aux-instantiate)))]))))
  
  (define gb:make-button-snip%
    (lambda (cl cn)
      (class-asi cl
	(inherit w h get-label get-label-size get-callback-names draw-label)
	(public
	  [classname cn]
	  [name (new-name "button")]
	  [m 5]
	  [gb-get-min-size
	   (lambda (dc)
	     (let-values ([(x y) (get-label-size dc)])
	       (values (+ (* 2 m) x) (+ (* 2 m) y))))]
	  [draw
	   (lambda (dc x y . other)
	     (send dc draw-rounded-rectangle x y w h 3)
	     (let-values ([(lw lh) (get-label-size dc)])
	       (draw-label dc 
			   (+ (+ x m) (/ (- w lw (* 2 m)) 2))
			   (+ (+ y m) (/ (- h lh (* 2 m)) 2)))))]
	  [gb-local-instantiate
	   (lambda (p)
	     `(make-object mred:button% ,p (lambda (b e) (,(car (get-callback-names)) b e)) ,(get-label)))]))))
  
  (define gb:make-check-box-snip%
    (lambda (cl cn)
      (class-asi cl
	(inherit w h get-style get-label get-callback-names get-label-size draw-label)
	(public
	  [classname cn]
	  [name (new-name "checkbox")]
	  [hspace 2]
	  [boxsize 12]
	  [gb-get-min-size
	   (lambda (dc)
	     (let-values ([(x y) (get-label-size dc)])
	       (values (+ boxsize hspace x) (max boxsize y))))]
	  [draw
	   (lambda (dc x y . other)
	     (let-values ([(lx ly) (get-label-size dc)])
	       (send dc draw-rectangle x (+ y (/ (- h boxsize) 2)) boxsize boxsize)
	       (draw-label dc (+ x boxsize hspace) (+ y (/ (- h ly) 2)))))]
	  [gb-local-instantiate
	   (lambda (p)
	     `(make-object mred:check-box% ,p (lambda (c e) (,(car (get-callback-names)) c e)) ,(get-label)))]))))
  
  (define gb:button-snip% (gb:make-button-snip%
			   (gb:make-callback-snip%
			    (gb:make-text-label-snip% gb:atomic-snip% 
						      "Button"))
			   "gb:button"))
  
  (define gb:check-box-snip% (gb:make-check-box-snip%
			      (gb:make-callback-snip%
			       (gb:make-text-label-snip% gb:atomic-snip% 
							 "Checkbox"))
			      "gb:checkbox"))
  
  (register-class gb:button-snip% "gb:button")
  (register-class gb:check-box-snip% "gb:checkbox")
  
  (define gb:make-item-list-snip%
    (lambda (cl)
      (class-asi cl
	(inherit gb-need-recalc-size)
	(rename [super-get-frame% get-frame%]
		[super-copy copy]
		[super-write write]
		[super-read read])
	(private
	  [delete
	   (lambda (l p)
	     (let loop ([l l][p p])
	       (cond
		 [(null? l) l]
		 [(zero? p) (cdr l)]
		 [else (cons (car l) (loop (cdr l) (sub1 p)))])))])
	(public
	  [get-frame%
	   (lambda ()
	     (class (super-get-frame%) args
	       (inherit controls)
	       (sequence
		 (apply super-init args))
	       (public
		 [user-item (lambda (v)
			      (mred:get-text-from-user "Item name: " "List Item Name" v))]
		 [items-panel (make-object mred:vertical-panel% controls)]
		 [items-list (make-object mred:list-box% items-panel 
					  (lambda (l e)
					    (when (= 2 (send e get-extra-long))
					      (let ([pos (send items-list get-selection)])
						(unless (negative? pos)
						  (let ([v (user-item (list-ref items pos))])
						    (when v
						      (send items-list set-string pos v)
						      (set-car! (list-tail items pos) v)
						      (gb-need-recalc-size)))))))
					  "Items: " 
					  wx:const-single -1 -1 -1 -1
					  items)]
		 [item-buttons-panel (let ([v (make-object mred:horizontal-panel% items-panel)])
				       (send v stretchable-in-x #f)
				       v)]
		 [add-item (make-object mred:button% item-buttons-panel
					(lambda (b e)
					  (let ([v (user-item (format "Item~a" (send items-list number)))])
					    (when v
					      (send items-list append v)
					      (set! items (append items (list v)))
					      (gb-need-recalc-size))))
					"Add Item")]
		 [delete-item (make-object mred:button% item-buttons-panel
					   (lambda (b e)
					     (let* ([lsb (box null)])
					       (unless (zero? (send items-list get-selections lsb))
						 (let loop ([ls (reverse (unbox lsb))])
						   (unless (null? ls)
						     (send items-list delete (car ls))
						     (set! items (delete items (car ls)))
						     (loop (cdr ls))))
						 (gb-need-recalc-size))))
					   "Delete Item")])))]
	  [get-items
	   (lambda ()
	     items)]
	  [init-items null]
	  [get-item-height
	   (lambda (dc)
	     (let ([w (box 0)]
		   [h (box 0)])
	       (send dc get-text-extent "Xj" w h)
	       (unbox h)))]
	  [get-max-item-width
	   (lambda (dc)
	     (let loop ([l items][mw 0])
	       (if (null? l)
		   mw
		   (let ([w (box 0)]
			 [h (box 0)])
		     (send dc get-text-extent (car l) w h)
		     (loop (cdr l) (max mw (unbox w)))))))]
	  
	  [items-install
	   (lambda (l)
	     (set! items l))]
	  [copy
	   (lambda ()
	     (let ([o (super-copy)])
	       (send o items-install items)
	       o))]
	  [write
	   (lambda (stream)
	     (super-write stream)
	     (stream-write-list stream items))]
	  [read
	   (lambda (stream version)
	     (super-read stream version)
	     (items-install (stream-read-list stream)))])
	(private
	  [items init-items]))))
  
  (define gb:make-text-labelled-snip%
    (lambda (cl deflabel)
      (class-asi (gb:make-text-label-snip% cl deflabel)
	(inherit get-label-size w h draw-label gb-need-recalc-size)
	(rename [super-get-frame% get-frame%]
		[super-copy copy]
		[super-write write]
		[super-read read])
	(private
	  [hmargin 2])
	(public
	  [get-frame%
	   (lambda ()
	     (class (super-get-frame%) args
	       (inherit controls)
	       (sequence
		 (apply super-init args))
	       (public
		 [direction-radio
		  (make-object mred:radio-box% controls 
			       (lambda (r e)
				 (set! vertical-label? (zero? (send direction-radio get-selection)))
				 (gb-need-recalc-size))
			       "Label Position: "
			       -1 -1 -1 -1
			       '("Top" "Left")
			       0 wx:const-horizontal)])
	       (sequence
		 (send direction-radio set-selection (if vertical-label? 0 1)))))]
	  [vertical-label? #f]
	  [get-min-body-size
	   (lambda (dc)
	     (values 0 0))]
	  [gb-get-min-size
	   (lambda (dc)
	     (let-values ([(x y) (get-label-size dc)]
			  [(x2 y2) (get-min-body-size dc)]
			  [(+x +y) (if vertical-label?
				       (values max +)
				       (values (lambda (a b) (+ a b hmargin)) max))])
	       (values (+x x x2) (+y y y2))))]
	  [draw
	   (lambda (dc x y . other)
	     (draw-label dc x y)
	     (let*-values ([(lw lh) (get-label-size dc)]
			   [(dx dy) (if vertical-label?
					(values 0 lh)
					(values (+ lw hmargin) 0))])
	       (let ([cx (box 0)][cy (box 0)][cw (box 0)][ch (box 0)])
		 (send dc get-clipping-region cx cy cw ch)
		 (send dc set-clipping-region (+ x dx) (+ y dy) (- w dx) (- h dy))
		 (draw-body dc (+ x dx) (+ y dy) (- w dx) (- h dy))
		 (if (positive? (unbox cw))
		     (send dc set-clipping-region (unbox cx) (unbox cy) (unbox cw) (unbox ch))
		     (send dc destroy-clipping-region)))))]
	  [draw-body
	   (lambda (dc x y w h)
	     (void))]
	  [wrap-label-direction
	   (lambda (p e)
	     `(begin
		(send ,p set-label-position 
		      ,(if vertical-label? 'wx:const-vertical 'wx:const-horizontal))
		,e))]
	  
	  [labelpos-install
	   (lambda (vert?)
	     (set! vertical-label? vert?))]
	  [copy
	   (lambda ()
	     (let ([o (super-copy)])
	       (send o labelpos-install vertical-label?)
	       o))]
	  [write
	   (lambda (stream)
	     (super-write stream)
	     (send stream << (if vertical-label? 1 0)))]
	  [read
	   (lambda (stream version)
	     (super-read stream version)
	     (labelpos-install (positive? (send stream get-exact))))]))))
  
  (define gb:make-list-box-snip%
    (lambda (cl cn)
      (class-asi cl
	(inherit w h get-callback-names get-items get-item-height
		 wrap-label-direction get-label)
	(public
	  [classname cn]
	  [name (new-name "listbox")]
	  [y-stretch? #t]
	  [x-stretch? #t]
	  [min-body-width 50]
	  [min-item-count 3]
	  [get-min-body-size
	   (lambda (dc)
	     (let ([y (get-item-height dc)])
	       (values min-body-width (* min-item-count y))))]
	  [draw-body
	   (lambda (dc x y w h)
	     (send dc draw-rectangle x y w h)
	     (let ([ih (get-item-height dc)])
	       (let loop ([l (get-items)][iy y])
		 (unless (or (>= iy (+ y h)) (null? l))
		   (send dc draw-text (car l) x iy)
		   (loop (cdr l) (+ iy ih))))))]
	  [callback-kinds (list "-select-callback" "-deselect-callback" "-double-select-callback")]
	  [gb-local-instantiate
	   (lambda (p)
	     (wrap-label-direction
	      p `(make-object mred:list-box% ,p 
			      ,(let-values ([(sel desel dbl) (apply values (get-callback-names))])
				 `(lambda (b e) 
				    (case (send e get-extra-long)
				      [(0) (,desel b e)]
				      [(1) (,sel b e)]
				      [(2) (,dbl b e)])))
			      ,(get-label)
			      wx:const-single -1 -1 -1 -1 ',(get-items))))]))))
  
  
  (define gb:list-box-snip% (gb:make-list-box-snip%
			     (gb:make-item-list-snip%
			      (gb:make-callback-snip%
			       (gb:make-text-labelled-snip% gb:atomic-snip% 
							    "List")))
			     "gb:listbox"))
  
  (register-class gb:list-box-snip% "gb:listbox")
  
  
  (define gb:make-layout-snip%
    (lambda (cl)
      (class-asi cl
	(inherit gb-need-recalc-size)
	(rename [super-get-frame% get-frame%]
		[super-copy copy]
		[super-write write]
		[super-read read])
	(public
	  [get-frame%
	   (lambda ()
	     (class (super-get-frame%) args
	       (inherit controls)
	       (sequence
		 (apply super-init args))
	       (public
		 [layout-direction-radio
		  (make-object mred:radio-box% controls 
			       (lambda (r e)
				 (set! vertical-layout? (zero? (send layout-direction-radio get-selection)))
				 (gb-need-recalc-size))
			       "Layout: "
			       -1 -1 -1 -1
			       '("Vertical" "Horizontal")
			       0 wx:const-horizontal)])
	       (sequence
		 (send layout-direction-radio set-selection (if vertical-layout? 0 1)))))]
	  [vertical-layout? #t]
	  
	  [layout-install
	   (lambda (vert?)
	     (set! vertical-layout? vert?))]
	  [copy
	   (lambda ()
	     (let ([o (super-copy)])
	       (send o labelpos-install vertical-layout?)
	       o))]
	  [write
	   (lambda (stream)
	     (super-write stream)
	     (send stream << (if vertical-layout? 1 0)))]
	  [read
	   (lambda (stream version)
	     (super-read stream version)
	     (layout-install (positive? (send stream get-exact))))]))))
  
  (define gb:make-radio-box-snip%
    (lambda (cl cn)
      (class-asi cl
	(inherit w h get-item-height get-max-item-width get-callback-names get-items
		 wrap-label-direction get-label gb-need-recalc-size vertical-layout?)
	(rename [super-get-frame% get-frame%])
	(public
	  [classname cn]
	  [name (new-name "radiobox")]
	  [init-items (list "First" "Second")]
	  [circle-size 10]
	  [margin 2]
	  [get-min-body-size
	   (lambda (dc)
	     (let ([h (max (get-item-height dc) circle-size)]
		   [w (get-max-item-width dc)]
		   [l (length (get-items))])
	       (let-values ([(x-l y-l) (if vertical-layout?
					   (values 1 l)
					   (values l 1))])
		 (values (* (+ circle-size margin w) x-l)
			 (* h y-l)))))]
	  [draw-body
	   (lambda (dc x y w h)
	     (let ([ih (max (get-item-height dc) circle-size)]
		   [iw (+ (get-max-item-width dc) circle-size margin)])
	       (let loop ([l (get-items)][iy y][ix x])
		 (unless (null? l)
		   (send dc draw-ellipse ix (+ iy (/ (- ih circle-size) 2)) circle-size circle-size)
		   (send dc draw-text (car l) (+ circle-size margin ix) iy)
		   (if vertical-layout?
		       (loop (cdr l) (+ iy ih) ix)
		       (loop (cdr l) iy (+ ix iw)))))))]
	  [gb-local-instantiate
	   (lambda (p)
	     (wrap-label-direction
	      p `(make-object mred:radio-box% ,p (lambda (b e) (,(car (get-callback-names)) b e)) 
			      ,(get-label)
			      -1 -1 -1 -1 ',(get-items) 0
			      ,(if vertical-layout? 'wx:const-vertical wx:const-horizontal))))]))))
  
  
  (define gb:radio-box-snip% (gb:make-radio-box-snip%
			      (gb:make-item-list-snip%
			       (gb:make-layout-snip%
				(gb:make-callback-snip%
				 (gb:make-text-labelled-snip% gb:atomic-snip% 
							      "Radiobox"))))
			      "gb:radiobox"))
  
  (register-class gb:radio-box-snip% "gb:radiobox")
  
  
  (define gb:make-choice-snip%
    (lambda (cl cn)
      (class-asi cl
	(inherit w h get-item-height get-max-item-width get-callback-names get-items
		 wrap-label-direction gb-need-recalc-size get-label)
	(public
	  [classname cn]
	  [name (new-name "choice")]
	  [init-items (list "First")]
	  [arrow-size 10]
	  [lmargin 2]
	  [amargin 2]
	  [rmargin 2]
	  [arrow (list (make-object wx:point% 0 0)
		       (make-object wx:point% arrow-size 0)
		       (make-object wx:point% (quotient arrow-size 2) (quotient arrow-size 2)))]
	  [get-min-body-size
	   (lambda (dc)
	     (let ([h (get-item-height dc)]
		   [w (get-max-item-width dc)])
	       (values (+ lmargin arrow-size amargin w rmargin 3) (+ 3 h))))]
	  [draw-body
	   (lambda (dc x y w h)
	     (send dc draw-rectangle x y (sub1 w) (sub1 h))
	     (send dc draw-line (sub1 (+ x w)) (add1 y) (sub1 (+ x w)) (+ y h))
	     (send dc draw-line (add1 x) (sub1 (+ y h)) (+ x w) (sub1 (+ y h)))
	     (send dc draw-polygon arrow (+ 1 lmargin x) (+ y (/ (- h (/ arrow-size 2)) 2)))
	     (let ([l (get-items)])
	       (unless (null? l)
		 (send dc draw-text (car l) (+ 1 lmargin arrow-size amargin x) (add1 y)))))]
	  [gb-local-instantiate
	   (lambda (p)
	     (wrap-label-direction
	      p `(make-object mred:choice% ,p (lambda (b e) (,(car (get-callback-names)) b e)) 
			      ,(get-label)
			      -1 -1 -1 -1 ',(get-items))))]))))
  
  
  (define gb:choice-snip% (gb:make-choice-snip%
			   (gb:make-item-list-snip%
			    (gb:make-callback-snip%
			     (gb:make-text-labelled-snip% gb:atomic-snip% 
							  "Choice")))
			   "gb:choice"))
  
  (register-class gb:choice-snip% "gb:choice")
  
  
  (define gb:make-slider-snip%
    (lambda (cl cn)
      (class-asi cl
	(inherit get-label get-callback-names wrap-label-direction gb-need-recalc-size )
	(rename [super-get-frame% get-frame%]
		[super-copy copy]
		[super-write write]
		[super-read read])
	(public
	  [get-frame%
	   (lambda ()
	     (class (super-get-frame%) args
	       (inherit controls)
	       (sequence
		 (apply super-init args))
	       (public
		 [min-val (make-number-control controls "Minimum: " 0 (lambda () -10000) (lambda () 10000) 
					       (lambda (x) 
						 (set! min-value x) 
						 (send max-val check)
						 (send init-val check)
						 (gb-need-recalc-size)))]
		 [max-val (make-number-control controls "Maximum: " 10 (lambda () (send min-val get-val)) (lambda () 10000)
					       (lambda (x)
						 (set! max-value x)
						 (send init-val check)
						 (gb-need-recalc-size)))]
		 [init-val (make-number-control controls "Initial: " 0 (lambda () (send min-val get-val)) 
						(lambda () (send max-val get-val))
						(lambda (x)
						  (set! init-value x)
						  (gb-need-recalc-size)))])))]
	  [classname cn]
	  [name (new-name "slider")]
	  [vertical-layout? #f]
	  [init-value 0]
	  [min-value 0]
	  [max-value 10]
	  [arrow-size 10]
	  [height arrow-size]
	  [line-height 3]
	  [min-width 50]
	  [darrow (list (make-object wx:point% 0 0)
			(make-object wx:point% arrow-size 0)
			(make-object wx:point% (quotient arrow-size 2) (quotient arrow-size 2)))]
	  [rarrow (list (make-object wx:point% 0 0)
			(make-object wx:point% 0 arrow-size)
			(make-object wx:point% (quotient arrow-size 2) (quotient arrow-size 2)))]
	  [get-min-body-size
	   (lambda (dc)
	     (if vertical-layout?
		 (values height min-width)
		 (values min-width height)))]
	  [draw-body
	   (lambda (dc x y w h)
	     (let ([percent (/ (- init-value min-value) (- max-value min-value))])
	       (if vertical-layout?
		   (begin
		     (send dc draw-rectangle 
			   (+ x (/ arrow-size 2)) (+ y (/ arrow-size 2))
			   line-height (- h arrow-size))
		     (send dc draw-polygon rarrow x (+ y (* percent (- h arrow-size)))))
		   (begin
		     (send dc draw-rectangle 
			   (+ x (/ arrow-size 2)) (+ y (/ arrow-size 2)) 
			   (- w arrow-size) line-height)
		     (send dc draw-polygon darrow (+ x (* percent (- w arrow-size))) y)))))]
	  [gb-local-instantiate
	   (lambda (p)
	     (wrap-label-direction
	      p `(make-object mred:slider% ,p (lambda (b e) (,(car (get-callback-names)) b e)) 
			      ,(get-label) ,init-value ,min-value ,max-value
			      -1 -1 -1
			      ,(if vertical-layout? 'wx:const-vertical wx:const-horizontal))))]
	  
	  [slider-install
	   (lambda (mn mx in)
	     (set! min-value mn)
	     (set! max-value mx)
	     (set! init-value in))]
	  [copy
	   (lambda ()
	     (let ([o (super-copy)])
	       (send o slider-install min-value max-value init-value)
	       o))]
	  [write
	   (lambda (stream)
	     (super-write stream)
	     (send stream << min-value)
	     (send stream << max-value)
	     (send stream << init-value))]
	  [read
	   (lambda (stream version)
	     (super-read stream version)
	     (slider-install (send stream get-exact)
			     (send stream get-exact)
			     (send stream get-exact)))]))))
  
  (define gb:slider-snip% (gb:make-slider-snip%
			   (gb:make-layout-snip%
			    (gb:make-callback-snip%
			     (gb:make-text-labelled-snip% gb:atomic-snip% 
							  "Slider")))
			   "gb:slider"))
  
  (register-class gb:slider-snip% "gb:slider")
  
  (define gb:make-gauge-snip%
    (lambda (cl cn)
      (class-asi cl
	(inherit get-label wrap-label-direction gb-need-recalc-size)
	(rename [super-get-frame% get-frame%]
		[super-copy copy]
		[super-write write]
		[super-read read])
	(public
	  [get-frame%
	   (lambda ()
	     (class (super-get-frame%) args
	       (inherit controls)
	       (sequence
		 (apply super-init args))
	       (public
		 [max-val (make-number-control controls "Maximum: " 10 (lambda () 1) (lambda () 10000) 
					       (lambda (x) 
						 (set! max-value x) 
						 (gb-need-recalc-size)))])))]
	  [classname cn]
	  [name (new-name "gauge")]
	  [vertical-layout? #f]
	  [max-value 10]
	  [min-height 10]
	  [min-width 50]
	  [get-min-body-size
	   (lambda (dc)
	     (if vertical-layout?
		 (values min-height min-width)
		 (values min-width min-height)))]
	  [draw-body
	   (lambda (dc x y w h)
	     (send dc draw-rectangle x y w h)
	     (let ([b (send dc get-brush)])
	       (send dc set-brush (send wx:the-brush-list find-or-create-brush "BLACK" wx:const-solid))
	       (send dc draw-rectangle 
		     x (if vertical-layout? (+ y (* 0.75 h)) y)
		     (if vertical-layout? w (* 0.25 w)) (if vertical-layout? (* 0.25 h) h))
	       (send dc set-brush b)))]
	  [gb-local-instantiate
	   (lambda (p)
	     (wrap-label-direction
	      p `(make-object mred:gauge% ,p
			      ,(get-label) ,max-value
			      -1 -1 -1 -1
			      ,(if vertical-layout? 'wx:const-vertical wx:const-horizontal))))]
	  
	  
	  [gauge-install
	   (lambda (mx)
	     (set! max-value mx))]
	  [copy
	   (lambda ()
	     (let ([o (super-copy)])
	       (send o gauge-install max-value)
	       o))]
	  [write
	   (lambda (stream)
	     (super-write stream)
	     (send stream << max-value))]
	  [read
	   (lambda (stream version)
	     (super-read stream version)
	     (gauge-install (send stream get-exact)))]))))
  
  (define gb:gauge-snip% (gb:make-gauge-snip%
			  (gb:make-layout-snip%
			   (gb:make-text-labelled-snip% gb:atomic-snip% 
							"Gauge"))
			  "gb:gauge"))
  
  (register-class gb:gauge-snip% "gb:gauge")
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buffer (Pasteboard)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  ; INVARIANT: If a snip is selected, then no ancestor or
  ;  decendent of the snip can be selected. Otherwise, the
  ;  dragging rules get complicated (perhaps impossible).
  
  ; INVARIANT: a child must be ordered before its parent in the
  ;  pasteboard. Not only does this affect drawing, but it also
  ;  affects how select-all and rubber-banding work due to the
  ;  ancestor/decendent-selection-exclusion rule.
  
  (define gb:edit%
    (class mred:backup-autosave-pasteboard% ()
      (inherit set-selected find-next-selected-snip insert
	       find-first-snip is-selected? add-selected remove-selected
	       get-admin find-snip begin-edit-sequence end-edit-sequence
	       get-snip-location delete erase set-modified)
      (rename [super-interactive-adjust-move interactive-adjust-move]
	      [super-interactive-adjust-resize interactive-adjust-resize]
	      [super-on-interactive-move on-interactive-move]
	      [super-after-interactive-move after-interactive-move]
	      [super-after-interactive-resize after-interactive-resize]
	      [super-on-default-event on-default-event]
	      [super-after-delete after-delete]
	      [super-after-insert after-insert]
	      [super-do-paste do-paste]
	      [super-do-copy do-copy])
      (private 
	[dragging? #f]
	[pasting? #f]
	[copying? #f]
	[cur-hilite #f]
	[cur-hilite-pos 0]
	[cur-id 1])
      (public
	[new-id (lambda () 
		  (begin0 
		    (number->string cur-id)
		    (set! cur-id (add1 cur-id))))]
	[for-each-snip
	 (lambda (f)
	   (let loop ([s (find-first-snip)])
	     (unless (null? s)
	       (f s)
	       (loop (send s next)))))]
	[for-each-selected-snip
	 (lambda (f)
	   (let loop ([s (find-next-selected-snip null)])
	     (unless (null? s)
	       (f s)
	       (loop (find-next-selected-snip s)))))]
	[in-selected-hierarchy?
	 (lambda (s)
	   (or (is-selected? s)
	       (let ([parent (send s gb-get-parent)])
		 (and parent
		      (in-selected-hierarchy? parent)))))]
	[find-unselected-snip
	 (lambda (x y)
	   (let ([s (find-snip x y)])
	     (if (or (null? s) (and (not (in-selected-hierarchy? s))
				    (ivar s container?)))
		 s
		 (let loop ([s (find-first-snip)])
		   (cond
		     [(null? s) (values null)]
		     [(and (ivar s container?)
			   (not (in-selected-hierarchy? s)))
		      (let ([tb (box 0)]
			    [lb (box 0)]
			    [bb (box 0)]
			    [rb (box 0)])
			(get-snip-location s lb tb #f)
			(get-snip-location s rb bb #t)
			(if (and (<= (unbox lb) x (unbox rb))
				 (<= (unbox tb) y (unbox bb)))
			    s
			    (loop (send s next))))]
		     [else (loop (send s next))])))))]
	[find-snip-by-id
	 (lambda (id)
	   (let/ec found
	     (for-each-snip
	      (lambda (s)
		(when (equal? id (ivar s id))
		  (found s)))))
	   #f)]
	[find-snip-by-original-id
	 (lambda (id)
	   (let/ec found
	     (for-each-snip
	      (lambda (s)
		(when (equal? id (ivar s original-id))
		  (found s))))
	     #f))]
	[on-move-to
	 (lambda (snip x y dragging?)
	   (not (eq? snip main-panel)))]
	[after-move-to
	 (lambda (snip x y dragging?)
	   (when dragging?
	     (send snip gb-drag-children-along x y)))]
	[on-interactive-move
	 (lambda ()
	   (set! dragging? #t)
	   (for-each-snip (lambda (s) (send s gb-set-stable-position)))
	   (super-on-interactive-move))]
	[on-select
	 (lambda (s on?)
	   (when (and (not copying?) on?)
	     ; deselect parents:
	     (let loop ([p (send s gb-get-parent)])
	       (when p
		 (if (is-selected? p)
		     (remove-selected p)
		     (loop (send p gb-get-parent)))))
	     ; deselect children:
	     (for-each
	      (lambda (c)
		(when (is-selected? c)
		  (remove-selected c)))
	      (send s gb-get-children)))
	   #t)]
	[after-interactive-move
	 (lambda ()
	   (set! dragging? #f)
	   (super-after-interactive-move)
	   
	   ; Adjust parent of selected snips & move selected snip's children
	   (for-each-selected-snip
	    (lambda (snip)
	      (when (not (eq? snip main-panel))
		(let* ([parent (send snip gb-get-parent)]
		       [pos (if parent
				(send parent gb-get-child-pos snip)
				-1)])
		  (if cur-hilite
		      (when (or (not (eq? cur-hilite parent))
				(not (= pos cur-hilite-pos)))
			(when parent
			  (send parent gb-remove-child snip))
			(send cur-hilite gb-add-child snip cur-hilite-pos)
			(set! cur-hilite-pos (add1 cur-hilite-pos)))
		      (when parent
			(send parent gb-remove-child snip)
			(send snip gb-install this #f))))
		(send snip gb-need-recalc-size))))
	   
	   (when cur-hilite
	     (send cur-hilite gb-hilite #f)
	     (set! cur-hilite #f)))]
	[interactive-adjust-move
	 (lambda (snip x-box y-box)
	   (super-interactive-adjust-move snip x-box y-box)
	   (let ([parent (send snip gb-get-parent)])
	     (when parent
	       (let-values ([(x y w h) 
			     (send (let loop ([p parent])
				     (let ([parent (send p gb-get-parent)])
				       (if parent
					   (loop parent)
					   p)))
				   gb-get-position-and-size)])
		 (when (and (<= x (unbox x-box) (+ x w))
			    (<= y (unbox y-box) (+ y h)))
		   (set-box! x-box (send snip gb-get-stable-x))
		   (set-box! y-box (send snip gb-get-stable-y)))))))]
	[interactive-adjust-resize
	 (lambda (snip wb hb)
	   (super-interactive-adjust-resize snip wb hb)
	   (let-values ([(x-min y-min) (send snip gb-get-saved-min-size)])
	     (when (or (not (ivar snip x-stretch?))
		       (<= (unbox wb) x-min))
	       (set-box! wb x-min))
	     (when (or (not (ivar snip y-stretch?))
		       (<= (unbox hb) y-min))
	       (set-box! hb y-min))))]
	[after-interactive-resize
	 (lambda (snip)
	   (super-after-interactive-resize snip)
	   (send snip gb-need-recalc-size))]
	[on-default-event
	 (lambda (e)
	   (when dragging?
	     (let ([x (send e get-x)]
		   [y (send e get-y)]
		   [xb (box 0)]
		   [yb (box 0)])
	       (send (get-admin) get-dc xb yb)
	       (let ([lx (+ x (unbox xb))]
		     [ly (+ y (unbox yb))])
		 (let ([s (find-unselected-snip lx ly)])
		   (when (not (null? s))
		     (set! cur-hilite-pos (send s gb-find-position x y)))
		   (when (and (or cur-hilite (not (null? s)))
			      (not (eq? cur-hilite s)))
		     (begin-edit-sequence)
		     (when cur-hilite
		       (send cur-hilite gb-hilite #f)
		       (set! cur-hilite #f))
		     (unless (null? s)
		       (set! cur-hilite s)
		       (send s gb-hilite #t))
		     (end-edit-sequence))))))
	   (super-on-default-event e))]
	[on-double-click
	 (lambda (snip e)
	   (send snip gb-open-dialog))]
	[after-delete
	 (lambda (snip)
	   (for-each delete (send snip gb-get-children))
	   (let ([parent (send snip gb-get-parent)])
	     (when parent
	       (send parent gb-remove-child snip)))
	   (super-after-delete snip))]
	[on-insert
	 (lambda (snip before x y)
	   (is-a? snip gb:snip%))]
	[after-insert
	 (lambda (snip behind x y)
	   (super-after-insert snip behind x y)
	   (when pasting?
	     (dynamic-wind
	      (lambda () (set! pasting? #f))
	      (lambda () (send snip gb-install this #f))
	      (lambda () (set! pasting? #t)))))]
	[do-paste
	 (lambda (time)
	   (dynamic-wind
	    (lambda () (set! pasting? #t))
	    (lambda () (super-do-paste time))
	    (lambda () (set! pasting? #f)))
	   (handle-new-arrivals))]
	[handle-new-arrivals
	 (lambda ()
	   (let loop ()
	     ((let/ec k
		(for-each-snip 
		 (lambda (s) 
		   (when (send s gb-reconnect-to-original-children)
		     (k loop))))
		void)))
	   (for-each-snip (lambda (s) (send s gb-forget-original-id))))]
	[do-copy
	 (lambda (time delete?)
	   (dynamic-wind
	    (lambda () (set! copying? #t))
	    (lambda () 
	      (unless (null? (find-next-selected-snip null))
		(letrec ([selected
			  (let loop ([s (find-next-selected-snip null)])
			    (let ([next (find-next-selected-snip s)])
			      (if (null? next)
				  (list s)
				  (cons s (loop next)))))]
			 [close-selected
			  (lambda (method)
			    (lambda (s)
			      (for-each
			       (lambda (child)
				 (method child)
				 ((close-selected method) child))
			       (send s gb-get-children))))])
		  (for-each (close-selected add-selected) selected)
		  (super-do-copy time delete?)
		  (for-each (close-selected remove-selected) selected))))
	    (lambda () (set! copying? #f))))]
	[get-selected-snip
	 (lambda () 
	   (let ([s (find-next-selected-snip null)])
	     (if (or (null? s)
		     (not (null? (find-next-selected-snip s))))
		 main-panel
		 s)))]
	[insert-element
	 (lambda (c%)
	   (let* ([i (make-object c%)]
		  [se (get-selected-snip)]
		  [s (if (ivar se container?)
			 se
			 (ivar se parent))])
	     (send s gb-add-child i)
	     (set-selected s)))])
      (private
	[main-panel #f])
      (public
	[get-main-panel (lambda () main-panel)]
	[create-main-panel
	 (lambda () 
	   (erase)
	   (set! main-panel (make-object gb:panel-snip%))
	   (insert main-panel 0 0)
	   (send main-panel gb-install this #f)
	   (send main-panel set-id "0"))]
	[on-load-file
	 (lambda (file mode)
	   (set! pasting? #t))]
	[after-load-file
	 (lambda (ok?)
	   (set! pasting? #f)
	   (when ok?
	     (set! main-panel (find-snip-by-original-id "0"))
	     (send main-panel set-id "0")
	     (handle-new-arrivals)
	     (set-modified #f)))])
      (sequence
	(super-init))))
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  (define gb:frame%
    (class mred:pasteboard-info-file-frame% ([file #f])
      (inherit get-edit make-menu show)
      (rename [super-make-menu-bar make-menu-bar])
      (private)
      (public
	[get-edit% (lambda () gb:edit%)]
	[instantiate
	 (lambda ()
	   (make-object (eval (build-code))))]
	[view-source
	 (lambda ()
	   (let ([port (open-output-string)])
	     (pretty-print (build-code) port)
	     (let ([f (make-object mred:editor-frame%)])
	       (send (send f get-edit) insert (get-output-string port)))))]
	[build-code
	 (lambda ()
	   `(class null ()
	      (public
		[frame (make-object mred:frame% null "Demo")]
		,@(send (send (get-edit) get-main-panel) gb-instantiate 'frame))
	      (sequence
		(send frame show #t))))]
	[make-menu-bar
	 (lambda ()
	   (let* ([mb (super-make-menu-bar)]
		  [emenu (make-menu)]
		  [append-element-type
		   (lambda (name c%)
		     (send emenu append-item
			   name (lambda () (insert-element c%))))]
		  [vmenu (make-menu)])
	     (append-element-type "New Vertical Panel" gb:vertical-panel-snip%)
	     (append-element-type "New Horizontal Panel" gb:horizontal-panel-snip%)
	     (append-element-type "New Message Label" gb:message-snip%)
	     (append-element-type "New Button" gb:button-snip%)
	     (append-element-type "New Checkbox" gb:check-box-snip%)
	     (append-element-type "New List" gb:list-box-snip%)
	     (append-element-type "New Radiobox" gb:radio-box-snip%)
	     (append-element-type "New Choice" gb:choice-snip%)
	     (append-element-type "New Slider" gb:slider-snip%)
	     (append-element-type "New Gauge" gb:gauge-snip%)
	     
	     (send vmenu append-item "Real Window" instantiate)
	     (send vmenu append-item "Source Code" view-source)
	     
	     (send mb append emenu "Element")
	     (send mb append vmenu "Output")
	     mb))]
	[insert-element
	 (lambda (c%)
	   (let ([e (get-edit)])
	     (send e insert-element c%)))])
      (sequence
	(super-init)
	(let ([file (normalize-path file)])
	  (unless (and file (file-exists? file) (send (get-edit) load-file file))
	      (send (get-edit) create-main-panel)
	      (when file
		    (send (get-edit) set-filename file))))
	(show #t))))
  
  (mred:insert-format-handler "GUI Builder" "gui"
			      (lambda (file)
				(make-object gb:frame% file)))
  
  (define (new-gui-builder-frame) (make-object gb:frame%)))
