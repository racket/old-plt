
(unit/sig gui-builder^
  (import mzlib:function^
	  mzlib:pretty-print^
	  mzlib:file^
	  (mred : mred^)
	  (framework : framework^))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Snips
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define GB:SNIP-VERSION 4)
  (define MINOR-VERSION 0)
  
  (define START-FRAME-WIDTH 100)
  (define START-FRAME-HEIGHT 100)

  ; Keep in order of choice items:
  (define FRAME-MODE 0)
  (define MODAL-DIALOG-MODE 1)
  (define DIALOG-MODE 2)
  (define PANEL-MODE 3)

  (define -FIRST-MODE- FRAME-MODE)
  (define -LAST-MODE- PANEL-MODE)

  (define make-one-line/callback-edit
    (opt-lambda (parent label cb [v ""])
      (make-object mred:text% label parent cb v)))

  (define make-number-control
    (lambda (parent label value get-min get-max set-v)
      (let* ([p (make-object mred:horizontal-panel% parent)]
	     [l (make-object mred:message% label p)]
	     [vl (make-object mred:message% "999999" p)]
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
					 (mred:message-box "Error" "Bad value"))))))
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

  (define (with-clipping-region dc x y w h thunk)
    (let ([cx (box 0)][cy (box 0)][cw (box 0)][ch (box 0)])
      (send dc get-clipping-region cx cy cw ch)
      (send dc set-clipping-region x y w h)
      (thunk)
      (if (positive? (unbox cw))
	  (send dc set-clipping-region (unbox cx) (unbox cy) (unbox cw) (unbox ch))
	  (send dc destroy-clipping-region))))
  
  (define gb:snip%
    (class mred:snip% ([lm 5][tm 5][rm 5][bm 5])
      (inherit get-admin set-snipclass set-count)
      (private 
	[need-recalc? #t]
	[prev-min-w 0]
	[prev-min-h 0])
      (public 
	[x 0] [stable-x 0]
	[y 0] [stable-y 0]
	[w (+ lm rm)]
	[h (+ tm bm)]
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
	       [kind "Panel"]
	       [on-main (lambda (x) x)]
	       [find-control (lambda (tag) #f)])
	     (sequence
	       (super-init null (format "~a Settings" kind) -1 -1 200 10))
	     (private
	       [main (on-main (make-object mred:vertical-panel% this))])
	     (public
	       [name-edit (make-one-line/callback-edit main "Scheme Name:"
						       (lambda (txt)
							 (set! name txt))
						       name)]
	       [controls (make-object mred:vertical-panel% main)])
	     (override
	       [on-close (lambda () (do-on-close))])
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
		      [xsc (make-sc "Allow Horizontal Stretching"
				    (lambda (on?) (set! x-stretch? on?)))]
		      [ysc (make-sc "Allow Vertical Stretching"
				    (lambda (on?) (set! y-stretch? on?)))])
		 (send p minor-align-left)
		 (send xsc set-value x-stretch?)
		 (send ysc set-value y-stretch?)
		 (let ([p (make-object mred:vertical-panel% p)])
		   (send p stretchable-height #f)))))))
	
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
		 [(2) (/ (- h mh) 2)]
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
	       [(null? l) (let* ([w (+ lw lm rm)]
				 [h (+ lh tm bm)])
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
	   (unless parent
		   (when (and pb (not (and (= w w-in) (= h h-in))))
			 (send pb top-resized this w h w-in h-in)))
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
			       [(cx) (send c gb-get-stable-x)]
			       [(cy) (send c gb-get-stable-y)]
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
		 (when pb 
		       (send pb get-snip-location this xb yb #f)
		       (send pb get-main-location this dc xb yb))
		 (gb-set-shape dc (unbox xb) (unbox yb) 
			       (if x-stretch? (max w mw) mw)
			       (if y-stretch? (max h mh) mh))))))
	
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
	   ; Make unique name
	   (let ([orig-name name])
	     (set! name #f)
	     (let loop ([new-name orig-name])
	       (if (send pb find-snip-by-name new-name)
		   (loop (string-append new-name "+"))
		   (set! name new-name))))
	   (set! original-id #f)
	   (set! original-children-ids #f)))
	
	(gb-get-class-defines
	 (lambda ()
	   (list `[-:init-stretch
		   (lambda (c hs? vs?)
		     (send c stretchable-width hs?)
		     (send c stretchable-height vs?)
		     c)])))

	(gb-get-instantiate-class-getter
	 (lambda ()
	   `(,(string->symbol (string-append "get-" name "%")))))
	(gb-local-instantiate
	 (lambda (parent)
	   `(make-object ,(gb-get-instantiate-class-getter) ,parent
			 ,(if with-border? '(border) 'null))))
	(gb-wrap-instantiate
	 (lambda (v)
	   `(-:init-stretch ,v ,x-stretch? ,y-stretch?)))

	(gb-get-default-class (lambda () 'mred:vertical-panel%))
	(gb-aux-instantiate
	 (lambda ()
	   (list
	    `[,(string->symbol (string-append "get-" name "%"))
	      (lambda () ,(gb-get-default-class))])))
	(gb-instantiate
	 (lambda (parent)
	   (let ([v (gb-local-instantiate parent)]
		 [name (string->symbol name)])
	     `(,@(gb-aux-instantiate)
	       [,name ,(gb-wrap-instantiate v)]
	       ,@(apply append
			(map (lambda (c) (send c gb-instantiate name)) children))))))

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
	 (lambda (nm xs? ys? nw nh hca vca wb? id children-ids)
	   (set! name nm)
	   (set! x-stretch? xs?)
	   (set! y-stretch? ys?)
	   (set! w nw)
	   (set! h nh)
	   (set! horizontal-child-alignment hca)
	   (set! vertical-child-alignment vca)
	   (set! with-border? wb?)
	   (set! original-id id)
	   (set! original-children-ids children-ids)))

	[get-tagged-value
	 (lambda (tag) #f)]
	[set-tagged-value void]

	(refresh
	 (lambda ()
	   (let ([admin (get-admin)])
	     (unless (null? admin)
	       (send admin needs-update this 0 0 w h)))))
	(resized
	 (lambda ()
	   (let ([admin (get-admin)])
	     (unless (null? admin)
	       (send admin resized this #t))))))

      (override
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
	(copy
	 (lambda ()
	   (let ([o (make-object (object-class this) lm tm rm bm)])
	     (send o base-setup 
		   name
		   x-stretch? y-stretch? w h 
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
	   (send stream << name)
	   (send stream << (if x-stretch? 1 0))
	   (send stream << (if y-stretch? 1 0))
	   (send stream << (inexact->exact w))
	   (send stream << (inexact->exact h))
	   (send stream << horizontal-child-alignment)
	   (send stream << vertical-child-alignment)
	   (send stream << (if with-border? 1 0))
	   (send stream << (if id id "BAD"))
	   (stream-write-list stream (map (lambda (c) (ivar c id)) children)))))
      (public
	(read
	 (lambda (stream version)
	   (base-setup
	    (if (>= version 3) (send stream get-string) name) ; name
	    (positive? (send stream get-exact))
	    (positive? (send stream get-exact))
	    (send stream get-exact) ; w
	    (send stream get-exact) ; h
	    (if (>= version 2) (send stream get-exact) horizontal-child-alignment) ; hca
	    (if (>= version 2) (send stream get-exact) vertical-child-alignment) ; vca
	    (if (>= version 2) (positive? (send stream get-exact)) #f) ; with-border?
	    (send stream get-string)
	    (let ([v (stream-read-list stream)])
	      (if (null? v) #f v))))))
      (override
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
	(set-snipclass (send (mred:get-the-snip-class-list) find classname))
	(set-count 1))))
  
  (define register-class
    (lambda (class% classname)
      (let ([snipclass
	     (make-object 
	      (class mred:snip-class% ()
		(inherit set-classname set-version)
		(override
		  [read
		   (lambda (stream)
		     (let ([o (make-object class%)]
			   [scl (mred:get-the-snip-class-list)])
		       (send o read stream (send scl reading-version this))
		       o))])
		(sequence
		  (super-init)
		  (set-classname classname)
		  (set-version GB:SNIP-VERSION))))])
	(send (mred:get-the-snip-class-list) add snipclass))))
  
  (register-class gb:snip% "gb:core")
  
  (define gb:make-panel-params-snip%
    (lambda (cl)
      (class-asi cl
	(inherit horizontal-child-alignment set-horizontal-child-alignment
		 vertical-child-alignment  set-vertical-child-alignment
		 with-border? set-with-border
		 gb-need-recalc-size)
	(rename [super-get-frame% get-frame%]
		[super-gb-get-class-defines gb-get-class-defines]
		[super-gb-wrap-instantiate gb-wrap-instantiate])
	(override
	  [get-frame%
	   (lambda ()
	     (class (super-get-frame%) args
	       (inherit controls)
	       (sequence
		 (apply super-init args))
	       (public
		 [hca-choice
		  (make-object mred:choice% controls 
			       (lambda (r e)
				 (set-horizontal-child-alignment
				  (add1 (send r get-selection)))
				 (gb-need-recalc-size))
			       "Horizontal Align Children:"
			       -1 -1 -1 -1
			       '("Left" "Center" "Right"))]
		 [vca-choice
		  (make-object mred:choice% controls 
			       (lambda (r e)
				 (set-vertical-child-alignment
				  (add1 (send r get-selection)))
				 (gb-need-recalc-size))
			       "Vertical Align Children:"
			       -1 -1 -1 -1
			       '("Top" "Center" "Bottom"))]
		 [border-check
		  (make-object mred:check-box% controls
			       (lambda (c e)
				 (set-with-border (send c get-value))
				 (gb-need-recalc-size))
			       "Show Border")])
	       (sequence
		 (send hca-choice stretchable-width #f)
		 (send hca-choice set-selection (sub1 horizontal-child-alignment))
		 (send vca-choice stretchable-width #f)
		 (send vca-choice set-selection (sub1 vertical-child-alignment))
		 (send border-check set-value with-border?))))])
	(private
	 [symbol-append
	  (lambda (a b) (string->symbol (string-append (symbol->string a) (symbol->string b))))])
	(public
	 [horizontal-is-major? #f])
	(override
	 [gb-get-class-defines
	  (lambda ()
	    (append (super-gb-get-class-defines)
		    (list `[-:init-panel
			    (lambda (c ha va)
			      ((ivar/proc c ha))
			      ((ivar/proc c va))
			      c)])))]
	 [gb-wrap-instantiate
	  (lambda (v)
	    `(-:init-panel ,(super-gb-wrap-instantiate v)
			   ',(symbol-append
			      (if horizontal-is-major?
				  'major
				  'minor)
			      (case horizontal-child-alignment
				[(1) '-align-left]
				[(2) '-align-center]
				[(3) '-align-right]))
			   ',(symbol-append
				 (if horizontal-is-major?
				     'minor
				     'major)
				 (case horizontal-child-alignment
				   [(1) '-align-top]
				   [(2) '-align-center]
				   [(3) '-align-bottom]))))]))))

  (define gb:vertical-panel-snip% 
    (class-asi (gb:make-panel-params-snip% gb:snip%)
      (override
        [old-vertical-child-alignment 1]
	[classname "gb:vertical-panel"]
	[name (new-name "vpanel")])))
  
  (register-class gb:vertical-panel-snip% "gb:vertical-panel")

  ; Used by top-level panel:
  (define gb:panel-snip%
    (class-asi gb:vertical-panel-snip%
      (override
       [classname "gb:panel"])))
  
  (register-class gb:panel-snip% "gb:panel")
  
  (define gb:horizontal-panel-snip%
    (class-asi (gb:make-panel-params-snip% gb:snip%)
      (inherit spacing-+ horizontal-child-alignment vertical-child-alignment
	       children gb-get-instantiate-class-getter)
      (private
	(sp-+ (lambda args (apply spacing-+ args))))
      (override
        [old-horizontal-child-alignment 1]
	[classname "gb:horizontal-panel"]
	[name (new-name "hpanel")]

	[horizontal-is-major? #t]

	(gb-get-child-x-start
	 (lambda (mw mh w h)
	   (if (or (= horizontal-child-alignment 1)
		   (ormap (lambda (c) (ivar c x-stretch?)) children))
	       0
	       (case horizontal-child-alignment
		 [(2) (/ (- w mw) 2)]
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
	
	[gb-get-default-class (lambda () 'mred:horizontal-panel%)])))

  (register-class gb:horizontal-panel-snip% "gb:horizontal-panel")
  
  (define gb:atomic-snip%
    (class-asi gb:snip%
      (override
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
	(override
	  [get-frame%
	   (lambda ()
	     (class (super-get-frame%) args
	       (inherit controls)
	       (public
		 [kind deflabel])
	       (sequence
		 (apply super-init args))
	       (public
		 [label-buffer (make-one-line/callback-edit controls "Label:"
							    (lambda (txt)
							      (set! label txt)
							      (gb-need-recalc-size))
							    label)])))])
	(public
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
	     (set! label n))])
	(override
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
	(inherit w h get-label get-label-size draw-label
		 gb-get-instantiate-class-getter)
	(override
	  [classname cn]
	  [name (new-name "message")]
	  [gb-get-min-size
	   (lambda (dc)
	     (get-label-size dc))]
	  [draw
	   (lambda (dc x y . other)
	     (draw-label dc x y))]
	  [gb-get-default-class (lambda () 'mred:message%)]
	  [gb-local-instantiate
	   (lambda (p)
	     `(make-object ,(gb-get-instantiate-class-getter) ,p ,(get-label)))]))))
  
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
	      callback-kinds))])
	(override
	  [gb-aux-instantiate
	   (lambda ()
	     (append
	      (map (lambda (n c) `[,n ,c]) (get-callback-names) callback-code)
	      (super-gb-aux-instantiate)))]))))
  
  (define gb:make-button-snip%
    (lambda (cl cn)
      (class-asi cl
	(inherit w h get-label get-label-size get-callback-names draw-label
		 gb-get-instantiate-class-getter)
	(private
	  [m 5])
	(override
	  [classname cn]
	  [name (new-name "button")]
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
	  [gb-get-default-class (lambda () 'mred:button%)]
	  [gb-local-instantiate
	   (lambda (p)
	     `(make-object ,(gb-get-instantiate-class-getter) 
			   ,p (lambda (b e) (,(car (get-callback-names)) b e)) ,(get-label)))]))))
  
  (define gb:make-check-box-snip%
    (lambda (cl cn)
      (class-asi cl
	(inherit w h get-style get-label get-callback-names get-label-size draw-label
		 gb-get-instantiate-class-getter)
	(private
	  [hspace 2]
	  [boxsize 12])
	(override
	  [classname cn]
	  [name (new-name "checkbox")]
	  [gb-get-min-size
	   (lambda (dc)
	     (let-values ([(x y) (get-label-size dc)])
	       (values (+ boxsize hspace x) (max boxsize y))))]
	  [draw
	   (lambda (dc x y . other)
	     (let-values ([(lx ly) (get-label-size dc)])
	       (send dc draw-rectangle x (+ y (/ (- h boxsize) 2)) boxsize boxsize)
	       (draw-label dc (+ x boxsize hspace) (+ y (/ (- h ly) 2)))))]
	  [gb-get-default-class (lambda () 'mred:check-box%)]
	  [gb-local-instantiate
	   (lambda (p)
	     `(make-object ,(gb-get-instantiate-class-getter)
			   ,p (lambda (c e) (,(car (get-callback-names)) c e)) ,(get-label)))]))))
  
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
	     (set! items l))])
	(override
	  [get-frame%
	   (lambda ()
	     (class (super-get-frame%) args
	       (inherit controls)
	       (sequence
		 (apply super-init args))
	       (public
		 [user-item (lambda (v)
			      (mred:get-text-from-user "Item name:" "List Item Name" v))]
		 [items-panel (make-object mred:vertical-panel% controls)]
		 [items-list (make-object mred:list-box%
					  "Items:" 
					  items
					  items-panel 
					  (lambda (l e)
					    (when (= 2 (send e get-extra-long))
					      (let ([pos (send items-list get-selection)])
						(unless (negative? pos)
						  (let ([v (user-item (list-ref items pos))])
						    (when v
						      (send items-list set-string pos v)
						      (set-car! (list-tail items pos) v)
						      (gb-need-recalc-size))))))))]
		 [item-buttons-panel (let ([v (make-object mred:horizontal-panel% items-panel)])
				       (send v stretchable-width #f)
				       v)]
		 [add-item (make-object mred:button% "Add Item" item-buttons-panel
					(lambda (b e)
					  (let ([v (user-item (format "Item~a" (send items-list number)))])
					    (when v
					      (send items-list append v)
					      (set! items (append items (list v)))
					      (gb-need-recalc-size)))))]
		 [delete-item (make-object mred:button% "Delete Item" item-buttons-panel
					   (lambda (b e)
					     (let loop ([ls (reverse (send items-list get-selections))])
					       (unless (null? ls)
						 (send items-list delete (car ls))
						 (set! items (delete items (car ls)))
						 (loop (cdr ls))))
					     (gb-need-recalc-size)))])))]
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
		[super-read read]
		[super-gb-get-class-defines gb-get-class-defines])
	(private
	  [hmargin 2])
	(public
	  [get-label-top-margin (lambda () 0)]
	  [vertical-label? #f]

	  [get-min-body-size
	   (lambda (dc)
	     (values 0 0))]
	  [draw-body
	   (lambda (dc x y w h)
	     (void))]

	  [wrap-label-direction
	   (lambda (p e)
	     `(-:with-label-direction ,(if vertical-label? 
					   ''vertical 
					   ''horizontal) 
				      ,p (lambda () ,e)))]
	  
	  [labelpos-install
	   (lambda (vert?)
	     (set! vertical-label? vert?))])
	(override
	  [get-frame%
	   (lambda ()
	     (class (super-get-frame%) args
	       (inherit controls)
	       (sequence
		 (apply super-init args))
	       (public
		 [direction-radio
		  (make-object mred:radio-box% "Label Position:" '("Top" "Left")
			       controls 
			       (lambda (r e)
				 (set! vertical-label? (zero? (send direction-radio get-selection)))
				 (gb-need-recalc-size))
			       '(horizontal))])
	       (sequence
		 (send direction-radio set-selection (if vertical-label? 0 1)))))]
	  [gb-get-min-size
	   (lambda (dc)
	     (let-values ([(x y) (get-label-size dc)]
			  [(x2 y2) (get-min-body-size dc)]
			  [(+x +y) (if vertical-label?
				       (values max +)
				       (values (lambda (a b) (+ a b hmargin)) max))])
	       (values (+x x x2) (+y (+ y (get-label-top-margin)) y2))))]
	  [draw
	   (lambda (dc x y . other)
	     (draw-label dc x (+ y (get-label-top-margin)))
	     (let*-values ([(lw lh) (get-label-size dc)]
			   [(dx dy) (if vertical-label?
					(values 0 lh)
					(values (+ lw hmargin) 0))])
	       (with-clipping-region dc (+ x dx) (+ y dy) (- w dx) (- h dy)
		 (lambda ()
		   (draw-body dc (+ x dx) (+ y dy) (- w dx) (- h dy))))))]
	  
	  [gb-get-class-defines
	   (lambda ()
	     (append (super-gb-get-class-defines)
		     (list `[-:with-label-direction
			     (lambda (direction parent make-it)
			       (send parent set-label-position direction)
			       (make-it))])))]

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
  
  (define gb:make-text-initial-snip%
    (lambda (cl)
      (class-asi cl
	(inherit gb-need-recalc-size get-style)
	(rename [super-get-frame% get-frame%]
		[super-copy copy]
		[super-write write]
		[super-read read]
		[super-gb-get-class-defines gb-get-class-defines])
	(private
	  [initial "value"])
	(public
	  [get-initial (lambda () initial)]
	  [get-initial-size
	   (lambda (dc)
	     (let ([xb (box 0.0)]
		   [yb (box 0.0)])
	       (send dc get-text-extent initial xb yb null null
		     (send (get-style) get-font))
	       (values (unbox xb) (unbox yb))))]

	  [initial-install
	   (lambda (i)
	     (set! initial i))])
	(override
	  [get-frame%
	   (lambda ()
	     (class (super-get-frame%) args
	       (inherit controls)
	       (sequence
		 (apply super-init args))
	       (public
		 [initial-text
		  (make-one-line/callback-edit controls "Initial:"
					       (lambda (txt)
						 (set! initial txt)
						 (gb-need-recalc-size))
					       initial)])))]

	  [copy
	   (lambda ()
	     (let ([o (super-copy)])
	       (send o initial-install initial)
	       o))]
	  [write
	   (lambda (stream)
	     (super-write stream)
	     (send stream << initial))]
	  [read
	   (lambda (stream version)
	     (super-read stream version)
	     (initial-install (send stream get-string)))]))))


  (define gb:make-configure-snip%
    (lambda (cl tag init)
      (class-asi cl
	(rename [super-copy copy]
		[super-write write]
		[super-read read]
		[super-get-tagged-value get-tagged-value]
		[super-set-tagged-value set-tagged-value])
	(private
	  [v init])
	(override
	  [get-tagged-value
	   (lambda (t)
	     (if (eq? t tag)
		 v
		 (super-get-tagged-value t)))]
	  [set-tagged-value
	   (lambda (t v-in)
	     (if (eq? t tag)
		 (set! v v-in)
		 (super-set-tagged-value t v-in)))]

	  [copy
	   (lambda ()
	     (let ([o (super-copy)])
	       (send o set-tagged-value tag v)
	       o))]))))

  (define gb:make-boolean-configure-snip%
    (lambda (cl tag label init change-cb init-cb)
      (class-asi (gb:make-configure-snip% cl tag init)
	(inherit gb-need-recalc-size get-tagged-value set-tagged-value)
	(rename [super-get-frame% get-frame%]
		[super-write write]
		[super-read read])
	(override
	  [get-frame%
	   (lambda ()
	     (class*/names (frame super-init) (super-get-frame%) () args
	       (inherit controls)
	       (rename [super-find-control find-control])
	       (public
		[find-control
		 (lambda (t)
		   (if (eq? t tag)
		       c
		       (super-find-control t)))])
	       (sequence
		 (apply super-init args))
	       (private
		 [c (make-object mred:check-box% controls
				 (lambda (c e)
				   (set-tagged-value tag (send c get-value))
				   (change-cb frame this)
				   (gb-need-recalc-size))
				 label)])
	       (sequence
		 (send c set-value (get-tagged-value tag))
		 (init-cb frame this))))]
	  [write
	   (lambda (stream)
	     (super-write stream)
	     (send stream << (if (get-tagged-value tag) 1 0)))]
	  [read
	   (lambda (stream version)
	     (super-read stream version)
	     (set-tagged-value tag (positive? (send stream get-exact))))]))))

  (define gb:make-multi-checkable-snip%
    (lambda (cl)
      (class-asi (gb:make-boolean-configure-snip% cl 'multi "Multiple Lines" #f
						  (lambda (f snip)
						    (send snip multi-changed f))
						  void)
	 (inherit get-tagged-value)
	 (public
	  [get-multi
	   (lambda () (get-tagged-value 'multi))]
	  [multi-changed
	   (lambda (f)
	     (send (send f find-control 'hscroll) enable (get-multi)))]))))
  
  (define gb:make-text-hscroll-checkable-snip%
    (lambda (cl)
      (class-asi (gb:make-boolean-configure-snip% cl 'hscroll "Horizontal Scroll" #f 
						  void
						  (lambda (f snip)
						    (send (send f find-control 'hscroll)
							  enable
							  (send snip get-tagged-value 'multi))))
	 (inherit get-tagged-value)
	 (public
	  [get-style-flag
	   (lambda () 
	     (if (get-tagged-value 'hscroll)
		 '(hscroll)
		 null))]))))
  
  (define gb:make-text-snip%
    (lambda (cl cn)
      (class-asi cl
	(inherit w h get-initial-size get-initial
		 get-callback-names get-multi get-style-flag
		 wrap-label-direction get-label gb-get-instantiate-class-getter)
	(private
	  [margin 2])
	(override
	  [classname cn]
	  [name (new-name "text")]
	  [x-stretch? #t]
	  [get-label-top-margin (lambda () margin)]
	  [get-min-body-size
	   (lambda (dc)
	     (let-values ([(w h) (get-initial-size dc)])
		(values (+ w (* 2 margin))
			(+ (* h (if (get-multi) 3 1))
			   (* 2 margin)))))]
	  [draw-body
	   (lambda (dc x y w h)
	     (send dc draw-rectangle x y w h)
	     (send dc draw-text (get-initial) (+ x margin) (+ y margin)))]
	  [callback-kinds (list "-change-callback" "-return-callback" "-focus-callback")]
	  [gb-get-default-class (lambda () 
				  (if (get-multi)
				      'mred:media-multi-text%
				      'mred:media-text%))]
	  [gb-local-instantiate
	   (lambda (p)
	     (wrap-label-direction
	      p `(make-object ,(gb-get-instantiate-class-getter)
			      ,(get-label) ,p 
			      ,(let-values ([(change return focus) 
					     (apply values (get-callback-names))])
				 `(lambda (b e) 
				    (let ([t (send e get-event-type)])
				      (cond 
				       [(eq? t 'text-field) (,change b e)]
				       [(eq? t 'text-field-enter) (,return b e)]
				       [else (,focus b e)]))))
			      ,(get-initial)
			      ,(get-style-flag))))]))))
  
  (define gb:text-snip% (gb:make-text-snip%
			 (gb:make-text-hscroll-checkable-snip%
			  (gb:make-multi-checkable-snip%
			   (gb:make-text-initial-snip%
			    (gb:make-callback-snip%
			     (gb:make-text-labelled-snip% gb:atomic-snip% 
							  "Text")))))
			 "gb:text"))
  
  (register-class gb:text-snip% "gb:text")
  
  
  (define gb:make-list-box-snip%
    (lambda (cl cn)
      (class-asi cl
	(inherit w h get-callback-names get-items get-item-height
		 wrap-label-direction get-label gb-get-instantiate-class-getter)
	(public
	  [min-body-width 50]
	  [sb-width 10]
	  [min-item-count 3])
	(override
	  [classname cn]
	  [name (new-name "listbox")]
	  [y-stretch? #t]
	  [x-stretch? #t]
	  [get-min-body-size
	   (lambda (dc)
	     (let ([y (get-item-height dc)])
	       (values min-body-width (* min-item-count y))))]
	  [draw-body
	   (lambda (dc x y w h)
	     (send dc draw-rectangle x y w h)
	     (send dc draw-line 
		   (+ w x (- sb-width)) y
		   (+ w x (- sb-width)) (+ y h))
	     (with-clipping-region 
	      dc x y (- w sb-width) h
	      (lambda ()
		(let ([ih (get-item-height dc)])
		  (let loop ([l (get-items)][iy (add1 y)])
		    (unless (or (>= iy (+ y h)) (null? l))
			    (send dc draw-text (car l) (+ 2 x) iy)
			    (loop (cdr l) (+ iy ih))))))))]
	  [callback-kinds (list "-select-callback" "-double-select-callback")]
	  [gb-get-default-class (lambda () 'mred:list-box%)]
	  [gb-local-instantiate
	   (lambda (p)
	     (wrap-label-direction
	      p `(make-object ,(gb-get-instantiate-class-getter)
			      ,(get-label) ',(get-items) ,p 
			      ,(let-values ([(sel dbl) (apply values (get-callback-names))])
				 `(lambda (b e) 
				    (case (send e get-event-type)
				      [(list-box) (,sel b e)]
				      [(list-box-dclick) (,dbl b e)]))))))]))))
  
  
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
	(override
	  [get-frame%
	   (lambda ()
	     (class (super-get-frame%) args
	       (inherit controls)
	       (sequence
		 (apply super-init args))
	       (public
		 [layout-direction-radio
		  (make-object mred:radio-box%
			       "Layout:"
			       '("Vertical" "Horizontal")
			       controls 
			       (lambda (r e)
				 (set! vertical-layout? (zero? (send layout-direction-radio get-selection)))
				 (gb-need-recalc-size))
			       '(horizontal))])
	       (sequence
		 (send layout-direction-radio set-selection (if vertical-layout? 0 1)))))])
	(public
	  [vertical-layout? #t]
	  [layout-install
	   (lambda (vert?)
	     (set! vertical-layout? vert?))])
	(override
	  [copy
	   (lambda ()
	     (let ([o (super-copy)])
	       (send o layout-install vertical-layout?)
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
		 wrap-label-direction get-label gb-need-recalc-size vertical-layout?
		 gb-get-instantiate-class-getter)
	(rename [super-get-frame% get-frame%])
	(private
	  [circle-size 10]
	  [margin 2])
	(override
	  [classname cn]
	  [name (new-name "radiobox")]
	  [init-items (list "First" "Second")]
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
	  [gb-get-default-class (lambda () 'mred:radio-box%)]
	  [gb-local-instantiate
	   (lambda (p)
	     (wrap-label-direction
	      p `(make-object ,(gb-get-instantiate-class-getter)
			      ,(get-label) ',(get-items)
			      ,p (lambda (b e) (,(car (get-callback-names)) b e)) 
			      '(,(if vertical-layout? 'vertical 'horizontal)))))]))))
  
  
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
		 wrap-label-direction gb-need-recalc-size get-label gb-get-instantiate-class-getter)
	(public
	  [arrow-size 10]
	  [lmargin 2]
	  [amargin 2]
	  [rmargin 2]
	  [arrow (list (make-object mred:point% 0 0)
		       (make-object mred:point% arrow-size 0)
		       (make-object mred:point% (quotient arrow-size 2) (quotient arrow-size 2)))])
	(override
	  [classname cn]
	  [name (new-name "choice")]
	  [init-items (list "First")]
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
	  [gb-get-default-class (lambda () 'mred:choice%)]
	  [gb-local-instantiate
	   (lambda (p)
	     (wrap-label-direction
	      p `(make-object ,(gb-get-instantiate-class-getter)
			      ,p (lambda (b e) (,(car (get-callback-names)) b e)) 
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
	(inherit get-label get-callback-names wrap-label-direction gb-need-recalc-size 
		 gb-get-instantiate-class-getter)
	(rename [super-get-frame% get-frame%]
		[super-copy copy]
		[super-write write]
		[super-read read])
	(public
	  [init-value 0]
	  [min-value 0]
	  [max-value 10]
	  [arrow-size 10]
	  [height arrow-size]
	  [line-height 3]
	  [min-width 50]
	  [darrow (list (make-object mred:point% 0 0)
			(make-object mred:point% arrow-size 0)
			(make-object mred:point% (quotient arrow-size 2) (quotient arrow-size 2)))]
	  [rarrow (list (make-object mred:point% 0 0)
			(make-object mred:point% 0 arrow-size)
			(make-object mred:point% (quotient arrow-size 2) (quotient arrow-size 2)))]
	  [slider-install
	   (lambda (mn mx in)
	     (set! min-value mn)
	     (set! max-value mx)
	     (set! init-value in))])
	(override
	  [get-frame%
	   (lambda ()
	     (class (super-get-frame%) args
	       (inherit controls)
	       (sequence
		 (apply super-init args))
	       (public
		 [min-val (make-number-control controls "Minimum:" 0 (lambda () -10000) (lambda () 10000) 
					       (lambda (x) 
						 (set! min-value x) 
						 (send max-val check)
						 (send init-val check)
						 (gb-need-recalc-size)))]
		 [max-val (make-number-control controls "Maximum:" 10 (lambda () (send min-val get-val)) (lambda () 10000)
					       (lambda (x)
						 (set! max-value x)
						 (send init-val check)
						 (gb-need-recalc-size)))]
		 [init-val (make-number-control controls "Initial:" 0 (lambda () (send min-val get-val)) 
						(lambda () (send max-val get-val))
						(lambda (x)
						  (set! init-value x)
						  (gb-need-recalc-size)))])))]
	  [classname cn]
	  [name (new-name "slider")]
	  [vertical-layout? #f]
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
	  [gb-get-default-class (lambda () 'mred:slider%)]
	  [gb-local-instantiate
	   (lambda (p)
	     (wrap-label-direction
	      p `(make-object ,(gb-get-instantiate-class-getter)
			      ,(get-label) ,min-value ,max-value ,p 
			      (lambda (b e) (,(car (get-callback-names)) b e)) 
			      ,init-value 
			      '(,(if vertical-layout? 'vertical 'horizontal)))))]
	  
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
	(inherit get-label wrap-label-direction gb-need-recalc-size
		 gb-get-instantiate-class-getter)
	(rename [super-get-frame% get-frame%]
		[super-copy copy]
		[super-write write]
		[super-read read])
	(public
	  [max-value 10]
	  [min-height 10]
	  [min-width 50]
	  [gauge-install
	   (lambda (mx)
	     (set! max-value mx))])
	(override
	  [get-frame%
	   (lambda ()
	     (class (super-get-frame%) args
	       (inherit controls)
	       (sequence
		 (apply super-init args))
	       (public
		 [max-val (make-number-control controls "Maximum:" 10 (lambda () 1) (lambda () 10000) 
					       (lambda (x) 
						 (set! max-value x) 
						 (gb-need-recalc-size)))])))]
	  [classname cn]
	  [name (new-name "gauge")]
	  [vertical-layout? #f]
	  [get-min-body-size
	   (lambda (dc)
	     (if vertical-layout?
		 (values min-height min-width)
		 (values min-width min-height)))]
	  [draw-body
	   (lambda (dc x y w h)
	     (send dc draw-rectangle x y w h)
	     (let ([b (send dc get-brush)])
	       (send dc set-brush (send mred:the-brush-list find-or-create-brush "BLACK" 'solid))
	       (send dc draw-rectangle 
		     x (if vertical-layout? (+ y (* 0.75 h)) y)
		     (if vertical-layout? w (* 0.25 w)) (if vertical-layout? (* 0.25 h) h))
	       (send dc set-brush b)))]
	  [gb-get-default-class (lambda () 'mred:gauge%)]
	  [gb-local-instantiate
	   (lambda (p)
	     (wrap-label-direction
	      p `(make-object ,(gb-get-instantiate-class-getter)
			      ,(get-label) ,max-value ,p
			      '(,(if vertical-layout? 'vertical 'horizontal)))))]
	  
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

  (define gb:make-canvas-hscroll-checkable-snip%
    (lambda (cl)
      (class-asi (gb:make-boolean-configure-snip% cl 'hscroll "Horizontal Scroll" #t
						  void void)
	 (inherit get-tagged-value)
	 (public
	  [get-hscroll
	   (lambda () (get-tagged-value 'hscroll))]))))

  (define gb:make-canvas-vscroll-checkable-snip%
    (lambda (cl)
      (class-asi (gb:make-boolean-configure-snip% cl 'vscroll "Vertical Scroll" #t
						  void void)
	 (inherit get-tagged-value)
	 (public
	  [get-vscroll
	   (lambda () (get-tagged-value 'vscroll))]))))

  (define gb:make-sb-box-snip%
    (lambda (cl item-kind)
      (class-asi cl
	(inherit w h get-hscroll get-vscroll)
	(rename [super-get-frame% get-frame%])
	(public
	  [sb-width 10]
	  [canvas-min-space 15])
	(override
	 [get-frame%
	  (lambda ()
	    (class-asi (super-get-frame%)
	     (public
	       [kind item-kind])))]
	  [x-stretch? #t]
	  [y-stretch? #t]
	  [gb-get-min-size
	   (lambda (dc)
	     (values (+ sb-width canvas-min-space)
		     (+ sb-width canvas-min-space)))]
	  [draw
	   (lambda (dc x y . other)
	     (send dc draw-rectangle x y w h)
	     (when (get-vscroll)
		   (send dc draw-line 
			 (+ x w (- sb-width)) y
			 (+ x w (- sb-width)) (+ y h -1)))
	     (when (get-hscroll)
		   (send dc draw-line
			 x (+ y h (- sb-width))
			 (+ x w -1) (+ y h (- sb-width)))))]))))

  (define gb:make-canvas-snip%
    (lambda (cl cn)
      (class-asi cl
	(inherit get-hscroll get-vscroll gb-get-instantiate-class-getter)
	(public
	  [get-style-flag
	   (lambda ()
	     (cond
	      [(and (get-hscroll) (get-vscroll)) ''(hscroll vscroll)]
	      [(get-hscroll) ''(hscroll)]
	      [(get-vscroll) ''(vscroll)]
	      [else 0]))])
	(override
	  [classname cn]
	  [name (new-name "canvas")]
	  
	  [gb-get-default-class (lambda () 'mred:canvas%)]
	  [gb-local-instantiate
	   (lambda (p)
	     `(make-object ,(gb-get-instantiate-class-getter) ,p
			   -1 -1 -1 -1
			   ,(get-style-flag)))]))))
	  
  (define gb:canvas-snip% (gb:make-canvas-snip%
			   (gb:make-sb-box-snip%
			    (gb:make-canvas-vscroll-checkable-snip%
			     (gb:make-canvas-hscroll-checkable-snip%
			      gb:atomic-snip%))
			    "Canvas")
			   "gb:canvas"))
  
  (register-class gb:canvas-snip% "gb:canvas")

  (define gb:make-select-configure-snip%
    (lambda (cl tag label choices)
      (class-asi (gb:make-configure-snip% cl tag 0)
	(inherit gb-need-recalc-size get-tagged-value set-tagged-value)
	(rename [super-get-frame% get-frame%]
		[super-write write]
		[super-read read])
	(override
	  [get-frame%
	   (lambda ()
	     (class*/names (frame super-init) (super-get-frame%) () args
	       (inherit controls)
	       (sequence
		 (apply super-init args))
	       (private
		 [c (make-object mred:choice% controls
				 (lambda (c e)
				   (set-tagged-value tag (send c get-selection))
				   (gb-need-recalc-size))
				 label
				 -1 -1 -1 -1
				 choices)])
	       (sequence
		 (send c set-selection (get-tagged-value tag)))))]
	  [write
	   (lambda (stream)
	     (super-write stream)
	     (send stream << (get-tagged-value tag)))]
	  [read
	   (lambda (stream version)
	     (super-read stream version)
	     (set-tagged-value tag (send stream get-exact)))]))))
  
  (define gb:make-mcanvas-hscroll-select-snip%
    (lambda (cl)
      (class-asi (gb:make-select-configure-snip% cl 'hscroll "Horizontal Scroll"
						 '("Show" "Hide" "No Scrolling"))
	 (inherit get-tagged-value)
	 (public
	  [get-hscroll
	   (lambda () (zero? (get-hscroll-val)))]
	  [get-hscroll-val
	   (lambda () (get-tagged-value 'hscroll))]))))

  (define gb:make-mcanvas-vscroll-select-snip%
    (lambda (cl)
      (class-asi (gb:make-select-configure-snip% cl 'vscroll "Vertical Scroll"
						 '("Show" "Hide" "No Scrolling"))
	 (inherit get-tagged-value)
	 (public
	  [get-vscroll
	   (lambda () (zero? (get-vscroll-val)))]
	  [get-vscroll-val
	   (lambda () (get-tagged-value 'vscroll))]))))

  (define gb:make-media-canvas-snip%
    (lambda (cl cn)
      (class-asi cl
	(inherit get-hscroll-val get-vscroll-val gb-get-instantiate-class-getter)
	(public
	  [get-style-flag
	   (lambda ()
	     `'(,@(case (get-hscroll-val)
		    [(0) null]
		    [(1) '(hide-hscroll)]
		    [(2) '(no-hscroll)])
		,@(case (get-vscroll-val)
		    [(0) ()]
		    [(1) '(hide-vscroll)]
		    [(2) '(no-vscroll)])))])
	  
	(override
	  [classname cn]
	  [name (new-name "mcanvas")]
	  
	  [gb-get-default-class (lambda () 'mred:media-canvas%)]
	  [gb-local-instantiate
	   (lambda (p)
	     `(make-object ,(gb-get-instantiate-class-getter) ,p
			   -1 -1 -1 -1 "media-canvas"
			   ,(get-style-flag)))]))))
	  
  (define gb:media-canvas-snip% (gb:make-media-canvas-snip%
				 (gb:make-sb-box-snip%
				  (gb:make-mcanvas-vscroll-select-snip%
				   (gb:make-mcanvas-hscroll-select-snip%
				    gb:atomic-snip%))
				  "Media Canvas")
				 "gb:media-canvas"))
  
  (register-class gb:media-canvas-snip% "gb:media-canvas")

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
  
  (define top-font (send mred:the-font-list find-or-create-font 
			 12 'default 'normal 'normal #f))

  (define gb:edit%
    (class framework:pasteboard:backup-autosave% ()
      (inherit set-selected find-next-selected-snip insert
	       find-first-snip is-selected? add-selected remove-selected
	       get-admin find-snip begin-edit-sequence end-edit-sequence
	       get-snip-location delete erase set-modified resize
	       invalidate-bitmap-cache
	       begin-write-header-footer-to-file end-write-header-footer-to-file)
      (rename [super-interactive-adjust-move interactive-adjust-move]
	      [super-interactive-adjust-resize interactive-adjust-resize]
	      [super-on-interactive-move on-interactive-move]
	      [super-after-interactive-move after-interactive-move]
	      [super-after-interactive-resize after-interactive-resize]
	      [super-on-default-event on-default-event]
	      [super-after-delete after-delete]
	      [super-after-insert after-insert]
	      [super-do-paste do-paste]
	      [super-do-copy do-copy]
	      [super-write-footers-to-file write-footers-to-file]
	      [super-read-footer-from-file read-footer-from-file])
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
	[find-snip-by-XXX
	 (lambda (id get)
	   (let/ec found
	     (for-each-snip
	      (lambda (s)
		(when (equal? id (get s))
		  (found s))))
	     #f))]
	[find-snip-by-id
	 (lambda (id)
	   (find-snip-by-XXX id (make-generic gb:snip% id)))]
	[find-snip-by-original-id
	 (lambda (id)
	   (find-snip-by-XXX id (make-generic gb:snip% original-id)))]
	[find-snip-by-name
	 (lambda (id)
	   (find-snip-by-XXX id (make-generic gb:snip% name)))]

	[top-resized
	 (lambda (snip old-w old-h w h)
	   (when (eq? snip main-panel)
		 (unless (= top-level-type PANEL-MODE)
			 (invalidate-bitmap-cache 0 0
						  (+ (max old-w w) (* 2 margin))
						  (+ (max old-h h) (* 2 margin)
						     (or frame-label-h 0) 2)))))])

      (override
	[on-move-to
	 (lambda (snip x y dragging?)
	   (or (not (eq? snip main-panel))
	       (and (= x main-panel-x)
		    (= y main-panel-y))))]
	[after-move-to
	 (lambda (snip x y dragging?)
	   (when dragging?
	     (send snip gb-drag-children-along x y)))]
	[after-resize
	 (lambda (snip w h did?)
	   (when (and (eq? snip main-panel) did?)
		 (unless (= top-level-type PANEL-MODE)
			 (invalidate-bitmap-cache 
			  0 0 last-frame-paint-w last-frame-paint-h))))]
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
	   ;; This doesn't really work very well.
	   '(let ([parent (send snip gb-get-parent)])
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
		     (set! cur-hilite-pos (send s gb-find-position lx ly)))
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
	   (handle-new-arrivals))])
      (public
	[handle-new-arrivals
	 (lambda ()
	   (let loop ()
	     ((let/ec k
		(for-each-snip 
		 (lambda (s) 
		   (when (send s gb-reconnect-to-original-children)
		     (k loop))))
		void)))
	   (for-each-snip (lambda (s) (send s gb-forget-original-id))))])
      (override
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
	    (lambda () (set! copying? #f))))])
      (public
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
			 (or (ivar se parent)
			     main-panel))])
	     (send s gb-add-child i)
	     (set-selected s)))])
      (private
       [frame-label "Frame"]
       [frame-label-w #f]
       [frame-label-h #f]
       [last-frame-paint-w 0]
       [last-frame-paint-h 0]
       [main-panel-x 0]
       [main-panel-y 0]
       [margin 2]
       [top-level-type FRAME-MODE]
       [auto-show? #f]
       [configure-frame #f])
      (public
        [get-top-level-type
	 (lambda () top-level-type)]
	[get-auto-show
	 (lambda () auto-show?)]
	[get-frame-label
	 (lambda () frame-label)]
        [open-dialog
	 (lambda ()
	   (unless configure-frame
	     (set! configure-frame (make-object 
				    (class-asi mred:frame%
				       (rename [super-on-close on-close])
				       (public
					[on-close
					 (lambda ()
					   (and (super-on-close)
						(set! configure-frame #f)))]))
				    null "Output"))
	     (let ([p (make-object mred:vertical-panel% configure-frame)])
	       (send p minor-align-left)
	       (letrec ([update-frame
			 (lambda ()
			   (send main-panel gb-need-recalc-size)
			   (invalidate-bitmap-cache 0 0 -1 -1))]
			[kind-choice
			 (make-object mred:choice% p
				      (lambda (c e)
					(let ([mode (send c get-selection)])
					  (set! top-level-type mode)
					  (send frame-stuff enable (< mode PANEL-MODE))
					  (update-frame)))
				      "Output:"
				      -1 -1 -1 -1
				      '("Frame" "Modal Dialog" "Non-modal Dialog" "Panel"))]
			[frame-stuff (make-object mred:vertical-panel% p)]
			[title-text (make-one-line/callback-edit 
				     frame-stuff
				     "Frame Title:"
				     (lambda (txt)
				       (unless (string=? frame-label txt)
					       (set! frame-label txt)
					       (let ([w frame-label-w]
						     [h frame-label-h])
						 (set! frame-label-h #f)
						 (update-frame))))
				     frame-label)]
			[auto-show-check (make-object mred:check-box% frame-stuff
						      (lambda (c e)
							(set! auto-show? (send c get-value)))
						      "Show Frame Automatically")])
		 (send frame-stuff minor-align-left)
		 (send frame-stuff enable (< top-level-type PANEL-MODE))
		 (send kind-choice stretchable-width #f)
		 (send kind-choice set-selection top-level-type)
		 (send auto-show-check set-value auto-show?))))
	   (send configure-frame show #t))]
        [get-main-location
	 (lambda (snip dc dx dy)
	   (when (eq? snip main-panel)
		 (if (= top-level-type PANEL-MODE)
		     (begin
		       (set! main-panel-x 0)
		       (set! main-panel-y 0))
		     (begin
		       (unless frame-label-h
			 (let ([wb (box 0)]
			       [hb (box 0)])
			   (send dc get-text-extent frame-label wb hb null null top-font)
			   (set! frame-label-w (unbox wb))
			   (set! frame-label-h (unbox hb))))
		       (set! main-panel-x margin)
		       (set! main-panel-y (+ frame-label-h 2 margin))))
		 (set-box! dx main-panel-x)
		 (set-box! dy main-panel-y)))])
      (override
	[on-paint
	 (lambda (pre? dc l t r b dx dy show-caret?)
	   (unless (or (not pre?) (= top-level-type PANEL-MODE)
		       (not main-panel))
	      (let ([tb (box 0)]
		    [lb (box 0)]
		    [bb (box 0)]
		    [rb (box 0)])
		(get-snip-location main-panel lb tb #f)
		(get-snip-location main-panel rb bb #t)
		(let* ([w (- (unbox rb) (unbox lb))]
		       [h (- (unbox bb) (unbox tb))]
		       [th (+ (or frame-label-h 0) 2)]
		       [tw (+ (* 2 margin) w)]
		       [totalh (+ th (* 2 margin) h)])
		  (when (and (or (<= 0 l tw) (<= 0 r tw) (<= l 0 tw r))
			     (or (<= 0 t totalh) (<= 0 b totalh) (<= t 0 totalh b)))
		    (set! last-frame-paint-w tw)
		    (set! last-frame-paint-h totalh)
		    (send dc draw-rectangle dx dy 
			  tw totalh)
		    (send dc draw-line dx (+ dy th)
			  (+ dx tw -1) (+ dy th))
		    (with-clipping-region 
		     dc (add1 dx) (add1 dy) 
		     (+ tw -2) (- th 2)
		     (lambda ()
		       (let ([f (send dc get-font)])
			 (send dc set-font f)
			 (send dc draw-text frame-label 
			       (+ dx (/ (- tw frame-label-w) 2)) 
			       (+ dy 1))
			 (send dc set-font f)))))))))]
	[write-footers-to-file
	 (lambda (stream)
	   (super-write-footers-to-file stream)
	   (let ([out (lambda (name val)
			(let ([info (box 0)])
			  (begin-write-header-footer-to-file stream name info)
			  (send stream << val)
			  (end-write-header-footer-to-file stream (unbox info))))])
	     (out "gb:mode" top-level-type)
	     (out "gb:title" frame-label)
	     (out "gb:show" (if auto-show? 1 0))))]
	[read-footer-from-file
	 (lambda (stream kind)
	   (cond
	     [(string=? kind "gb:mode") 
	      (set! top-level-type 
		    (min -LAST-MODE- 
			 (max -FIRST-MODE- (send stream get-exact))))]
	     [(string=? kind "gb:title") 
	      (set! frame-label (send stream get-string))]
	     [(string=? kind "gb:show") 
	      (set! auto-show? (positive? (send stream get-exact)))]
	     [else (super-read-footer-from-file stream kind)]))])
      (private
	[main-panel #f])
      (public
	[get-main-panel (lambda () main-panel)]
	[create-main-panel
	 (lambda () 
	   (erase)
	   (set! main-panel (make-object gb:panel-snip%))
	   (insert main-panel 0 0)
	   (resize main-panel START-FRAME-WIDTH START-FRAME-HEIGHT)
	   (send main-panel gb-install this #f)
	   (send main-panel set-id "0")
	   (send main-panel gb-need-recalc-size)
	   (set-modified #f))])
      (override
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
  
  (define-struct tool (icon callback active?))

  (define lg-pen (send mred:the-pen-list find-or-create-pen 
		       (make-object mred:color% 200 200 200) 0 'solid))

  (define dg-pen (send mred:the-pen-list find-or-create-pen 
		       (make-object mred:color% 140 140 140) 0 'solid))

  (define icons (make-hash-table))

  (define toolbar%
    (class mred:canvas% (parent)
      (inherit min-height stretchable-height get-dc)
      (private
       [margin 2]
       [icon-size 16]
       [tools null]
       [active-tool #f]
       [deactivate-tool
	(lambda ()
	  (when active-tool
		(set-tool-active?! active-tool #f)
		(set! active-tool #f)
		(on-paint)))]
       [activate-tool
	(lambda (mx my only)
	  (let ([y 0]
		[h (+ icon-size (* 2 margin))]
		[w (+ icon-size (* 2 margin))])
	    (let loop ([l tools][x 0])
	      (unless (null? l)
		 (if (and (<= x mx (+ x w)) (<= y my (+ y h))
			  (or (not only) (eq? (car l) only)))
		     (begin
		       (set! active-tool (car l))
		       (set-tool-active?! active-tool #t)
		       (on-paint))
		     (loop (cdr l) (+ x w)))))))]
       [can-drag #f])
      (override
       [on-paint
	(lambda ()
	  (let ([dc (get-dc)]
		[y 0]
		[h (+ icon-size (* 2 margin))]
		[w (+ icon-size (* 2 margin))])
	    (let loop ([l tools][x 0])
	      (unless (null? l)
		(let ([tool (car l)])
		  (let ([p (send dc get-pen)]
			[on? (tool-active? tool)])
		    (send dc set-pen (if on? dg-pen lg-pen))
		    (send dc draw-line x y (+ x w -1) y)
		    (send dc draw-line x y x (+ y h -1))
		    (send dc draw-line x (add1 y) (+ x w -2) (add1 y))
		    (send dc draw-line (add1 x) y (add1 x) (+ y h -2))
		    (send dc set-pen (if on? lg-pen dg-pen))
		    (send dc draw-line (+ x 1) (+ y h -1) (+ x w -1) (+ y h -1))
		    (send dc draw-line (+ x w -1) (+ y 1) (+ x w -1) (+ y h -1))
		    (send dc draw-line (+ x 2) (+ y h -2) (+ x w -2) (+ y h -2))
		    (send dc draw-line (+ x w -2) (+ y 2) (+ x w -2) (+ y h -2))
		    (send dc set-pen p))
		  (if (tool-icon tool)
		      (send dc draw-bitmap (tool-icon tool) (+ x margin) margin)
		      (send dc draw-rectangle (+ x margin) margin icon-size icon-size)))
		(loop (cdr l) (+ x w))))))]
       [on-event
	(lambda (e)
	  (cond
	   [(send e button-down?)
	    (deactivate-tool)
	    (activate-tool (send e get-x) (send e get-y) #f)
	    (set! can-drag active-tool)]
	   [(send e button-up?)
	    (set! can-drag #f)
	    (when active-tool
		  (let ([cb (tool-callback active-tool)])
		    (deactivate-tool)
		    (cb)))]
	   [(send e dragging?)
	    (when can-drag
		  (let ([old-active active-tool])
		    (set! active-tool #f)
		    (activate-tool (send e get-x) (send e get-y) can-drag)
		    (when (and (not active-tool) old-active)
			  (set-tool-active?! old-active #f)
			  (on-paint))))]
	   [else (set! can-drag #f) 
		 (deactivate-tool)]))])
      (public
       [append-tool
	(lambda (icon-name cb)
	  (let* ([name (string->symbol icon-name)]
		 [icon
		  (hash-table-get 
		   icons name
		   (lambda ()
		     (let* ([icon (make-object mred:bitmap% 
					       (build-path (collection-path "guibuilder") icon-name)
					       'xpm)])
		       (if (send icon ok?) 
			   icon
			   #f))))])
	    (hash-table-put! icons name icon)
	    (set! tools (append tools (list (make-tool icon cb #f))))))])
      (sequence
	(super-init parent)
	(min-height (+ icon-size (* margin 2)))
	(stretchable-height #f))))

  (define gb:frame%
    (class framework:frame:pasteboard-info-file% ([file #f])
      (inherit get-editor show get-area-container get-menu-bar)
      (rename [super-get-menu-bar% get-menu-bar%]
	      [super-help-menu:after-about help-menu:after-about])
      (private)
      (override
        [help-menu:about-string "GUI Builder"]
	[help-menu:about
	 (lambda (evt)
	   (mred:message-box (format "Version ~a.~a, Copyright (c) 1997, PLT (Matthew Flatt)~n"
				     GB:SNIP-VERSION
				     MINOR-VERSION)
			     "About GUI Builder"))]
	[help-menu:after-about
	 (lambda (help-menu)
	   (send help-menu append-item "GUI Builder Help"
		 (lambda ()
		   (let ([f (framework:handler:edit-file (build-path (collection-path "guibuilder") "help.mre"))])
		     (send (send f get-editor) lock #t))))
	   (super-help-menu:after-about help-menu))]

	[get-editor% (lambda () gb:edit%)])

      (public
	[instantiate
	 (lambda ()
	   (let ([cl (eval (build-code #t))])
	     (thread
	      (lambda ()
		(parameterize ([mred:current-eventspace (mred:make-eventspace)])
		   (make-object cl))))))]
	[view-source
	 (lambda ()
	   (let ([port (open-output-string)])
	     (pretty-print (build-code #f) port)
	     (let ([f (make-object framework:frame:editor%)])
	       (send (send f get-editor) insert (get-output-string port)))))]
	[build-code
	 (lambda (force-frame?)
	   (let* ([edit (get-editor)]
		  [main (send edit get-main-panel)]
		  [type (send edit get-top-level-type)]
		  [frame-label (if (and (= type PANEL-MODE) force-frame?)
				   "Panel Tester"
				   (send edit get-frame-label))])
	     `(class null ,(if (and (= type PANEL-MODE) (not force-frame?))
			       '(top) 
			       '())
		(private
		 ,@(get-class-functions))
		(public
		 ,@(cond
		    [(or (= type FRAME-MODE) 
			 (and (= type PANEL-MODE) force-frame?))
		     `([get-top% (lambda () mred:frame%)]
		       [top (make-object (get-top%) null ,frame-label)])]
		    [(= type PANEL-MODE) null]
		    [else
		     `([get-top% (lambda () mred:dialog-box%)]
		       [top (make-object (get-top%) null ,frame-label
					 ,(= type MODAL-DIALOG-MODE)
					 -1 -1 10 10)])])
		 ,@(send main gb-instantiate 'top))
		,@(if (and (not force-frame?)
			   (or (= type PANEL-MODE) (not (send edit get-auto-show))))
		      null
		      '((sequence
			  (send top show #t)))))))]
	[get-class-functions
	 (lambda ()
	   (let ([ht (make-hash-table)])
	     (send (get-editor) for-each-snip
		   (lambda (s)
		     (for-each 
		      (lambda (l)
			(hash-table-put! ht l #t))
		      (send s gb-get-class-defines))))
	     (hash-table-map ht (lambda (key value) key))))]

	[toolbar #f]
	[init-tools
	 (lambda (mb)
	   (set! toolbar (make-object toolbar% (get-area-container)))
	   (send (get-area-container) change-children
		 (lambda (l)
		   (cons toolbar (remove toolbar l))))
	   
	   (let* ([emenu (make-object mred:menu% "Element" mb)]
		  [append-element-type
		   (lambda (name icon c%)
		     (let ([maker (lambda (i e) (insert-element c%))])
		       (send toolbar append-tool icon maker)
		       (make-object mred:menu-item% name emenu maker)))]
		  [vmenu (make-object mred:menu% "Output" mb)])
	     (make-object mred:menu-item% "Configure Selected" emenu
			  (lambda (i e)
			    (send (get-editor)
				  for-each-selected-snip
				  (lambda (s)
				    (send s gb-open-dialog)))))
	     (make-object mred:separator-menu-item% emenu)
	     (append-element-type "New Vertical Panel" "vpanel.xpm" gb:vertical-panel-snip%)
	     (append-element-type "New Horizontal Panel" "hpanel.xpm" gb:horizontal-panel-snip%)
	     (append-element-type "New Message Label" "message.xpm" gb:message-snip%)
	     (append-element-type "New Button" "button.xpm" gb:button-snip%)
	     (append-element-type "New Checkbox" "checkbox.xpm" gb:check-box-snip%)
	     (append-element-type "New Text Field" "text.xpm" gb:text-snip%)
	     (append-element-type "New List" "list.xpm" gb:list-box-snip%)
	     (append-element-type "New Radiobox" "radiobox.xpm" gb:radio-box-snip%)
	     (append-element-type "New Choice" "choice.xpm" gb:choice-snip%)
	     (append-element-type "New Slider" "slider.xpm" gb:slider-snip%)
	     (append-element-type "New Gauge" "gauge.xpm" gb:gauge-snip%)
	     (append-element-type "New Canvas" "canvas.xpm" gb:canvas-snip%)
	     (append-element-type "New Media Canvas" "mcanvas.xpm" gb:media-canvas-snip%)
	     
	     (make-object mred:menu-item% "Configure Output"  vmenu
			  (lambda (i e) (send (get-editor) open-dialog)))
	     (make-object mred:separator-menu-item% vmenu)
	     (make-object mred:menu-item%  "Make Sample Window" vmenu (lambda (i e) (instantiate)))
	     (make-object mred:menu-item%  "Make Source Code" vmenu (lambda (i e) (view-source)))))]
	[insert-element
	 (lambda (c%)
	   (let ([e (get-editor)])
	     (send e insert-element c%)))])
      (sequence
	(super-init (or file "GUI Builder"))

	(init-tools (get-menu-bar))

	(let ([file (and file (normalize-path file))])
	  (unless (and file (file-exists? file) (send (get-editor) load-file file))
	      (send (get-editor) create-main-panel)
	      (when file
		    (send (get-editor) set-filename file))))
	(show #t))))
  
  (framework:handler:insert-format-handler "GUI Builder" "gui"
					   (lambda (file)
					     (make-object gb:frame% file)))
  
  (define (new-gui-builder-frame) (make-object gb:frame%))

  (new-gui-builder-frame))
