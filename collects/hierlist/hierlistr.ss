(unit/sig hierlist^
  (import mred^)

  (define transparent (make-object brush% "WHITE" 'transparent))
  (define transparent-pen (make-object pen% "WHITE" 1 'transparent))
  (define black-xor-pen (make-object pen% "BLACK" 1 'xor))
  (define red (make-object brush% "RED" 'solid))
  (define blue (make-object brush% "BLUE" 'solid))
  (define black-xor (make-object brush% "BLACK" 'xor))
  (define arrow-cursor (make-object cursor% 'arrow))

  ; Hack for implementing auto-wrapping items:
  (define arrow-size 0)

  ; Private arrow snip class:
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

  ; Hack to get whitespace matching width of arrow: derive a new
  ; class that overrides the `draw' method to do nothing. 
  (define whitespace-snip%
    (class arrow-snip% ()
      (override [draw void])
      (sequence (super-init void))))

  ; 
  (define item-keymap (make-object keymap%))

  (send item-keymap add-function "mouse-select"
	(lambda (edit event) (if (send event button-down?) (send edit select #t))))
  (send item-keymap add-function "mouse-double-select"
	(lambda (edit event) (if (send event button-down?) (send edit double-select))))

  (send item-keymap map-function "leftbutton" "mouse-select")
  (send item-keymap map-function "leftbuttondouble" "mouse-double-select")

  (define hierarchical-list-item<%>
    (interface ()
      get-editor is-selected? select user-data))

  (define hierarchical-list-item%
    (class* object% (hierarchical-list-item<%>) (snip)
      (private
	[data #f])
      (public
	[get-editor (lambda () (send snip get-item-buffer))]
	[is-selected? (lambda () (send snip is-selected?))]
	[select (lambda (on?) (send snip select on?))]
	[scroll-to (lambda () (send (send snip get-admin)
				    scroll-to
				    snip
				    0 0 0 10 #t))]
	[user-data (case-lambda [() data][(x) (set! data x)])])
      (sequence (super-init))))

  (define hierarchical-list-compound-item<%>
    (interface (hierarchical-list-item<%>)
      new-item new-list delete-item get-items))

  (define hierarchical-list-compound-item%
    (class* hierarchical-list-item% (hierarchical-list-compound-item<%>) (snip)
      (override
	[get-editor (lambda () (send snip get-title-buffer))])
      (public
	[new-item (lambda () 
		    (begin0
		     (send (send snip get-content-buffer) new-item)
		     (send snip not-empty-anymore)))]
	[new-list (lambda () 
		    (begin0 
		     (send (send snip get-content-buffer) new-list)
		     (send snip not-empty-anymore)))]
	[delete-item (lambda (i) (begin0
				  (send (send snip get-content-buffer) delete-item i)
				  (send snip check-empty-now)))]
	[get-items (lambda () (send (send snip get-content-buffer) get-items))])
      (sequence
	(super-init snip))))

  ; Buffer for a single list item
  (define hierarchical-item-text%
    (class text% (top top-select item snip depth)
      (inherit set-max-undo-history hide-caret
	       last-position set-position set-keymap
	       invalidate-bitmap-cache set-max-width
	       get-view-size)
      (rename [super-auto-wrap auto-wrap])
      (private
	[selected? #f])
      (public
	[is-selected? (lambda () selected?)]
	[show-select (lambda (on?)
		       (set! selected? on?)
		       (invalidate-bitmap-cache))])
      (override
	[auto-wrap (case-lambda
		    [() (super-auto-wrap)]
		    [(on?) (super-auto-wrap on?)
			   (when on?
			     (let ([wbox (box 0)])
			       (send (send top get-editor) get-view-size wbox (box 0))
			       ;; These icky constants should be eliminated
			       (let ([w (- (unbox wbox) 8 (* depth arrow-size))])
				 (set-max-width (if (positive? w)
						    w
						    'none)))))])]
	[on-paint
	 (lambda (pre? dc left top_ right bottom dx dy caret)
	   (when (and (not pre?) selected?)
	     (let ([b (send dc get-brush)]
		   [p (send dc get-pen)]
		   [filled? (or (not (send top show-focus))
				(send top has-focus?))])
	       (unless filled?
		 ; To draw the right outline, we need the display area
		 (set! left 0)
		 (set! top_ 0)
		 (let ([wbox (box 0)]
		       [hbox (box 0)])
		   (get-view-size wbox hbox)
		   (set! right (unbox wbox))
		   (set! bottom (unbox hbox))))
	       (send dc set-brush (if filled? black-xor transparent))
	       (send dc set-pen (if filled? transparent-pen black-xor-pen))
	       (send dc draw-rectangle (+ dx left) (+ dy top_) (- right left) (- bottom top_))
	       (send dc set-pen p)
	       (send dc set-brush b))))])
      (public
	[select (lambda (on?)
		  (unless (eq? (not selected?) (not on?))
		    (top-select (if on? item #f) snip)))]
	[double-select (lambda () (send top on-double-select item))]
	[select-prev (lambda () (send top select-prev))])
      (override
	[on-default-char void]
	[on-default-event void])
      (sequence
	(super-init)
	(hide-caret #t)
	(set-max-undo-history 0)
	(set-keymap item-keymap))))

  ; Buffer for a compound list item (and the top-level list)
  (define (make-hierarchical-list-text% super%)
    (class super% (top top-select depth)
      (inherit set-max-undo-history hide-caret
	       last-position insert delete line-start-position line-end-position
	       begin-edit-sequence end-edit-sequence get-style-list)
      (private
	[children null]
	[make-whitespace (lambda () (make-object whitespace-snip%))]
	[insert-item 
	 (lambda (snip% whitespace?)
	   (let ([s (make-object snip% top top-select (add1 depth))])
	     (begin-edit-sequence)
	     (unless (null? children)
	       (insert #\newline (last-position)))
	     (when whitespace? (insert (make-whitespace) (last-position)))
	     (insert s (last-position))
	     (end-edit-sequence)
	     (set! children (append children (list s)))
	     (send s get-item)))])
      (public
	[deselect-all 
	 (lambda () (for-each (lambda (x) (send x deselect-all)) children))]
	[new-item 
	 (lambda () (insert-item hierarchical-item-snip% #t))]
	[new-list
	 (lambda () (insert-item hierarchical-list-snip% #f))]
	[get-items (lambda () (map (lambda (x) (send x get-item)) children))]
	[delete-item
	 (lambda (i)
	   (let loop ([pos 0][l children][others null])
	     (cond
	      [(null? l) (error 'hierarchical-list-compound-item::delete-item "item not found: ~a" i)]
	      [(eq? (send (car l) get-item) i)
	       (send (car l) deselect-all)
	       (set! children (append (reverse others) (cdr l)))
	       (let ([s (line-start-position pos)]
		     [e (line-end-position pos)])
		 (delete (if (zero? s) s (sub1 s)) (if (zero? s) (add1 e) e)))]
	      [else (loop (add1 pos) (cdr l) (cons (car l) others))])))]
	[reflow-items
	 (lambda ()
	   (for-each
	    (lambda (c)
	      (send c reflow-item))
	    children))])
      (override
	[on-default-char void]
	[on-default-event void])
      (sequence
	(super-init)
	(hide-caret #t)
	(set-max-undo-history 0))))

  (define hierarchical-list-text% (make-hierarchical-list-text% text%))

  ; Snip for a single list item
  (define hierarchical-item-snip%
    (class editor-snip% (top top-select depth)
      (public
	[get-item-text% (lambda () hierarchical-item-text%)]
	[select (lambda (on?) (send item-buffer select on?))]
	[deselect-all (lambda () (select #f))]
	[show-select (lambda (on?) (send item-buffer show-select on?))]
	[get-item-buffer (lambda () item-buffer)]
	[get-item (lambda () item)]
	[reflow-item (lambda () 
		       (when (send item-buffer auto-wrap)
			 (send item-buffer auto-wrap #t)))])
      (private
	[item (make-object hierarchical-list-item% this)]
	[item-buffer (make-object (get-item-text%) top top-select item this depth)])
      (sequence
	(super-init item-buffer #f 0 0 0 0 0 0 0 0))))

  ; Snip for a compound list item
  (define hierarchical-list-snip%
    (class editor-snip% (top top-select depth [title #f][content #f])
      (public
	[get-main-text% (lambda () (class text% args
				     (override
				       [on-default-char void]
				       [on-default-event void])
				     (sequence 
				       (apply super-init args))))]
	[get-title-text% (lambda () hierarchical-item-text%)]
	[get-content-text% (lambda () hierarchical-list-text%)]
	[get-arrow-snip% (lambda () arrow-snip%)]
	[select (lambda (on?)
		  (if on?
		      (send title-buffer select #t)
		      (send title-buffer select #f)))]
	[deselect-all (lambda ()
			(select #f)
			(send content-buffer deselect-all))]
	[show-select (lambda (on?) (send title-buffer show-select on?))]
	[not-empty-anymore (lambda ()
			     (when was-empty?
				   (set! was-empty? #f)
				   (set! was-non-empty? #t)
				   (send main-buffer begin-edit-sequence)
				   (send main-buffer insert #\newline 2)
				   (send main-buffer insert whitespace 3)
				   (send main-buffer insert content-snip 4)
				   (send main-buffer end-edit-sequence)))]
	[check-empty-now (lambda ()
			   (when (and was-non-empty? 
				      (zero? (send content-buffer last-position)))
				 (set! was-empty? #t)
				 (set! was-non-empty? #f)
				 (send main-buffer delete 2 5)))]
	[on-arrow (lambda (a)
		    (if (send a on)
			(begin
			  (send main-buffer begin-edit-sequence)
			  (send top on-item-opened (get-item))
			  (if (zero? (send content-buffer last-position))
			      (set! was-empty? #t)
			      (begin
				(set! was-non-empty? #t)
				(send main-buffer insert #\newline 2)
				(send main-buffer insert whitespace 3)
				(send main-buffer insert content-snip 4)))
			  (send main-buffer end-edit-sequence))
			(begin
			  (set! was-empty? #f)
			  (set! was-non-empty? #f)
			  (send main-buffer begin-edit-sequence)
			  (send content-buffer deselect-all)
			  (send main-buffer delete 2 5)
			  (send top on-item-closed (get-item))
			  (send main-buffer end-edit-sequence))))]
	[get-title-buffer (lambda () title-buffer)]
	[get-content-buffer (lambda () content-buffer)]
	[get-item (lambda () item)]
	[reflow-item (lambda () 
		       (when (send title-buffer auto-wrap)
			 (send title-buffer auto-wrap #t))
		       (send (send content-snip get-editor) reflow-items))])
      (private
        [was-empty? #f]
	[was-non-empty? #f]
	[item (make-object hierarchical-list-compound-item% this)]
	[main-buffer (make-object (get-main-text%))]
	[title-buffer (make-object (get-title-text%) top top-select item this depth)]
	[content-buffer (make-object (get-content-text%) top top-select depth)]
	[title-snip (make-object editor-snip% title-buffer #f 0 0 0 0 0 0 0 0)]
	[content-snip (make-object editor-snip% content-buffer #f 4 0 0 0 0 0 0 0)]
	[arrow (make-object arrow-snip% on-arrow)]
	[whitespace (make-object whitespace-snip%)])
      (sequence
	(super-init main-buffer #f 0 0 0 0 0 0 0 0)
	(send main-buffer set-max-undo-history 0)
	(send main-buffer hide-caret #t)
	(send main-buffer insert arrow)
	(when title (send title-buffer insert title))
	(when content (send content-buffer insert content))
	(send main-buffer insert title-snip)
	(send main-buffer change-style (make-object style-delta% 'change-alignment 'top) 0 2))))

  (define list-keymap (make-object keymap%))

  (send list-keymap add-function "select-prev"
	(lambda (list event) (send list select-prev)))
  (send list-keymap add-function "select-next"
	(lambda (list event) (send list select-next)))
  (send list-keymap add-function "select-first"
	(lambda (list event) (send list select-first)))
  (send list-keymap add-function "select-last"
	(lambda (list event) (send list select-last)))
  (send list-keymap add-function "page-up"
	(lambda (list event) (send list page-up)))
  (send list-keymap add-function "page-down"
	(lambda (list event) (send list page-down)))

  (send list-keymap map-function "up" "select-prev")
  (send list-keymap map-function "down" "select-next")
  (send list-keymap map-function "home" "select-first")
  (send list-keymap map-function "end" "select-last")
  (send list-keymap map-function "pageup" "page-up")
  (send list-keymap map-function "pagedown" "page-down")

  (define hierarchical-list%
    (class editor-canvas% (parent)
      (inherit min-width min-height)
      (rename [super-on-char on-char]
	      [super-on-focus on-focus])
      (public
	[selectable
	 (case-lambda
	  [() selectable?]
	  [(on?) (set! selectable? on?)])]
	[get-selected (lambda () selected-item)]
	[on-item-opened void]
	[on-item-closed void]
	[on-double-select void]
	[on-select void]
	[new-item (lambda () (send top-buffer new-item))]
	[new-list (lambda () (send top-buffer new-list))]
	[delete-item (lambda (i) (send top-buffer delete-item i))]
	[get-items (lambda () (send top-buffer get-items))]
	[select-next (lambda () (move +1))]
	[select-prev (lambda () (move -1))]
	[select-first (lambda () (let ([l (get-items)])
				   (unless (null? l)
				     (send (car l) select #t)
				     (send (car l) scroll-to))))]
	[select-last (lambda () (let loop ([l (get-items)])
				  (cond
				   [(null? l) (void)]
				   [(null? (cdr l))
				    (send (car l) select #t)
				    (send (car l) scroll-to)]
				   [else (loop (cdr l))])))]
	[page-up (lambda () (page 'up))]
	[page-down (lambda () (page 'down))]
	[show-focus
	 (case-lambda
	  [() show-focus?]
	  [(on?) (set! show-focus? on?)])])
      (override
	[on-char
	 (lambda (e)
	   (unless (send list-keymap handle-key-event this e)
	     (super-on-char e)))]
	[on-size
	 (lambda (w h)
	   (send top-buffer begin-edit-sequence)
	   (send top-buffer reflow-items)
	   (send top-buffer end-edit-sequence))]
	[on-focus
	 (lambda (on?)
	   (when (and selected show-focus?)
	     (send selected show-select #t))
	   (super-on-focus on?))])
      (private
	[move (lambda (dir) 
		(define (find i l)
		  (let loop ([l l][pos 0])
		    (if (null? l)
			#f
			(if (eq? (car l) i)
			    pos
			    (loop (cdr l) (add1 pos))))))
		(let* ([l (get-items)]
		       [pos (if selected-item
				(+ dir (find selected-item l))
				(if (negative? dir)
				    (sub1 (length l))
				    0))])
		  (when (< -1 pos (length l))
		    (let ([i (list-ref l pos)])
		      (send i select #t)
		      (send i scroll-to)))))]
	[page (lambda (dir)
		(let ([items (get-items)])
		  (unless (null? items)
		    (let ([sbox (box 0)]
			  [ebox (box 0)])
		      (send top-buffer get-visible-line-range sbox ebox)
		      (let* ([len (max 1 (sub1 (- (unbox ebox) (unbox sbox))))]
			     [l (if (eq? dir 'up)
				    (max 0 (- (unbox sbox) len))
				    (min (sub1 (length items)) (+ (unbox ebox) len)))]
			     [i (list-ref items l)])
			(send i select #t)
			(send i scroll-to))))))]
	[selectable? #t]
	[show-focus? #f]
	[do-select (lambda (item s)
		     (when selectable?
		       (unless (eq? item selected-item)
			 (when selected (send selected show-select #f))
			 (set! selected (if item s #f))
			 (set! selected-item item)
			 (when selected (send selected show-select #t))
			 (on-select item))))]
	[top-buffer (make-object hierarchical-list-text% this do-select 0)]
	[selected #f]
	[selected-item #f])
      (sequence
	(super-init parent top-buffer '(no-hscroll))
	(send top-buffer set-cursor arrow-cursor) 
	(min-width 150)
	(min-height 200)))))
