(unit/sig mred:hier:hierlist^
  (import (mred : mred^))

  (define transparent (make-object wx:brush% "WHITE" wx:const-transparent))
  (define transparent-pen (make-object wx:pen% "WHITE" 1 wx:const-transparent))
  (define gray (make-object wx:brush% "GREY" wx:const-solid))
  (define blue (make-object wx:brush% "BLUE" wx:const-solid))
  (define black (make-object wx:brush% "BLACK" wx:const-solid))
  (define arrow-cursor (make-object wx:cursor% wx:const-cursor-arrow))

  ; Private arrow snip class:
  (define arrow-snip-class (make-object wx:snip-class%))
  (send arrow-snip-class set-classname "hier-arrow")
  (define arrow-snip%
    (class wx:snip% (click-callback)
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
	     (let* ([voffset (floor (/ d 2))]
		    [s (floor (- h d a))]
		    [sz (if (even? s) s (sub1 s))]
		    [offset (ceiling (* (/ (- 1 width-fraction) 2) sz))]
		    [width (floor (* width-fraction sz))])
	       (set! right-points (list (make-object wx:point% offset voffset)
					(make-object wx:point% offset (+ voffset sz))
					(make-object wx:point% (+ offset width) (+ voffset (quotient sz 2)))))
	       (set! down-points 
		     (list (make-object wx:point% 0 (+ voffset offset))
			   (make-object wx:point% sz (+ voffset offset))
			   (make-object wx:point% (quotient sz 2) (+ width offset voffset)))))))])
      (private
	[get-width (lambda () (+ 2 size))]
	[get-height (lambda () size)]
	[clicked? #f]
	[update
	 (lambda ()
	   (send (get-admin) needs-update this 0 0 (get-width) (get-height)))])
      (public
	[get-extent (lambda (dc x y w h descent space lspace rspace)
		      (super-get-extent dc x y w h descent space lspace rspace)
		      (unless size (set-sizes dc))
		      (unless (null? w) (set-box! w (get-width)))
		      (unless (null? h) (set-box! h (get-height)))
		      (unless (null? descent) (set-box! descent 0))
		      (unless (null? space) (set-box! space 0)))]
	[partial-offset (lambda (dc x y len)
			  (unless size (set-sizes dc))
			  (if (zero? len) 0 size))]
	[draw (lambda (dc x y left top right bottom dx dy draw-caret)
		(unless size (set-sizes dc))
		(let ([b (send dc get-brush)])
		  (send dc set-brush (if clicked? blue gray))
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
	[on (case-lambda 
	     [(v) (set! on? v) (send (get-admin) needs-update this 0 0 (add1 size) (add1 size))]
	     [() on?])]
	[copy (lambda () (make-object arrow-snip% click-callback))])
      (sequence
	(super-init)
	(set-snipclass arrow-snip-class)
	(set-count 1)
	(set-flags (+ (get-flags) wx:const-snip-handles-events)))))

  ; Hack to get whitespace matching width of arrow: derive a new
  ; class that overrides the `draw' method to do nothing. 
  (define whitespace-snip%
    (class arrow-snip% ()
      (public [draw void])
      (sequence (super-init void))))

  ; 
  (define item-keymap (make-object wx:keymap%))
  (send item-keymap add-mouse-function "select"
	(lambda (edit event) (if (send event button-down?) (send edit select #t))))
  (send item-keymap add-mouse-function "double-select"
	(lambda (edit event) (if (send event button-down?) (send edit double-select))))
  (send item-keymap map-function "leftbutton" "select")
  (send item-keymap map-function "leftbuttondouble" "double-select")

  (define hierarchical-list-item%
    (class null (snip)
      (private
	[data #f])
      (public
	[get-buffer (lambda () (send snip get-item-buffer))]
	[is-selected? (lambda () (send snip is-selected?))]
	[select (lambda (on?) (send snip select on?))]
	[user-data (case-lambda [() data][(x) (set! data x)])])))

  (define hierarchical-list-compound-item%
    (class hierarchical-list-item% (snip)
      (public
	[get-buffer (lambda () (send snip get-title-buffer))]
	[new-item (lambda () (send (send snip get-content-buffer) new-item))]
	[new-list (lambda () (send (send snip get-content-buffer) new-list))]
	[delete-item (lambda (i) (send (send snip get-content-buffer) delete-item i))]
	[get-items (lambda () (send (send snip get-content-buffer) get-items))])
      (sequence
	(super-init snip))))

  ; Buffer for a single list item
  (define mred:hierarchical-item-buffer%
    (class wx:media-edit% (top top-select item snip)
      (inherit set-max-undo-history hide-caret
	       last-position set-position set-keymap
	       invalidate-bitmap-cache)
      (private
	[selected? #f])
      (public
	[is-selected? (lambda () selected?)]
	[show-select (lambda (on?)
		       (set! selected? on?)
		       (invalidate-bitmap-cache))]
	[on-paint
	 (lambda (pre? dc left top right bottom dx dy caret)
	   (if (and (not pre?) selected?)
	       (let ([b (send dc get-brush)]
		     [p (send dc get-pen)]
		     [f (send dc get-logical-function)])
		 (send dc set-brush black)
		 (send dc set-pen transparent-pen)
		 (send dc set-logical-function wx:const-xor)
		 (send dc draw-rectangle (+ dx left) (+ dy top) (- right left) (- bottom top))
		 (send dc set-logical-function f)
		 (send dc set-pen p)
		 (send dc set-brush b))))]
	[select (lambda (on?)
		  (unless (eq? (not selected?) (not on?))
		    (top-select (if on? item #f) snip)))]
	[double-select (lambda () (send top double-select item))]
	[on-default-char void]
	[on-default-event void])
      (sequence
	(super-init)
	(hide-caret #t)
	(set-max-undo-history 0)
	(set-keymap item-keymap))))

  ; Buffer for a compound list item (and the top-level list)
  (define (make-hierarchical-list-buffer% super%)
    (class super% (top top-select)
      (inherit set-max-undo-history hide-caret
	       last-position insert delete line-start-position line-end-position
	       begin-edit-sequence end-edit-sequence)
      (private
	[children null]
	[make-whitespace (lambda () (make-object whitespace-snip%))]
	[insert-item 
	 (lambda (snip% whitespace?)
	   (let ([s (make-object snip% top top-select)])
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
	 (lambda () (for-each (lambda (l) (send l select #f)) children))]
	[new-item 
	 (lambda () (insert-item mred:hierarchical-item-snip% #t))]
	[new-list
	 (lambda () (insert-item mred:hierarchical-list-snip% #f))]
	[get-items (lambda () (map (lambda (x) (send x get-item)) children))]
	[delete-item
	 (lambda (i)
	   (let loop ([pos 0][l children][others null])
	     (cond
	      [(null? l) (error 'hierarchical-list-compound-item::delete-item "item not found: ~a" i)]
	      [(eq? (send (car l) get-item) i)
	       (set! children (append (reverse others) (cdr l)))
	       (let ([s (line-start-position pos)]
		     [e (line-end-position pos)])
		 (delete (if (zero? s) s (sub1 s)) (if (zero? s) (add1 e) e)))]
	      [else (loop (add1 pos) (cdr l) (cons (car l) others))])))]
	[on-default-char void]
	[on-default-event void])
      (sequence
	(super-init)
	(hide-caret #t)
	(set-max-undo-history 0))))

  (define mred:top-hierarchical-list-buffer% (make-hierarchical-list-buffer% mred:media-edit%))
  (define mred:hierarchical-list-buffer% (make-hierarchical-list-buffer% wx:media-edit%))

  ; Snip for a single list item
  (define mred:hierarchical-item-snip%
    (class wx:media-snip% (top top-select)
      (public
	[get-item-buffer% (lambda () mred:hierarchical-item-buffer%)]
	[select (lambda (on?) (send item-buffer select on?))]
	[show-select (lambda (on?) (send item-buffer show-select on?))]
	[get-item-buffer (lambda () item-buffer)]
	[get-item (lambda () item)])
      (private
	[item (make-object hierarchical-list-item% this)]
	[item-buffer (make-object (get-item-buffer%) top top-select item this)])
      (sequence
	(super-init item-buffer #f 0 0 0 0 0 0 0 0))))

  ; Snip for a compound list item
  (define mred:hierarchical-list-snip%
    (class wx:media-snip% (top top-select [title #f][content #f])
      (public
	[get-main-buffer% (lambda () (class-asi wx:media-edit% 
						(public
						 [on-default-char void]
						 [on-default-event void])))]
	[get-title-buffer% (lambda () mred:hierarchical-item-buffer%)]
	[get-content-buffer% (lambda () mred:hierarchical-list-buffer%)]
	[get-arrow-snip% (lambda () arrow-snip%)]
	[select (lambda (on?)
		  (if on?
		      (send title-buffer select #t)
		      (begin
			(send title-buffer select #f)
			(send content-buffer deselect-all))))]
	[show-select (lambda (on?) (send title-buffer show-select on?))]
	[on-arrow (lambda (a)
		    (if (send a on)
			(begin
			  (send main-buffer begin-edit-sequence)
			  (send content-buffer deselect-all)
			  (send top item-opened (get-item))
			  (send* main-buffer
			    (insert #\newline 2)
			    (insert whitespace 3)
			    (insert content-snip 4))
			  (send main-buffer end-edit-sequence))
			(begin
			  (send main-buffer begin-edit-sequence)
			  (send main-buffer delete 2 5)
			  (send top item-closed (get-item))
			  (send main-buffer end-edit-sequence))))]
	[get-title-buffer (lambda () title-buffer)]
	[get-content-buffer (lambda () content-buffer)]
	[get-item (lambda () item)])
      (private
	[item (make-object hierarchical-list-compound-item% this)]
	[main-buffer (make-object (get-main-buffer%))]
	[title-buffer (make-object (get-title-buffer%) top top-select item this)]
	[content-buffer (make-object (get-content-buffer%) top top-select)]
	[title-snip (make-object wx:media-snip% title-buffer #f 0 0 0 0 0 0 0 0)]
	[content-snip (make-object wx:media-snip% content-buffer #f 4 0 0 0 0 0 0 0)]
	[arrow (make-object arrow-snip% on-arrow)]
	[whitespace (make-object whitespace-snip%)])
      (sequence
	(super-init main-buffer #f 0 0 0 0 0 0 0 0)
	(send main-buffer set-max-undo-history 0)
	(send main-buffer hide-caret #t)
	(send main-buffer insert arrow)
	(when title (send title-buffer insert title))
	(when content (send content-buffer insert content))
	(send main-buffer insert title-snip))))

  (define hierarchical-list%
    (class mred:media-canvas% (parent)
      (inherit user-min-width user-min-height)
      (public
	[get-selected (lambda () selected-item)]
	[item-opened void]
	[item-closed void]
	[double-select void]
	[select void]
	[new-item (lambda () (send top-buffer new-item))]
	[new-list (lambda () (send top-buffer new-list))]
	[delete-item (lambda (i) (send top-buffer delete-item i))]
	[get-items (lambda () (send top-buffer get-items))])
      (private
	[do-select (lambda (item s)
		     (unless (eq? s selected)
		       (when selected (send selected show-select #f))
		       (set! selected (if item s #f))
		       (set! selected-item item)
		       (when selected (send selected show-select #t))
		       (select item)))]
	[top-buffer (make-object mred:top-hierarchical-list-buffer% this do-select)]
	[selected #f]
	[selected-item #f])
      (sequence
	(super-init parent -1 -1 -1 -1 "media-canvas"
		    wx:const-mcanvas-no-h-scroll
		    4
		    top-buffer)
	(send top-buffer set-cursor arrow-cursor) 
	(user-min-width 150)
	(user-min-height 200)))))
