
(unit/sig cards^
  (import (wx : wx^)
	  (mred : mred^)
	  mzlib:function^)
  (rename (create-region make-region))

  (define shuffle-list 
   (lambda (l c)
     (if (zero? c)
	 l
	 (let-values ([(a b)
		       (let ([half (floor (/ (length l) 2))])
			 (values
			  (let loop ([l l][n half])
			    (if (zero? n)
				null
				(cons (car l) (loop (cdr l) (sub1 n)))))
			  (list-tail l half)))])
	   (shuffle-list
	    (let loop ([a a][b b][l null])
	      (cond
	       [(null? a) (append (reverse b) l)]
	       [(null? b) (append (reverse a) l)]
	       [(zero? (random 2))
		(loop (cdr a) b (cons (car a) l))]
	       [else
		(loop a (cdr b) (cons (car b) l))]))
	    (sub1 c))))))

  (define ANIMATION-STEPS 5)
  (define ANIMATION-TIME 0.3)

  (define-struct region (x y w h label callback hilite? decided-start? can-select?))

  (define create-region
    (lambda (x y w h label callback)
      (make-region x y w h label callback #f #f #f)))

  (define cards:pasteboard%
    (class mred:pasteboard% ()
	   (inherit begin-edit-sequence end-edit-sequence get-admin
		    invalidate-bitmap-cache
		    find-next-selected-snip find-first-snip find-snip
		    set-before set-after
		    add-selected is-selected? no-selected
		    get-snip-location move-to
		    dc-location-to-buffer-location
		    set-selection-visible)
	   (rename [super-after-select after-select]
		   [super-on-default-event on-default-event]
		   [super-on-interactive-move on-interactive-move] 
		   [super-interactive-adjust-move interactive-adjust-move]
		   [super-after-interactive-move after-interactive-move])
	   (private 
	    [select-one? #t]
	    [select-backward? #f]
	    [raise-to-front? #f]
	    [button-map '((1 #f #f #t)
			  (2 #t #f #t)
			  (3 #f #t #f))]

	    [do-on-double-click 'flip]

	    [selecting? #f]
	    [dragging? #f]
	    [useless-drag? #f]
	    [click-base null]
	    [regions null]
	    [get-snip-bounds
	     (lambda (s)
	       (let ([xbox (box 0)]
		     [ybox (box 0)])
		 (get-snip-location s xbox ybox #f)
		 (let ([l (unbox xbox)]
		       [t (unbox ybox)])
		   (get-snip-location s xbox ybox #t)
		   (values l t (unbox xbox) (unbox ybox)))))]
	    [for-each-selected
	     (lambda (f)
	       (let loop ([snip (find-next-selected-snip null)])
		 (unless (null? snip)
		   (f snip)
		   (loop (find-next-selected-snip snip)))))]
	    [make-overlapping-list
	     (lambda (s so-far behind?)
	       (let-values ([(sl st sr sb) (get-snip-bounds s)])
		 (let loop ([t (find-first-snip)][so-far so-far][get? (not behind?)])
		   (cond
		    [(null? t) so-far]
		    [(eq? s t) (if behind?
				   (loop (send t next) so-far #t)
				   so-far)]
		    [get?
		     (let ([l (if (and (not (memq t so-far))
				       (let-values ([(tl tt tr tb) 
						     (get-snip-bounds t)])
					 (and (or (<= sl tl sr)
						  (<= sl tr sr))
					      (or (<= st tt sb)
						  (<= st tb sb)))))
				  (make-overlapping-list t (cons t so-far) behind?)
				  so-far)])
		       (loop (send t next) l #t))]
		    [else
		     (loop (send t next) so-far #f)]))))]
	    [get-reverse-selected-list
	     (lambda ()
	       (let loop ([s (find-next-selected-snip null)][l null])
		 (if (null? s)
		     l
		     (loop (find-next-selected-snip s) (cons s l)))))]
	    [shuffle
	     (lambda (selected-list) ; cards to shuffle in back->front order
	       (let* ([permuted-list
		       (shuffle-list selected-list 7)]
		      [get-pos
		       (lambda (s)
			 (let ([xb (box 0)]
			       [yb (box 0)])
			   (get-snip-location s xb yb)
			   (cons (unbox xb) (unbox yb))))]
		      [sel-loc-list (map get-pos selected-list)]
		      [perm-loc-list (map get-pos permuted-list)])
		 (for-each
		  (lambda (s start-pos end-pos)
		    (let* ([sx (car start-pos)]
			   [sy (cdr start-pos)]
			   [ex (car end-pos)]
			   [ey (cdr end-pos)]
			   [steps (max 1 (floor (/ 50 (length selected-list))))])
		      (let loop ([i 1])
			(unless (> i steps)
			  (let ([x (+ sx (* (/ i steps) (- ex sx)))]
				[y (+ sy (* (/ i steps) (- ey sy)))])
			    (move-to s x y)
			    (wx:flush-display)
			    (loop (add1 i)))))))
		  permuted-list perm-loc-list sel-loc-list)
		 (let loop ([l permuted-list])
		   (unless (null? l)
		     (set-before (car l) null)
		     (loop (cdr l))))
		 (no-selected)))])
	   (public
	    [on-paint
	     (lambda (before? dc l t r b dx dy caret)
	       (when before?
		 (for-each
		  (lambda (region)
		    (when (region-label region)
		      (let-values ([(sx sy sw sh) (get-region-box region)]
				   [(old-b) (send dc get-brush)])
			(when (region-hilite? region)
			  (send dc set-brush (send wx:the-brush-list
						   find-or-create-brush
						   "RED" wx:const-solid)))
			(send dc draw-rectangle (+ dx sx) (+ dy sy) sw sh)
			(when (region-hilite? region)
			  (send dc set-brush old-b))
			(let ([xb (box 0)]
			      [yb (box 0)]
			      [text (region-label region)])
			  (send dc get-text-extent text xb yb)
			  (send dc draw-text text
				(+ dx sx (/ (- sw (unbox xb)) 2))
				(+ dy sy 5))))))
		  regions)))]
	    [after-select
	     (lambda (s on?)
	       (super-after-select s on?)
	       (unless (or (not on?) selecting?)
		 (set! selecting? #t)
		 (if select-one?
		     (when raise-to-front?
		       (set-before s null))
		     (begin
		       (begin-edit-sequence)
		       (let ([l (make-overlapping-list s (list s) select-backward?)])
			 (for-each add-selected l))
		       (when raise-to-front?
			 (let loop ([snip (find-next-selected-snip null)][prev null])
			   (unless (null? snip)
			     (if (null? prev)
				 (set-before snip null)
				 (set-after snip prev))
			     (loop (find-next-selected-snip snip) snip))))
		       (end-edit-sequence)))
		 (set! selecting? #f)))]
	    [on-interactive-move
	     (lambda ()
	       (super-on-interactive-move)
	       (for-each (lambda (region) (set-region-decided-start?! region #f)) regions)
	       (for-each-selected (lambda (snip) (send snip remember-location this)))
	       (set! dragging? #t))]
	    [interactive-adjust-move
	     (lambda (snip xb yb)
	       (super-interactive-adjust-move snip xb yb)
	       (let-values ([(l t r b) (get-snip-bounds snip)])
		 (let ([wb (box 0)][hb (box 0)])
		   (send (get-admin) get-view null null wb hb)
		   (let ([max-x (- (unbox wb) (- r l))]
			 [max-y (- (unbox hb) (- b t))])
		     (when (> (unbox xb) max-x)
		       (set-box! xb max-x))
		     (when (> (unbox yb) max-y)
		       (set-box! yb max-y))))))]
	    [after-interactive-move
	     (lambda ()
	       (set! dragging? #f)
	       (super-after-interactive-move)
	       (for-each-selected (lambda (snip) (send snip back-to-original-location this)))
	       (let ([cards (get-reverse-selected-list)])
		 ; (no-selected)
		 (for-each
		  (lambda (region)
		    (when (region-hilite? region)
		      (semaphore-callback
		       ; Call it outside the current edit sequence
		       (make-semaphore 1)
		       (lambda ()
			 ((region-callback region) cards)
			 (unhilite-region region)))))
		  regions)))]
	    [update-region 
	     (lambda (region)
	       (let-values ([(sx sy sw sh) (get-region-box region)])
		 (invalidate-bitmap-cache sx sy sw sh)))]
	    [on-default-event
	     (lambda (e)
	       ; Left click: move one
	       ; Middle click: carry above
	       ; Right click: carry below
	       (let ([click (or (and (send e button-down? 1) 1)
				(and (send e button-down? 2) 2)
				(and (send e button-down? 3) 3))])
		 (when click
		   (let* ([actions (cdr (assoc click button-map))]
			  [one? (list-ref actions 0)]
			  [backward? (list-ref actions 1)]
			  [raise? (list-ref actions 2)])
		     (unless (and (eq? backward? select-backward?)
				  (eq? one? select-one?)
				  (eq? raise? raise-to-front?))
		     (set! select-one? one?)
		     (set! select-backward? backward?)
		     (set! raise-to-front? raise?)
		     (no-selected)))))
	       (let*-values ([(lx ly) (dc-location-to-buffer-location 
				       (send e get-x) 
				       (send e get-y))]
			     [(s) (find-snip lx ly)])
		 ; Clicking on a "selected" card unselects others
		 ; in this interface
		 (when (send e button-down?)
		   (unless (or (null? click-base) (null? s) (eq? s click-base))
		     (no-selected))
		   (set! click-base s))
		 (when (and dragging? (not (null? click-base)) (send click-base user-can-move))
		   (for-each
		    (lambda (region)
		      (when (and (region-callback region)
				 (or (not (region-decided-start? region))
				     (region-can-select? region)))
			(let-values ([(sx sy sw sh) (get-region-box region)])
			  (let ([in? (and (<= sx lx (+ sx sw))
					  (<= sy ly (+ sy sh)))])
			    (unless (region-decided-start? region)
			      (set-region-decided-start?! region #t)
			      (set-region-can-select?! region (not in?)))
			    (when (and (not (eq? in? (region-hilite? region)))
				       (region-can-select? region))
			      (set-region-hilite?! region in?)
			      (invalidate-bitmap-cache sx sy sw sh))))))
		    regions))
		 (if (send e button-down?)
		     (if (null? s)
			 (set! useless-drag? #t)
			 (set! useless-drag? #f))
		     (when (and useless-drag?
				(not (send e dragging?)))
			 (set! useless-drag? #f)))
		 (unless useless-drag?
		   (super-on-default-event e))
		 (when (and (send e button-down?)
			    (not (null? click-base))
			    (not (send click-base user-can-move)))
		   (no-selected))))]
	    [on-double-click
	     (lambda (s e)
	       (cond
		[(eq? do-on-double-click 'flip)
		 (begin-edit-sequence)
		 (let ([l (get-reverse-selected-list)])
		   (for-each 
		    (lambda (s) 
		      (when (send s user-can-flip)
			(send s flip)))
		    l)
		   (let loop ([l (reverse! l)])
		     (unless (null? l)
		       (set-before (car l) null)
		       (loop (cdr l)))))
		 (no-selected)
		 (end-edit-sequence)]
		[do-on-double-click
		 (do-on-double-click s)]
		[else (void)]))])
	   (public
	     [get-full-box
	      (lambda ()
		(let ([xb (box 0)][yb (box 0)]
				  [wb (box 0)][hb (box 0)])
		  (send (get-admin) get-view xb yb wb hb)
		  (values 0 0 (unbox wb) (unbox hb))))]
	     [get-region-box
	      (lambda (region)
		(values (region-x region)
			(region-y region)
			(region-w region)
			(region-h region)))]
	    [add-region
	     (lambda (r)
	       (set! regions (append regions (list r)))
	       (update-region r))]
	    [remove-region
	     (lambda (r)
	       (set! regions (remq r regions))
	       (update-region r))]
	    [unhilite-region
	     (lambda (region)
	       (set-region-hilite?! region #f)
	       (update-region region))]
	    [set-double-click-action
	     (lambda (a)
	       (set! do-on-double-click a))]
	    [set-button-action
	     (lambda (button action)
	       (let ([map
		      (case action
			[(drag/one) (list #t #f #f)]
			[(drag-raise/one) (list #t #f #t)]
			[(drag/above) (list #f #f #f)]
			[(drag-raise/above) (list #f #f #t)]
			[(drag/below) (list #f #t #f)]
			[(drag-raise/below) (list #f #t #t)]
			[else (error 'set-button-action "unknown action: ~s" action)])])
		 (set! button-map
		       (cons
			(cons button map)
			(remq (assoc button button-map)
			      button-map)))))])
	   (sequence
	     (super-init)
	     (set-selection-visible #f))))

  (define (get-bitmap file)
    (make-object wx:bitmap% file wx:const-bitmap-type-gif))
  (define (get-bitmap/dc file)
    (let ([bm (get-bitmap file)])
      (let ([m (make-object wx:memory-dc%)])
	(send m select-object bm)
	m)))

  (define sc (make-object wx:snip-class%))
  (send sc set-classname "card")
  (send (wx:get-the-snip-class-list) add sc)

  (define card%
    (class wx:snip% (suite-id value width height front back)
	   (inherit set-snipclass set-count get-admin)
	   (private
	    [flipped? #f]
	    [can-flip? #t]
	    [can-move? #t]
	    [snap-back? #f])
	   (public
	    [face-down? (lambda () flipped?)]
	    [flip
	     (lambda ()
	       (set! flipped? (not flipped?))
	       (let ([a (get-admin)])
		 (unless (null? a)
		   (send a needs-update this 0 0 width height))))]
	    [face-up (lambda () (when flipped? (flip)))]
	    [face-down (lambda () (unless flipped? (flip)))]
	    [resize
	     (lambda (w h) (void))]
	    [get-suite-id
	     (lambda () suite-id)]
	    [get-suite
	     (lambda ()
	       (case suite-id
		 [(1) 'clubs]
		 [(2) 'diamonds]
		 [(3) 'hearts]
		 [(4) 'spades]))]
	    [get-value
	     (lambda () value)]
	    [user-can-flip
	     (case-lambda
	      [() can-flip?]
	      [(f) (set! can-flip? (and f #t))])]
	    [user-can-move
	     (case-lambda
	      [() can-move?]
	      [(f) (set! can-move? (and f #t))])]
	    [snap-back-after-move
	     (case-lambda
	      [() snap-back?]
	      [(f) (set! snap-back? (and f #t))])])
	   (public
	    [card-width (lambda () width)]
	    [card-height (lambda () height)]
	    [get-extent
	     (lambda (dc x y w h descent space lspace rspace)
	       (map
		(lambda (b)
		  (unless (null? b)
		    (set-box! b 0)))
		(list descent space lspace rspace))
	       (unless (null? w) (set-box! w width))
	       (unless (null? h) (set-box! h height)))]
	    [draw
	     (lambda (dc x y left top right bottom dx dy draw-caret)
	       (send dc blit x y width height 
		     (if flipped? back front) 
		     0 0))]
	    [copy (lambda () (make-object card% suite-id value width height front back))])
	   (private
	     [save-x (box 0)]
	     [save-y (box 0)])
	   (public
	     [remember-location
	      (lambda (pb)
		(send pb get-snip-location this save-x save-y))]
	     [back-to-original-location
	      (lambda (pb)
		(when snap-back?
		  (send pb move-to this (unbox save-x) (unbox save-y))))])
	   (sequence
	     (super-init)
	     (set-count 1)
	     (set-snipclass sc)
	     (flip))))

  (define tmpframe
    (let* ([f (make-object mred:dialog-box% null "Please Wait")]
	   [p (make-object mred:vertical-panel% f)])
      (make-object mred:message% p "Loading cards...")
      (send p stretchable-in-x #f)
      (send p stretchable-in-y #f)
      (send f center wx:const-both)
      (send f show #t)
      f))
  (wx:flush-display)
  (wx:yield)

  (define (here file)
    (build-path 
     (or (current-load-relative-directory) 
	 "/home/mflatt/proj/plt/collects/games/cards")
     file))

  (define back (get-bitmap (here "back.gif")))

  (define deck-of-cards
    (let* ([back (get-bitmap (here "back.gif"))]
	   [w (send back get-width)]
	   [h (send back get-height)]
	   [back (let ([m (make-object wx:memory-dc%)])
		   (send m select-object back)
		   m)])
      (let sloop ([suite 4])
	(if (zero? suite)
	    null
	    (let vloop ([value 13])
	      (sleep)
	      (if (zero? value)
		  (sloop (sub1 suite))
		  (cons (make-object card%
				     suite
				     value
				     w h
				     (get-bitmap/dc
				      (here 
				       (format "card-~a-~a.gif"
					       (sub1 value)
					       (sub1 suite))))
				     back)
			(vloop (sub1 value)))))))))
  
  (send tmpframe show #f)

  (define table%
    (class mred:frame% (title w h)
      (public
	[table-width (lambda () 
		       (let-values ([(x y w h) (send pb get-full-box)])
			 w))]
	[table-height (lambda () 
			(let-values ([(x y w h) (send pb get-full-box)])
			  h))]
	[begin-card-sequence
	 (lambda ()
	   (send pb begin-edit-sequence))]
	[end-card-sequence
	 (lambda ()
	   (send pb end-edit-sequence))]
	[add-card
	 (lambda (card x y)
	   (position-cards (list card) x y (lambda (p) (values 0 0)) add-cards-callback))]
	[add-cards
	 (opt-lambda (cards x y [offset (lambda (p) (values 0 0))])
	   (position-cards cards x y offset add-cards-callback))]
	[add-cards-to-region
	 (lambda (cards region)
	   (position-cards-in-region cards region add-cards-callback))]
	[move-card
	 (lambda (card x y)
	   (position-cards (list card) x y (lambda (p) (values 0 0)) move-cards-callback))]
	[move-cards
	 (opt-lambda (cards x y  [offset (lambda (p) (values 0 0))])
	   (position-cards cards x y offset move-cards-callback))]
	[move-cards-to-region
	 (lambda (cards region)
	   (position-cards-in-region cards region (ivar pb move-to)))]
	[card-to-front
	 (lambda (card)
	   (send pb set-before card null))]
	[card-to-back
	 (lambda (card)
	   (send pb set-after card null))]
	[stack-cards
	 (lambda (cards)
	   (unless (null? cards)
	     (begin-card-sequence)
	     (let loop ([l (cdr cards)][behind (car cards)])
	       (unless (null? l)
		 (send pb set-after (car l) behind)
		 (loop (cdr l) (car l))))
	     (end-card-sequence)))]
	[add-region
	 (lambda (r)
	   (send pb add-region r))]
	[remove-region
	 (lambda (r)
	   (send pb remove-region r))]
	[set-button-action
	 (lambda (button action)
	   (send pb set-button-action button action))]
	[set-double-click-action
	 (lambda (a)
	   (send pb set-double-click-action a))]
	[pause
	 (lambda (duration)
	   (let ([s (make-semaphore)])
	     (thread (lambda () (sleep duration) (semaphore-post s)))
	     ; Can't move the cards during this time:
	     (send c enable #f)
	     (wx:yield s)
	     (send c enable #t)))])
      (private
	[add-cards-callback
	 (lambda (card x y)
	   (send pb insert card null x y))]
        [move-cards-callback
	 (lambda (card x y)
	   (send pb move-to card x y))]
	[animate? #t]
        [position-cards
	 (lambda (cards x y offset set)
	   (let ([positions (let loop ([l cards][n 0])
			      (if (null? l)
				  null
				  (let-values ([(dx dy) (offset n)])
				    (cons (cons (+ x dx) (+ y dy))
					  (loop (cdr l) (add1 n))))))])
	     (if (or (not animate?) (eq? set add-cards-callback))
		 (begin
		   (begin-card-sequence)
		   (for-each (lambda (c p) (set c (car p) (cdr p))) cards positions)
		   (end-card-sequence))
		 (let-values ([(moving-cards
				source-xs
				source-ys
				dest-xs
				dest-ys)
			       (let loop ([cl cards][pl positions])
				 (if (null? cl)
				     (values null null null null null)
				     (let-values ([(mcl sxl syl dxl dyl) (loop (cdr cl) (cdr pl))]
						  [(card) (car cl)]
						  [(x y) (values (caar pl) (cdar pl))])
				       (let ([xb (box 0)][yb (box 0)])
					 (send pb get-snip-location card xb yb)
					 (let ([sx (unbox xb)][sy (unbox yb)])
					   (if (and (= x sx) (= y sy))
					       (values mcl sxl syl dxl dyl)
					       (values (cons card mcl)
						       (cons sx sxl)
						       (cons sy syl)
						       (cons x dxl)
						       (cons y dyl))))))))])
		   (let loop ([n 1])
		     (unless (> n ANIMATION-STEPS)
		       (let ([start (current-milliseconds)]
			     [scale (lambda (s d)
				      (+ s (* n (/ (- d s) ANIMATION-STEPS))))])
			 (begin-card-sequence)
			 (for-each
			  (lambda (c sx sy dx dy)
			    (set c (scale sx dx) (scale sy dy)))
			  moving-cards
			  source-xs source-ys
			  dest-xs dest-ys)
			 (end-card-sequence)
			 (pause (max 0 (- (/ ANIMATION-TIME ANIMATION-STEPS)
					  (/ (- (current-milliseconds) start) 1000))))
			 (loop (add1 n)))))))))]
	[position-cards-in-region
	 (lambda (cards r set)
	   (let-values ([(x y w h) (send pb get-region-box r)]
			[(len) (sub1 (length cards))]
			[(cw ch) (values (send back get-width)
					 (send back get-height))])
	     (position-cards cards x y
			     (lambda (p)
			       (if (zero? len)
				   (values (/ (- w cw) 2)
					   (/ (- h ch) 2))
				   (values (* (- len p) (/ (- w cw) len))
					   (* (- len p) (/ (- h ch) len)))))
			     set)))])
      (sequence
	(super-init null title))
      (private
        [p (make-object mred:vertical-panel% this)]
	[c (make-object mred:media-canvas% p
			-1 -1 -1 -1
			""
			(bitwise-ior wx:const-mcanvas-no-v-scroll
				     wx:const-mcanvas-no-h-scroll))]
	[pb (make-object cards:pasteboard%)])
      (sequence
	(send c user-min-client-width (* w (send back get-width)))
	(send c user-min-client-height (* h (send back get-height)))
	(send c stretchable-in-x #f)
	(send c stretchable-in-y #f)
	(send p stretchable-in-x #f)
	(send p stretchable-in-y #f)
	(send c set-media pb))))

  (define make-table
    (opt-lambda ([title "Cards"][w 7][h 3])
      (make-object table% title w h)))

  (define (make-deck)
    (map (lambda (l) (send l copy)) deck-of-cards)))
