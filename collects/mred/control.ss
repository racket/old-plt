
(unit/sig mred:control^
  (import [wx : wx^]
	  [mred : mred:edit^]
	  [mred : mred:canvas^]
	  [mred : mred:container^])
  
  ; Helper for media-text%
  (define media-text-edit% 
    (class mred:return-edit% (cb return-cb control)
      (rename [super-after-insert after-insert]
	      [super-after-delete after-delete]
	      [super-on-focus on-focus])
      (inherit get-text last-position)
      (private
	[block-callback 1]
	[callback
	 (lambda (type str?)
	   (when (zero? block-callback)
	     (let ([str (if str? (get-text 0 (last-position)) #f)]
		   [e (make-object wx:command-event% type)])
	       (send e set-event-object control)
	       (when str
		 (send e set-command-string str))
	       (cb control e))))])
      (public
	[after-insert
	 (lambda args
	   (apply super-after-insert args)
	   (callback wx:const-event-type-text-command #t))]
	[after-delete
	 (lambda args
	   (apply super-after-delete args)
	   (callback wx:const-event-type-text-command #t))]
	[on-focus
	 (lambda (on?)
	   (super-on-focus on?)
	   (if on?
	       (callback wx:const-event-type-set-focus #f)
	       (callback wx:const-event-type-kill-focus #f)))]
	[callback-ready
	 (lambda () 
	   (set! block-callback 0))]
	[without-callback
	 (lambda (thunk)
	   (dynamic-wind
	    (lambda () (set! block-callback (add1 block-callback)))
	    thunk
	    (lambda () (set! block-callback (sub1 block-callback)))))])
      (sequence
	(super-init (lambda ()
		      (return-cb
		       (lambda ()
			 (callback wx:const-event-type-text-enter-command #t)
			 #t)))))))
  
  (define media-text-canvas% 
    (class mred:one-line-canvas% (control parent style)
      (rename [super-on-char on-char])
      (public
	[on-char (lambda (e) (send control on-char e))]
	[continue-on-char (lambda (e) (super-on-char e))]
	[style-flags style])
      (sequence
	(super-init parent))))
  
  (define (make-media-text% multi?)
    (class mred:horizontal-panel% (parent func label [value ""] 
					  [x -1] [y -1] [width -1] [height -1] 
					  [style 0] [name "edit"])
      (inherit minor-align-top stretchable-in-y)
      (rename [super-place-children place-children])
      (public
	[get-edit% (lambda () media-text-edit%)]
	[get-canvas% (lambda () media-text-canvas%)])
      (sequence
	(super-init parent))
      (private
	[horiz? (= (send parent get-label-position) wx:const-horizontal)]
	[p (if horiz?
	       this
	       (make-object mred:vertical-panel% this))]
	[l (if (null? label)
	       #f
	       (make-object mred:message% p label))]
	[c (make-object (get-canvas%) this p
			(if multi?
			    (if (positive? (bitwise-and style wx:const-hscroll))
				0
				wx:const-mcanvas-hide-h-scroll)
			    (+ wx:const-mcanvas-hide-v-scroll
			       wx:const-mcanvas-hide-h-scroll)))]
	[e (make-object (get-edit%) 
			func
			(lambda (do-cb)
			  (if multi?
			      #f
			      (if (zero? (bitwise-and style wx:const-process-enter))
				  (send parent on-default-action this)
				  (do-cb))))
			this)]
	[dy 0])
      (public
	[get-edit (lambda () e)]
	
	;; wx:text% and wx:multi-text%
	[copy (lambda () (send e copy))]
	[cut (lambda () (send e cut))]
	[get-value (lambda () (send e get-text))]
	[on-char (lambda (ev) (send c continue-on-char ev))]
	[paste (lambda () (send e paste))]
	[set-editable (lambda (on?) (send e lock (not on?)))]
	[set-value (lambda (v) (send e without-callback
				     (lambda () (send e insert v 0 (send e last-position)))))]
	
	[set-label (lambda (str) (send l set-label str))]
	[get-label (lambda () (send l get-label))]
	
	;; panel vs. item mismatch
	[on-default-action (lambda () (void))]
	
	[place-children
	 (lambda (children-info width height)
	   (let ([r (super-place-children children-info width height)])
	     (if horiz?
		 ;; Line up label right with text:
		 (if (null? r)
		     r
		     (cons (list* (caar r) (+ (cadar r) dy) (cddar r))
			   (cdr r)))
		 r)))])
      (sequence
	(minor-align-top)
	(stretchable-in-y #f)
	(send c set-media e)
	(if multi?
	    (send c set-lines 3)
	    (send e set-auto-set-wrap #f))
	(unless horiz?
	  (send p minor-align-left))
	(when (and l horiz?)
	  ;; Find amount to drop label down to line up the baselines:
	  (let ([wbox (box 0)]
		[hbox (box 0)]
		[ybox (box 0)]
		[abox (box 0)])
	    ; To bottom of first line
	    (send (send e get-admin) get-dc null ybox)
	    (set! dy (+ -3 (abs (unbox ybox)) (send e line-location 0 #f))) ; 3 is fudge factor
	    
	    ; Add diff for client size
	    (send c get-client-size wbox hbox)
	    (let ([d (- (send c get-height) (unbox hbox))])
	      (set! dy (+ dy (quotient d 2))))
	    
	    ; Subtract descent of canvas-drawn text
	    (let ([font (send (send (send e get-style-list) find-named-style "Standard") get-font)])
	      (send c get-text-extent "hi" wbox hbox ybox null font)
	      (set! dy (- dy (unbox ybox))))
	    
	    ; Subtract ascent of label
	    (send l get-text-extent "hi" wbox hbox ybox abox)
	    (set! dy (- dy (- (unbox hbox) (unbox ybox))))
	    
	    ; Subtract space above label
	    (set! dy (- dy (quotient (- (send l get-height) (unbox hbox)) 2)))))
	
	(when multi? (send c stretchable-in-y #t))
	(unless (null? value)
	  (set-value value)
	  (unless (string=? value "")
	    (let* ([ew (box 0)]
		   [cw (box 0)]
		   [tw (box 0)])
	      (send e get-extent ew null)
	      (send (send e get-admin) get-view null null cw null)
	      (send c get-size tw (box 0))
	      (let ([new-min-width (+ (unbox ew) (- (unbox tw) (unbox cw)))])
		(send c user-min-width new-min-width)))))
	(send e callback-ready))))

  (define media-text% (make-media-text% #f))
  (define media-multi-text% (make-media-text% #t)))
