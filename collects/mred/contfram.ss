; to be exported
; frame%
; dialog-box%

; to be imported:
; child-info

(define mred:container-frames@
  (unit/sig mred:container-frames^
    (import [mred:debug : mred:debug^]
	    mred:container-children^
	    mred:container-panels^)
    
    (mred:debug:printf 'invoke "mred:container-frames@")
    
    (define counter 0)  ; used in debugging
    
    ; default size & placement for dialog boxes.  Note that these values
    ; are not used by these classes; they are simply used in the function
    ; that processes dialog-box%'s args before sending them on to
    ; super-init, so we don't change the defaults from wx:dialog-box%.
    (define default-dialog-posn 300)
    (define default-dialog-size 500)

    ; make-top-container%: adds the necessary functionality to wx:frame% and 
    ; wx:dialog-box%.
    ; input: base%: the base class from which to descend the new mred class.
    ;          Intended to be either wx:frame% or wx:dialog-box%, but can
    ;          be anything which contains all methods in the inherit section
    ;          below.
    ; returns: a new class, descended from base%, which possesses the added
    ;            capabilities necessary to serve as the frame/dialog which
    ;            contains mred container classes.
    (define make-top-container%
      (lambda (base%)
	(class base% args
	  (inherit
	    set-size
	    get-width
	    get-height
	    get-client-size)

	  (rename
	    [super-on-size on-size])
	  
	  (private
	    
	    ; pointer to panel in the frame for use in on-size
	    [panel null])

	  (public
	    
	    ; a unique ID for the object for debugging purposes.
	    object-ID
	      
	    ; insert-panel: update panel pointer.
	    ; input: new-panel: panel in frame (descendant of
	    ;   mred:panel%) 
	    ; returns: nothing
	    ; effects: sets panel to new-panel
	    ;          if new-panel is not a descendant of
	    ;            mred:panel%, calls error; panel not updated.
	    [insert-panel
	      (lambda (new-panel)
		(mred:debug:printf 'container-frame-insert-panel
		  "container-frame-insert-panel: Entering insert-panel, frame ~s" object-ID)
		(mred:debug:printf 'container-frame-insert-panel
		  "container-frame-insert-panel: Argument: ~s; panel ID ~s" new-panel
		  (ivar new-panel object-ID))
		(unless (null? panel)
		  (error 'insert-panel
			 "Adding a second panel to a frame or dialog"))
		(unless (is-a? new-panel panel%)
		  (error 'insert-panel
		    "Expected a mred:panel% descendant; got ~s"
		    new-panel))
		(unless (eq? this (send new-panel get-parent))
		  (error 'insert-panel
		    "Added panel ~s to a frame (~s) not its parent"
		    new-panel this))
		(set! panel new-panel)
		(mred:debug:printf 'container-frame-insert-panel
		  "container-frame-insert-panel: sizing panel, forcing redraw, and quitting.")
		(let-values ([(client-w client-h)
			      (get-two-int-values get-client-size)])
		  (send panel set-size 0 0 client-w client-h))
		(force-redraw))]
	    
	    [get-panel
	      (lambda ()
		panel)]

	    [force-redraw
	      (lambda ()
		(mred:debug:printf 'container-frame-force-redraw
		  "container-frame-force-redraw: Entering force-redraw; frame ~s" object-ID)
		(mred:debug:printf 'container-frame-force-redraw
		  "container-frame-force-redraw: calling on-size with ~s ~s and quitting"
		  (get-width) (get-height))
		(on-size (get-width) (get-height)))]
	    
	    ; on-size: ensures that size of frame matches size of content
	    ; input: new-width/new-height: new size of frame
	    ; returns: nothing
	    ; effects: if new size is smaller than allowed size of
	    ;            contents, frame resized to smallest possible size.
	    ;            If frame is larger than contents and contents
	    ;            aren't stretchable, frame resized to size of
	    ;            contents.  Each direction is handled
	    ;            independantly.
	    [on-size
	      (lambda (new-width new-height)
		(mred:debug:printf 'container-frame-on-size
		  "container-frame-on-size: Entered frame's on-size; args ~s ~s"
		  new-width new-height)
		(mred:debug:printf 'container-frame-on-size
		  "container-frame-on-size: Calling super-class's on-size")
		(unless (null? panel)
		  (let ([p-x (box 0)]
			[p-y (box 0)])
		    (send panel get-client-size p-x p-y)
		    (let* ([panel-info (send panel get-info)]
			   
			   ; minimum size assumable by client area of
			   ; panel
			   [min-width (child-info-x-min panel-info)]
			   [min-height (child-info-y-min panel-info)]
			   
			   ; panel's client size
			   [p-client-width (unbox p-x)]
			   [p-client-height (unbox p-y)]
			   
			   ; difference between panel's client & full
			   ; sizes. 
			   [p-delta-w (- (send panel get-width)
					 p-client-width)]
			   [p-delta-h (- (send panel get-height)
					 p-client-height)]
			   
			   ; difference between frame's full size &
			   ; panel's client size.
			   [delta-w (- new-width p-client-width)]
			   [delta-h (- new-height p-client-height)]
			   
			   ; new size for panel's client area
			   [new-w (cond
				    [(< p-client-width min-width)
				     min-width]
				    [(and (> p-client-width min-width)
				       (not (child-info-x-stretch
					      panel-info)))
				     min-width]
				    [else p-client-width])]
			   [new-h (cond
				    [(< p-client-height min-height)
				     min-height] 
				    [(and (> p-client-height min-height)
				       (not (child-info-y-stretch
					      panel-info)))
				     min-height]
				    [else p-client-height])]
			   
			   ; new frame size, computed from above
			   ; quantities.
			   [f-width (+ new-w delta-w)]
			   [f-height (+ new-h delta-h)])
		      (mred:debug:printf 'container-frame-on-size
			"container-frame-on-size: panel client ~s x ~s"
			p-client-width p-client-height)
		      (mred:debug:printf 'container-frame-on-size
			"container-frame-on-size: size differences: ~s, ~s"
			delta-w delta-h)
		      (mred:debug:printf 'container-frame-on-size
			"container-frame-on-size: New size: ~s x ~s"
			new-w new-h)
		      (begin
			(mred:debug:printf 'container-frame-on-size
			  "container-frame-on-size: Resizing panel to ~s x ~s"
			  (+ (- f-width delta-w) p-delta-w)
			  (+ (- f-height delta-h) p-delta-h))
			(send panel set-size const-default-posn
			  const-default-posn
			  (+ (- f-width delta-w) p-delta-w)
			  (+ (- f-height delta-h) p-delta-h))
			(mred:debug:printf 'container-frame-on-size
			  "container-frame-on-size: Resized panel"))
		      (unless (and (= new-width f-width)
				(= new-height f-height))
			(mred:debug:printf 'container-frame-on-size
			  "container-frame-on-size: Resizing to ~s x ~s"
			  f-width f-height)
			(set-size const-default-posn const-default-posn
			  f-width f-height)))))
		(mred:debug:printf 'container-frame-on-size
		  "container-frame-on-size: Leaving onsize at the end."))])
	  (sequence
	    (apply super-init args)
	    (set! object-ID counter)
	    (set! counter (add1 counter))))))
    
    (define frame%
      (class (make-top-container% wx:frame%) args
	(sequence
	  (apply (opt-lambda (parent title
			       [x const-default-posn]
			       [y const-default-posn]
			       [w const-default-size]
			       [h const-default-size]
			       [style (bitwise-ior
					wx:const-sdi
					wx:const-default-frame)]
			       [name "frame"])
		   (super-init parent title x y w h
		     (bitwise-ior style wx:const-allow-auto-resize)
		     name))
	    args))))
    
    (define dialog-box%
      (class (make-top-container% wx:dialog-box%) args
	(inherit
	  centre)
	(rename
	  [super-show show])
	(public

	  ; show: shows/hides the dialog and optionally centers it
	  ; on-screen
	  ; input: now?: a boolean; #t to show window, #f to hide it
	  ;        center?: an optional boolean; #t to center, #f not.
	  ; returns: nothing
	  ; effects: shows or hides window; if now? & center? are both #t,
	  ;   centers window on-screen as well.
	  [show
	    (opt-lambda (now? [center? #t])
	      (when (and now? center?) (centre wx:const-both))
	      (super-show now?))])
	(sequence
	  (apply (opt-lambda (parent title
			       [modal? #f]
			       [x default-dialog-posn]
			       [y default-dialog-posn]
			       [w default-dialog-size]
			       [h default-dialog-size]
			       [style wx:const-default-dialog-style]
			       [name "dialogBox"])
		   (super-init parent title modal? x y w h
		     (bitwise-ior style wx:const-allow-auto-resize)
		     name))
		 args))))))
