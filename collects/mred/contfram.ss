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
	    get-x
	    get-y
	    get-width
	    get-height
	    get-client-size)

	  (rename
	    [super-set-size set-size])
	  
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
		  (string-append
		   "container-frame-insert-panel: "
		   "Entering insert-panel, frame ~s")
		  object-ID)
		(mred:debug:printf 'container-frame-insert-panel
		  (string-append
		   "container-frame-insert-panel: Argument: ~s; "
		   "panel ID ~s")
		  new-panel (ivar new-panel object-ID))
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
		  (string-append
		   "container-frame-insert-panel: "
		   "sizing panel, forcing redraw, and quitting."))
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
		  (string-append
		   "container-frame-force-redraw: "
		   "Entering force-redraw; frame ~s")
		  object-ID)
		(mred:debug:printf 'container-frame-force-redraw
		  (string-append
		   "container-frame-force-redraw: "
		   "calling on-size with ~s ~s and quitting")
		  (get-width) (get-height))
		(on-size (get-width) (get-height)))]
	    
;	    [set-size
;	      (lambda (x y width height)
;		(if (and (or (= x (get-x))
;			     (= x const-default-posn))
;		         (or (= y (get-y))
;			     (= y const-default-posn))
;			 (or (= width (get-width))
;			     (= width const-default-size))
;			 (or (= height (get-height))
;			     (= height const-default-size)))
;		    (unless (null? (get-panel))
;		      (send panel set-size
;			(send panel get-x) (send panel get-y)
;			(send panel get-width) (send panel get-height)))
;		    (super-set-size x y width height)))]
	    
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
		  (string-append
		   "container-frame-on-size: "
		   "Entered frame's on-size; args ~s ~s")
		  new-width new-height)
		(unless (null? panel)
		  (let ([x (box 0)]
			[y (box 0)])
		    (get-client-size x y)
		    (mred:debug:printf 'container-frame-on-size
		      (string-append
			"container-frame-on-size: "
			"setting panel to 0 0 ~s ~s")
		      (unbox x) (unbox y))
		    (send panel set-size 0 0 (unbox x) (unbox y))
		    (send panel get-client-size x y)
		    (let* ([panel-info (send panel get-info)]

			   ; panel's client size:
			   [panel-client-w (unbox x)]
			   [panel-client-h (unbox y)]

			   ; frame's current size:
			   [frame-w (get-width)]
			   [frame-h (get-height)]

			   ; difference between frame's full size & panel's
			   ; client size:
			   [delta-w (- frame-w panel-client-w)]
			   [delta-h (- frame-h panel-client-h)]

			   ; minimum frame size:
			   [min-frame-w (+ delta-w
					   (child-info-x-min panel-info))]
			   [min-frame-h (+ delta-h
					   (child-info-y-min panel-info))]

			   ; new size for frame
			   [new-w
			     (cond
			       [(< frame-w min-frame-w) min-frame-w]
			       [(and (> frame-w min-frame-w)
				     (not (child-info-x-stretch
					    panel-info)))
				min-frame-w]
			       [else frame-w])]
			   [new-h
			     (cond
			       [(< frame-h min-frame-h) min-frame-h]
			       [(and (> frame-h min-frame-h)
	 			     (not (child-info-y-stretch
					    panel-info)))
				min-frame-h]
			       [else frame-h])])
			   (mred:debug:printf 'container-frame-on-size
			     "container-frame-on-size: panel client ~s x ~s"
			     panel-client-w panel-client-h)
			   (mred:debug:printf 'container-frame-on-size
			     (string-append
			       "container-frame-on-size: "
			       "size differences: ~s, ~s")
			     delta-w delta-h)
			   (mred:debug:printf 'container-frame-on-size
			     "container-frame-on-size: New size: ~s x ~s"
			     new-w new-h)
			   (unless (and (= frame-w new-w)
				        (= frame-h new-h))
			     (mred:debug:printf 'container-frame-on-size
			       "container-frame-on-size: Resizing to ~s x ~s"
			       new-w new-h)
			     (set-size const-default-posn const-default-posn
			       new-w new-h)))))
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
