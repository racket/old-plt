; to be exported
; frame%
; dialog-box%

; to be imported:
; child-info

    (unit/sig mred:container-frames^
      (import [mred:constants : mred:constants^]
	      [mred:connections : mred:connections^]
	      mred:container-children^
	      mred:container-panels^)
      
      (mred:debug:printf 'invoke "mred:container-frames@")
      
      (define counter 0)  ; used in debugging
      
      ; this is the size of the border that the window system automatically
      ; places between a panel and its frame.  I did it this way to make it
      ; easily modifiable if necessary.
      (define WX-BORDER-SIZE
	(case wx:window-system
	  [(xt)  2]  ; grrrr....
	  [(motif) 0]
	  [(windows) 0]
	  [(macintosh) 0]))
      
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
	     get-x
	     get-y
	     get-width
	     get-height
	     get-client-size
	     is-shown?)
	    
	    (rename
	     [super-show show]
	     [super-on-size on-size]
	     [super-set-size set-size])
	    
	  (private
	    ; have we had any redraw requests while the window has been
	    ; hidden?
	    [pending-redraws? #f]

	    [perform-updates? #t]

	    [ignore-redraw-request? #f]

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
				   "container-frame-insert-panel: "
				   "Argument: ~s; panel ID ~s")
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
				   "sizing panel, forcing redraw, "
				   "and quitting."))
	       (let-values ([(client-w client-h)
			     (get-two-int-values get-client-size)])
		 (send panel set-size WX-BORDER-SIZE WX-BORDER-SIZE
		       (- client-w (* 2 WX-BORDER-SIZE))
		       (- client-h (* 2 WX-BORDER-SIZE))))
	       (self-redraw-request))]
	    
	    [get-panel
	     (lambda ()
	       panel)]

	    [delay-updates
	     (case-lambda
	      [() (not perform-updates?)]
	      [(f) (set! perform-updates? (not f))
		   (when pending-redraws?
			 (force-redraw))])]

	    ; force-redraw: receives a message from to redraw the
	    ; entire frame.
	    ; input: none
	    ; returns: nothing
	    ; effects: redraws the frame at its current size (changing size
	    ;            as necessary).
	    [child-redraw-request
	     ; since there's only one panel, we assume that `from' is the
	     ; panel and the request should be granted
	     (lambda (from) 
	       (unless ignore-redraw-request?
		  (self-redraw-request)))]
	    [self-redraw-request
	     (lambda ()
	       (if (and (is-shown?) perform-updates?)
		   (force-redraw)
		   (set! pending-redraws? #t)))]
	    [force-redraw
	     (lambda ()
	       (mred:debug:printf 'container-frame-force-redraw
		       (string-append
			"container-frame-force-redraw: "
			"Entering force-redraw; frame ~s")
		       object-ID)
	       (unless (null? panel)
		 (dynamic-wind
		  (lambda () (set! ignore-redraw-request? #t))
		  (lambda () 
		    ; Ensures that the frame is big enough:
		    (set-size (get-x) (get-y) (get-width) (get-height))
		    (send panel on-container-resize))
		  (lambda () (set! ignore-redraw-request? #f))))
	       (set! pending-redraws? #f))]
	    
	    ; show: add capability to set perform-updates
	    ; input: now : boolean
	    ; returns: nothing
	    ; effects: if we're showing for the first time, unblock updates
	    ;            and force an update.  If we're hiding, block updates.
	    ;          pass now to superclass's show.
	    [show
	     (lambda (now)
	       (when (and now pending-redraws?)
		     (force-redraw))
	       (super-show now))]
	    
	    [set-size
	     (lambda (x y width height)
	       (mred:debug:printf
		'container-frame-set-size
		"Container-frame-set-size: entering; args ~s ~s ~s ~s"
		x y width height)
	       (let-values ([(correct-w correct-h)
			     (correct-size width height)])
		 (mred:debug:printf
		  'container-frame-set-size
		  "container-frame-set-size: correct size ~s ~s"
		  correct-w correct-h)
		 (if (and (same-dimension? x (get-x))
			  (same-dimension? y (get-y))
			  (and (same-dimension? width (get-width))
			       (= width correct-w))
			  (and (same-dimension? height (get-height))
			       (= height correct-h)))
		     (unless (null? (get-panel))
		       (let-values ([(f-client-w f-client-h)
				     (get-two-int-values get-client-size)])
			 (let ([panel-w (- f-client-w (* 2 WX-BORDER-SIZE))]
			       [panel-h (- f-client-h (* 2 WX-BORDER-SIZE))])
			   (mred:debug:printf
			    'container-frame-set-size
			    (string-append
			     "container-frame-set-size: "
			     "forcing panel to redraw to ~s ~s ~s ~s")
			    WX-BORDER-SIZE WX-BORDER-SIZE panel-w panel-h)
			   (send panel set-size
				 WX-BORDER-SIZE WX-BORDER-SIZE
				 panel-w panel-h))))
		     (begin
		       (mred:debug:printf
			'container-frame-set-size
			(string-append
			 "Container-frame-set-size: passing correct size to "
			 "super-set-size"))
		       (super-set-size x y correct-w correct-h)))))]
	    
	    [correct-size
	     (lambda (frame-w frame-h)
	       (if (null? panel)
		   (values frame-w frame-h)
		   (let-values ([(f-client-w f-client-h)
				 (get-two-int-values get-client-size)])
		     (let* ([panel-info (send panel get-info)]
			    
			    ; difference between panel's full size & 
			    ; frame's full size (tweaked for wm)
			    [delta-w (+ (- (get-width) f-client-w)
					(* 2 WX-BORDER-SIZE))]
			    [delta-h (+ (- (get-height) f-client-h)
					(* 2 WX-BORDER-SIZE))]
			    
			    ; minimum frame size:
			    [min-w (+ delta-w (child-info-x-min panel-info))]
			    [min-h (+ delta-h (child-info-y-min panel-info))]
			    
			    ; correct size for frame
			    [new-w
			     (cond
			       [(< frame-w min-w) min-w]
			       [(and (> frame-w min-w)
				     (not (child-info-x-stretch panel-info)))
				min-w]
			       [else frame-w])]
			    [new-h
			     (cond
			       [(< frame-h min-h) min-h]
			       [(and (> frame-h min-h)
				     (not (child-info-y-stretch panel-info)))
				min-h]
			       [else frame-h])])
		       (values new-w new-h)))))]
	    
	    ; on-size: ensures that size of frame matches size of content
	    ; input: new-width/new-height: new size of frame
	    ; returns: nothing
	    ; effects: if new size is smaller than allowed size of
	    ;            contents, frame resized to smallest possible size.
	    ;            If frame is larger than contents and contents
	    ;            aren't stretchable, frame resized to size of
	    ;            contents.  Each direction is handled
	    ;            independently.
	    [on-size
	     (opt-lambda (new-width new-height [force? #f])
	       (mred:debug:printf 'container-frame-on-size
				  (string-append
				   "container-frame-on-size: "
				   "Entered frame's on-size; args ~s ~s")
				  new-width new-height)
	       (super-on-size new-width new-height)
	       (let ([new-width (get-width)]
		     [new-height (get-height)])
		 (let-values ([(correct-w correct-h)
			       (correct-size new-width new-height)])
		   (mred:debug:printf 'container-frame-on-size
			   (string-append
			    "container-frame-on-size: "
			    "Correct size ~s ~s")
			   correct-w correct-h)
		   (unless (and (= new-width correct-w)
				(= new-height correct-h)
				(not force?))
		     (mred:debug:printf 
		      'container-frame-on-size
		      (string-append
		       "container-frame-on-size: "
		       "resizing frame to correct size"))
		     (set-size -1 -1 correct-w correct-h))
		   (mred:debug:printf         
		    'container-frame-on-size
		    (string-append
		     "container-frame-on-size: "
		     "Leaving onsize at the end.")))))])

	  (sequence
	    (apply super-init args)
	    (set! object-ID counter)
	    (set! counter (add1 counter))))))
    
    (define frame%
      (class (make-top-container% mred:connections:connections-frame%) args
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
      (class (make-top-container% mred:connections:connections-dialog-box%) args
	(inherit
	  centre)
	(rename
	  [super-show show])
	(public
	  
	 [on-default-action
	  (lambda (item)
	    (send item on-default-action))]

	  ; show: shows/hides the dialog and optionally centers it
	  ; on-screen
	  ; input: now: a boolean; #t to show window, #f to hide it
	  ;        center: an optional boolean; #t to center, #f not.
	  ; returns: nothing
	  ; effects: shows or hides window; if now & center are both #t,
	  ;   centers window on-screen as well.
	  [show
	   (opt-lambda (now [center #t])
	     (when (and now center) (centre wx:const-both))
	     (super-show now))])
	(sequence
	  (apply (opt-lambda (parent title
				     [modal #f]
				     [x default-dialog-posn]
				     [y default-dialog-posn]
				     [w default-dialog-size]
				     [h default-dialog-size]
				     [style wx:const-default-dialog-style]
				     [name "dialogBox"])
		   (super-init parent title modal x y w h
			       (bitwise-ior style wx:const-allow-auto-resize)
			       name))
		 args)))))
