
(define mred:canvas@
  (unit/sig mred:canvas^
    (import [mred:debug : mred:debug^] 
	    [mred:container : mred:container^]
	    [mred:edit : mred:edit^]
	    [mzlib:file : mzlib:file^])
	    
    (mred:debug:printf 'invoke "mred:canvas@")

    (define make-wrapping-canvas%
      (lambda (super%)
	(class super% (parent [x -1] [y -1] [w -1] [h -1] 
			      [name ""] [style 0] [spp 100] [m ()])
	  (sequence (mred:debug:printf 'creation "creating a canvas"))
	  (public
	    [get-edit% (lambda () mred:edit:edit%)]
	    [style-flags 0])
	  (inherit get-media get-parent frame)
	  (public
	    [get-style-flags (lambda () style-flags)]
	    [make-edit (lambda () (make-object (get-edit%)))]
	    [make-initial-edit (lambda () null)])

	  (private
	    [rewrap
	     (lambda ()
	       (let ([edit (get-media)])
		 (unless (null? edit)
		   (mred:debug:printf 'rewrap "canvas:rewrap")
		   (send edit rewrap))))]
	    [resize-edit
	       ;; only resize the edit when the container classes are 
	       ;; force redrawing or when the frame is shown.
	     (lambda ()
	       (let ([frame-shown? (ivar frame shown)])
		 (mred:debug:printf 'rewrap "resize-edit: ~a" frame-shown?)
		 (if frame-shown?
		     (rewrap)
		     (set! needs-rewrapping #t))))]
	    [must-resize-edit #f]
	    [needs-rewrapping #f])
	  (rename
	    [super-show show]
	    [super-force-redraw force-redraw]
	    [super-on-size on-size]
	    [super-set-media set-media])

	  (public [edit-modified void]
		  [edit-renamed void])

	  (public
	    [show 
	     (lambda (t)
	       (super-show t)
	       (when (and t
			  needs-rewrapping)
		 (set! needs-rewrapping #f)
		 (rewrap)))]
	    [force-redraw 
	     (lambda ()
	       (set! must-resize-edit #t)
	       (super-force-redraw))]
	    [on-size
	     (lambda (width height)
	       (super-on-size width height)
	       (resize-edit))]
	    [set-media
	     (opt-lambda (media [redraw? #t])
	       (super-set-media media redraw?)
	       (edit-modified (and (not (null? media))
				   (send media modified?)))
	       (edit-renamed (if (null? media)
				 "No buffer"
				 (send media get-filename)))
	       (mred:debug:printf 'rewrap "rewrap: setting the media: ~a" media)
	       (unless (null? media)
		 (mred:debug:printf 'rewrap "rewrap: setting the media")
		 (resize-edit)))])
	  (sequence
	    (super-init parent x y w h
			name (bitwise-ior style (get-style-flags)) 
			spp
			(if (null? m)
			    (make-initial-edit)
			    m))))))

    (define wrapping-canvas% (make-wrapping-canvas% mred:container:media-canvas%))

    (define make-frame-title-canvas%
      (lambda (super%)
	(class super% args
	  (inherit get-media edit-modified get-parent frame)
	  (rename
	    [super-edit-renamed edit-renamed]
	    [super-set-media set-media]
	    [super-on-set-focus on-set-focus])
	  (private
	    [set-frame-title
	     (lambda ()
	       (when frame
		 (let* ([edit (get-media)]
			[title 
			 (if (null? edit)
			     ""
			     (let ([fullname (send edit get-filename)])
			       (if (null? fullname)
				   ""
				   (trim-filename fullname))))])
		   (send frame set-title title))))])
	  (public
	    [trim-filename
	     (lambda (fullname)
	       (or (mzlib:file:file-name-from-path fullname)
		   "Untitled"))]
	    [edit-renamed
	     (lambda (name)
	       (super-edit-renamed name)
	       (set-frame-title))]
	    [on-set-focus
	     (lambda ()
	       (set-frame-title)
	       (super-on-set-focus))])
	  (public
	    [set-media
	     (opt-lambda (m [redraw? #t])
	       (super-set-media m redraw?))])
	  (sequence
	    (apply super-init args)))))

    (define frame-title-canvas% (make-frame-title-canvas% wrapping-canvas%))
    
    (define make-one-line-canvas%
      (lambda (super%)
	(class super% (parent [x -1] [y -1] [w -1] [h -1] 
			      [name ""] [style 0] [spp 100] [m ()])
	  
	  (inherit get-media call-as-primary-owner user-min-height
		   get-size min-height)
	  (rename [super-set-media set-media])
	  (private
	    [update-size
	     (lambda (media)
	       (unless (null? media)
		 (let* ([top (send media line-location 0 #t)]
			[bottom (send media line-location 0 #f)]
			[height (- bottom top)])
		   (let* ([ch (box 0)]
			  [h (box 0)])
		     (call-as-primary-owner
		      (lambda ()
			(send (send media get-admin) 
			      get-view null null null ch)))
		     (get-size (box 0) h)
		     (let ([new-min-height (+ height (- (unbox h) (unbox ch)))])
		       (set! min-height new-min-height)
		       (user-min-height new-min-height))))))])
	  (public
	    [default-y-stretch #f]
	    [set-media
	     (lambda (media)
	       (super-set-media media)
	       (update-size media))])
	  (sequence
	    (super-init parent x y w h name style spp m)
	    (update-size (get-media))))))
    
    (define one-line-canvas% (make-one-line-canvas% wrapping-canvas%))))
