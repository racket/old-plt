
(define mred:canvas@
  (unit/sig mred:canvas^
    (import [mred:debug : mred:debug^] 
	    [mred:container : mred:container^]
	    [mred:edit : mred:edit^]
	    [mzlib:file : mzlib:file^])
	    
    (mred:debug:printf 'invoke "mred:canvas@")

    (define make-editor-canvas%
      (lambda (super%)
	(class super% (parent [x -1] [y -1] [w -1] [h -1] 
			      [name ""] [style 0] [spp 100] [m ()])
	  (sequence (mred:debug:printf 'creation "creating a canvas"))
	  (public
	    [get-edit% (lambda () mred:edit:edit%)]
	    [style-flags 0])
	  (private
	    [name-message #f]
	    [save-button #f])
	  (inherit get-media)
	  (public
	    [get-style-flags (lambda () style-flags)]
	    [make-edit (lambda () (make-object (get-edit%)))]
	    [make-initial-edit make-edit]
	    [edit-modified
	     (lambda (mod?)
	       (if save-button
		   (send save-button show mod?)))]
	    [edit-renamed
	     (lambda (name)
	       (if name-message
		   (let ([name (if (null? name) "Untitled" name)])
		     (send name-message set-label 
			   (or (mzlib:file:file-name-from-path name) "Untitled")))))])
	  (sequence
	    (super-init parent x y w h
			name (bitwise-ior style (get-style-flags)) 
			spp
			(if (null? m)
			    (make-initial-edit)
			    m))))))

    (define editor-canvas% (make-editor-canvas% mred:container:media-canvas%))

    (define make-simple-frame-canvas%
      (lambda (super%)
	(class super% args
	  (inherit get-media edit-modified get-parent)
	  (rename
	    [super-on-size on-size]
	    [super-edit-renamed edit-renamed]
	    [super-force-redraw force-redraw]
	    [super-set-media set-media]
	    [super-on-set-focus on-set-focus])
	  (private
	    [set-frame-title
	     (lambda ()
	       (when frame
		 (let* ([edit (get-media)]
			[title 
			 (if (null? edit)
			     "MrEd"
			     (let ([fullname (send edit get-filename)])
			       (if (null? fullname)
				   ""
				   (or (mzlib:file:file-name-from-path
					fullname)
				       "Untitled"))))])
		   (send frame set-title title))))])
	  (public
	    [frame #f]
	    [set-frame (lambda (f) (set! frame f))]
	    [make-initial-edit (lambda () '())]
	    [edit-renamed
	     (lambda (name)
	       (set-frame-title)
	       (super-edit-renamed name))]
	    [on-set-focus
	     (lambda ()
	       (let ([m (get-media)])
		 (unless (null? m)
		   (send m set-active-canvas this)))
	       (set-frame-title)
	       (super-on-set-focus)
	       (when frame
		 (send frame on-frame-active)))])
	  (private
	    [resize-edit
	       ;; only resize the edit when the canvas is in a good state,
	       ;; a good state is when the container classes are 
	       ;; force redrawing or when the frame is shown.
	     (lambda ()
	       (let ([frame-shown?
		      (let loop ([t (get-parent)])
			(cond
			  [(is-a? t wx:frame%)
			   (ivar t shown)]
			  [(null? t) #f]
			  [else (loop (send t get-parent))]))])
		 (when frame-shown?
		   (let ([edit (get-media)])
		     (unless (null? edit)
		       (mred:debug:printf 'rewrap "canvas on-size rewrapping: ~a ~a~n"
					  must-resize-edit frame-shown?)
		       (send edit rewrap))))))]
	    [must-resize-edit #f])
	  (public
	    [force-redraw 
	     (lambda ()
	       (set! must-resize-edit #t)
	       (super-force-redraw))]
	    [on-size
	     (lambda (width height)
	       (super-on-size width height)
	       (resize-edit))]
	    [set-media
	     (opt-lambda (m [redraw? #t])
	       (let ([old-m (get-media)])
		 (unless (null? old-m)
		   (send old-m remove-canvas this)))
	       (super-set-media m redraw?)
	       (if (null? m)
		   (begin
		     (edit-modified #f)
		     (edit-renamed "No buffer"))
		   (begin
		     (send m add-canvas this)
		     (resize-edit)
		     (edit-modified (send m modified?))
		     (edit-renamed (send m get-filename))))
	       (set-frame-title))])
	  (sequence
	    (apply super-init args)))))

    (define simple-frame-canvas% (make-simple-frame-canvas% editor-canvas%))
    
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
    
    (define one-line-canvas%
      (make-one-line-canvas% simple-frame-canvas%))))
