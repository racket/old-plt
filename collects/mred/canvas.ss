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
	  (public
	    [edit% mred:edit:edit%]
	    [style-flags 0])
	  (private
	    [name-message #f]
	    [save-button #f])
	  (inherit get-media)
	  (public
	    [get-edit% (lambda () edit%)]
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
	  (inherit get-media edit-modified edit-renamed)
	  (rename
	    [super-on-size on-size]
	    [super-set-media set-media]
	    [super-on-set-focus on-set-focus])
	  (private
	    [set-frame-title
	     (lambda ()
	       (if frame
		   (let* ([edit (get-media)]
			  [title (if (null? edit)
				     "MrEd"
				     (send edit get-filename))]
			  [title (if (string? title)
				     (string-append
				      (ivar frame title-prefix)
				      (or (mzlib:file:file-name-from-path title) "Untitled"))
				     title)])
		     (if (string? title)
			 (let ([old-title (send frame get-title)])
			   (if (not (equal? title old-title))
			       (send frame set-title title)))))))])
	  (public
	    [frame #f]
	    [set-frame (lambda (f) (set! frame f))]
	    [make-initial-edit (lambda () '())]
	    [on-set-focus
	     (lambda ()
	       (if frame
		   (send frame set-last-focus-canvas this))
	       (let ([m (get-media)])
		 (unless (null? m)
		   (send m set-active-canvas this)))
	       (set-frame-title)
	       (super-on-set-focus)
	       (if frame
		   (send frame on-frame-active)))]
	    [on-size
	     (lambda (width height)
	       (super-on-size width height)
	       (let ([edit (get-media)])
		 (if (and (not (null? edit))
			  (ivar edit auto-set-wrap?))
		     (let ([admin (send edit get-admin)]
			   [w-box (box 0)]
			   [h-box (box 0)])
		       (unless (null? admin)
			 (send admin get-view 
			       () () w-box h-box #f)
			 (send edit set-max-width 
			       (unbox w-box)))))))]
	       
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
		     (if (ivar m auto-set-wrap?)	
			 (let ([admin (send m get-admin)]
			       [w-box (box 0)]
			       [h-box (box 0)])
			   (unless (null? admin)
			     (send admin get-view () () w-box h-box #f)
			     (send m set-max-width (unbox w-box)))))
		     (edit-modified (send m modified?))
		     (edit-renamed (send m get-filename))))
	       (set-frame-title))])
	  (sequence
	    (apply super-init args)))))

    (define simple-frame-canvas% (make-simple-frame-canvas% editor-canvas%))))
