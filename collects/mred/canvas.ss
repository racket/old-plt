(define mred:canvas@
  (unit/s mred:canvas^
    (import [mred:debug mred:debug^] [mred:edit mred:edit^]
	    [mzlib:file mzlib:file^])

    (mred:debug:printf "mred:canvas@")

    (define make-editor-canvas%
      (lambda (super%)
	(class super% (parent [x -1] [y -1] [w -1] [h -1] 
			      [name ""] [style 0] [spp 100] [m ()])
	       (public
		[edit% mred:edit:edit%]
		[style-flags 0]
		[use-panel? #f]
		[panel% wx:panel%]
		
		[NAME-WIDTH 50]
		[PANEL-HEIGHT 30])
	       (private
		[name-message #f]
		[save-button #f])
	       (inherit get-media)
	       (public
		[get-edit% (lambda () edit%)]
		[get-style-flags (lambda () style-flags)]
		[get-panel% (lambda () panel%)]
		[make-edit (lambda () (make-object (get-edit%)))]
		[make-initial-edit make-edit]
		[panel #f]
		[make-panel
		 (lambda (frame x y)
		   (let ([panel (make-object (get-panel%)
				  frame x y -1 PANEL-HEIGHT)]
			 [m (get-media)])
		     (set! name-message
			   (make-object wx:message% panel
					(let* ([name
						(if (null? m)
						    ()
						    (send m get-filename))]
					       [name
						(if (null? name)
						    "Untitled"
						    (or (mzlib:file:file-name-from-path name)
							"Untitled"))])
					  name)
					-1 -1))
		     (set! save-button 
			   (make-object wx:button% panel
					(lambda args
					  (let ([edit (get-media)])
					    (unless (null? edit)
					      (send edit save-file))))
					"Save"
					150))
		     (let ([m-h (box 0)]
			   [m-w (box 0)]
			   [m-x (box 0)]
			   [m-y (box 0)]
			   [b-h (box 0)]
			   [b-w (box 0)])
		       (send name-message get-size m-w m-h)
		       (send name-message get-position m-x m-y)
		       (send save-button get-size b-w b-h)
		       (send name-message set-size 
			     -1 (+ (unbox m-y)
				   (/ (- (unbox b-h) (unbox m-h))
				      2)) 
			     -1 -1))
		     panel))]
		[init-panel
		 (lambda ()
		   (let ([m (get-media)])
		     (if (or (null? m)
			     (not (send m modified?)))
			 (send save-button show #f))))]
		[create-panel
		 (opt-lambda (frame [x -1] [y -1])
		   (set! panel (make-panel frame x y))
		   (init-panel)
		   (when panel
		     (send panel new-line)
		     (send panel fit)))]
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

    (define editor-canvas% (make-editor-canvas% wx:media-canvas%))

    (define make-simple-frame-canvas%
      (lambda (super%)
	(class super% args
	       (inherit get-media edit-modified edit-renamed)
	       (rename
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
		[make-initial-edit (lambda () '())]
		[frame #f]
		[set-frame
		 (lambda (f)
		   (set! frame f))]
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
