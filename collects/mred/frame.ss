;;; Define the standard editing window class

;; mini-panels must inheirit from wx:panel% and must provide a
;; desired-height method that returns the desired height in pixels.

(define mred:frame@
  (unit/sig mred:frame^
    (import [mred:debug : mred:debug^]
	    [mred:edit : mred:edit^]
	    [mred:canvas : mred:canvas^]
	    [mred:icon : mred:icon^]
	    [mred:menu : mred:menu^] 
	    [mred:group : mred:group^]
	    [mred:finder : mred:finder^]
	    [mred:handler : mred:handler^]
	    [mred:exit : mred:exit^]
	    [mred:autosave : mred:autosave^]
	    [mred:gui-utils : mred:gui-utils^]
	    [mzlib:function : mzlib:function^]
	    [mzlib:file : mzlib:file^])
	    
    (mred:debug:printf 'invoke "mred:frame@")

    (define frame-name "MrEd")

    (define empty-frame%
      (class-asi wx:frame%
	(inherit show)
	(public
	  [on-close (lambda () #t)])))

    (define frame-width 600)
    (define frame-height 600)
    (let ([w (box 0)]
	  [h (box 0)])
      (wx:display-size w h)
      (if (< (unbox w) frame-width)
	  (set! frame-width (- (unbox w) 65)))
      (if (< (unbox h) frame-height)
	  (set! frame-height (- (unbox h) 65))))

    ; This defines a simple editing frame without a menu bar.
    ; On initialization, it doesn't automatically show itself
    (define make-simple-frame%
      (lambda (super%)
	(class super% ([name frame-name])
	  (inherit get-client-size get-title set-title set-icon)
	  (rename [super-on-size on-size]
		  [super-on-close on-close])
	  ; Overrideable constants
	  (public
	    [MINI-PANEL-HEIGHT 30]
	    
	    [WIDTH frame-width]
	    [HEIGHT frame-height]
	    [MARGIN 2]
	    
	    [TOP-MARGIN 0]
	    [BOTTOM-MARGIN 0]
	    [LEFT-MARGIN 0]
	    [RIGHT-MARGIN 0])
	  
	  (public
	    [edit% mred:edit:edit%]
	    [canvas% mred:canvas:simple-frame-canvas%]
	    
	    [set-last-focus-canvas
	     (lambda (c)
	       (set! last-focus-canvas c))]
	    [title-prefix ""])
	  
	  
	  ; we need to call this one directly
	  (public
	    [on-simple-size 
	     ; Window resize -> resize canvases
	     (lambda ()
	       (when mini-panel
		 (set! MINI-PANEL-HEIGHT (send mini-panel desired-height)))
	       (if (not (void? canvas))
		   (let ((client-w (box 0))
			 (client-h (box 0)))
		     (get-client-size client-w client-h)
		     (let* ([w (- (unbox client-w) (* 2 MARGIN)
				  LEFT-MARGIN RIGHT-MARGIN)]
			    [total-h (- (unbox client-h) (* 2 MARGIN)
					TOP-MARGIN BOTTOM-MARGIN
					(if mini-panel
					    MINI-PANEL-HEIGHT
					    0))]
			    [count (length canvases)]
			    [h (if (zero? count) 10 (floor (/ total-h count)))])
		       (when mini-panel
			 (send mini-panel set-size 0 total-h w MINI-PANEL-HEIGHT))
		       (let loop ([canvases canvases]
				  [v (+ MARGIN TOP-MARGIN)])
			 (unless (null? canvases)
			   (let* ([canvas (car canvases)]
				  [c-panel (if (ivar canvas use-panel?)
					       (ivar canvas panel)
					       #f)]
				  [panel-w-box (box 0)]
				  [panel-h-box (box 0)])
			     (when c-panel
			       (send c-panel set-size 
				     (+ MARGIN LEFT-MARGIN) v
				     w -1)
			       (send c-panel get-size
				     panel-w-box panel-h-box))
			     (send canvas set-size 
				   (+ MARGIN LEFT-MARGIN) 
				   (+ v (unbox panel-h-box))
				   w (- h (unbox panel-h-box)))
			     (let ([edit (send canvas get-media)])
			       (if (and (not (null? edit))
					(ivar edit auto-set-wrap?))
				   (let ([admin (send edit get-admin)]
					 [w-box (box 0)]
					 [h-box (box 0)])
				     (unless (null? admin)
				       (send admin get-view 
					     () () w-box h-box #f)
				       (send edit set-max-width 
					     (unbox w-box)))))))
			   (loop (cdr canvases) (+ v h))))))))])
	  
	  ; methods
	  (public
	    [on-frame-active (lambda () (void))]
	    [set-title-prefix
	     (lambda (s)
	       (if (string? s)
		   (set! title-prefix s)))]
	    [get-canvas% (lambda () canvas%)]
	    [make-canvas
	     (lambda ()
	       (let ([canvas (make-object (get-canvas%) this)])
		 (when (ivar canvas use-panel?)
		   (send canvas create-panel this))
		 canvas))]
	    [get-edit% (lambda () edit%)]
	    [make-edit
	     (lambda ()
	       (make-object (get-edit%)))]
	    [on-size
	     (lambda (w h)
	       (super-on-size w h)
	       (on-simple-size))]
	    
	    [add-canvas
	     (opt-lambda ([canvas (make-canvas)][prefix? #f])
	       (unless (member canvas canvases)
		 (set! canvases 
		       (if prefix?
			   (cons canvas canvases)
			   (append canvases (list canvas))))
		 (send canvas set-frame this)
		 (on-simple-size))
	       canvas)]
	    [remove-canvas
	     (lambda (canvas)
	       (let ([pre-len (length canvases)])
		 (set! canvases
		       (mzlib:function:remove 
			canvas canvases
			(lambda (canvas c)
			  (if (eq? c canvas)
			      (begin
				(send canvas set-frame #f)
				(send canvas set-size 50000 50000 10 10)
				(when (ivar canvas use-panel?)
				  (send (ivar canvas panel)
					set-size 50000 50000 -1 -1))
				(if (eq? canvas last-focus-canvas)
				    (set! last-focus-canvas #f))
				#t)
			      #f))))
		 (if (< (length canvases) pre-len)
		     (on-simple-size))))]
	    
	    [active-canvas
	     (lambda ()
	       (let loop ([canvases canvases])
		 (cond
		   [(null? canvases) last-focus-canvas]
		   [(send (car canvases) is-focus-on?) (car canvases)]
		   [else (loop (cdr canvases))])))]
	    [active-edit
	     (lambda ()
	       (send (active-canvas) get-media))])
	  
	  ; Initialization:
	  (sequence
	    (super-init () name -1 -1 WIDTH HEIGHT
			(+ wx:const-default-frame wx:const-sdi)
			name))
	  
	  (private
	    [mini-panel #f])
	  (public
	    [set-mini-panel%
	     (lambda (c%)
	       (set! mini-panel (make-object c% this 50000 50000 100 100))
	       (send mini-panel set-focus)
	       (on-simple-size))]
	    [get-mini-panel (lambda () mini-panel)]
	    [clear-mini-panel%
	     (lambda ()
	       (when mini-panel
		 (send (active-canvas) set-focus)
		 (send mini-panel show #f)
		 (send mini-panel set-size 50000 50000 30 30)
		 (set! mini-panel #f)
		 (on-simple-size)))])
	  
	  (public
	    [last-focus-canvas #f] ; Does this need to be inited during make-canvas?
	    [canvas (make-canvas)])
	  (sequence
	    (when (null? (send canvas get-media))
	      (send canvas set-media (make-edit)))
	    (send canvas set-frame this)
	    (set! last-focus-canvas canvas))
	  (public
	    [edit (send canvas get-media)]
	    [canvases (list canvas)])
	  (sequence
	    (if (send mred:icon:icon ok?)
		(set-icon mred:icon:icon))
	    (on-simple-size)))))

    (define simple-frame% (make-simple-frame% empty-frame%))

    (define make-menu-frame%
      (lambda (super%)
	(class super% args
	  (inherit set-menu-bar get-menu-bar)
	  (public
	    [menu% mred:menu:menu%]
	    [menu-bar% mred:menu:menu-bar%])
	  (public
	    [get-menu-bar% (lambda () menu-bar%)]
	    [get-menu% (lambda () menu%)]
	    [next-menu-id (lambda () 0)]
	    [make-menu-bar
	     (lambda ()
	       (make-object (get-menu-bar%)))]
	    [make-menu
	     (lambda ()
	       (make-object (get-menu%)))]
	    [on-menu-command
	     (lambda (op)
	       (send (get-menu-bar) dispatch op))])
	  (sequence
	    (apply super-init args)
	    
	    ; Build and install the menu bar:
	    (let ([menu-bar (make-menu-bar)])
	      (set-menu-bar menu-bar))))))

    (define menu-frame% (make-menu-frame% empty-frame%))

    (define simple-menu-frame% (make-menu-frame% simple-frame%))

    (define make-standard-menus-frame%
      (lambda (super%)
	(class-asi super%
	  (inherit make-menu)
	  (rename [super-make-menu-bar make-menu-bar])

	  (public
	    [file-menu:between-open-and-save 
	     (lambda (file-menu) (send file-menu append-separator))]
	    [file-menu:between-save-and-close 
	     (lambda (file-menu) (send file-menu append-separator))]
	    [file-menu:after-close (lambda (file-menu) (void))]
	    [edit-menu:after-std-items (lambda (edit-menu) (void))]

	    [file-menu:open (lambda () (void))]
	    [file-menu:open-string "&Open..."]

	    [file-menu:new (lambda () (void))]
	    [file-menu:new-string "&New..."]

	    [file-menu:revert (lambda () (void))]
	    [file-menu:revert-string "&Revert"]

	    [file-menu:save (lambda () (void))]
	    [file-menu:save-string "&Save"]

	    [file-menu:save-as (lambda () (void))]
	    [file-menu:save-as-string "Save &As..."]

	    [file-menu:close (lambda () (void))]
	    [file-menu:close-string "&Close"]

	    [edit-menu:undo (lambda () (void))]
	    [edit-menu:undo-string "&Undo"]

	    [edit-menu:redo (lambda () (void))]
	    [edit-menu:redo-string "&Redo"]

	    [edit-menu:clear (lambda () (void))]
	    [edit-menu:clear-string (if (eq? wx:platform 'windows)
					"&Delete"
					"Clear")]

	    [edit-menu:copy (lambda () (void))]
	    [edit-menu:copy-string "&Copy"]

	    [edit-menu:cut (lambda () (void))]
	    [edit-menu:cut-string "Cu&t"]

	    [edit-menu:paste (lambda () (void))]
	    [edit-menu:paste-string "&Paste"]

	    [edit-menu:select-all (lambda () (void))]
	    [edit-menu:select-all-string "Select A&ll"]

	    [make-menu-bar
	     (lambda ()
	       (let ([file-menu (make-menu)]
		     [edit-menu (make-menu)])
		 (send file-menu append-item file-menu:open-string file-menu:open)
		 (send file-menu append-item file-menu:new-string file-menu:new)
		 (send file-menu append-item file-menu:revert-string file-menu:revert)
		 (file-menu:between-open-and-save file-menu)
		 (send file-menu append-item file-menu:save-string file-menu:save)
		 (send file-menu append-item file-menu:save-as-string file-menu:save-as)
		 (file-menu:between-save-and-close file-menu)
		 (send file-menu append-item file-menu:close-string file-menu:close)
		 (file-menu:after-close file-menu)
		 
		 (send edit-menu append-item edit-menu:undo-string edit-menu:undo)
		 (send edit-menu append-item edit-menu:redo-string edit-menu:redo)
		 (send edit-menu append-separator)
		 (send edit-menu append-item edit-menu:cut-string edit-menu:cut)
		 (send edit-menu append-item edit-menu:copy-string edit-menu:copy)
		 (send edit-menu append-item edit-menu:paste-string edit-menu:paste)
		 (send edit-menu append-item edit-menu:clear-string edit-menu:clear)
		 (send edit-menu append-item edit-menu:select-all-string edit-menu:select-all)
		 (edit-menu:after-std-items edit-menu)
		 
		 (let ([mb (super-make-menu-bar)])
		   (send mb append file-menu "File")
		   (send mb append edit-menu "Edit")
		   mb)))]))))

    ; This defines the standard editing window.
    (define standard-menus-frame% (make-standard-menus-frame% simple-menu-frame%))

    (define make-editor-frame%
      (lambda (super%)
	(class super% ([filename #f][show? #t][frameset mred:group:frames])
	  (inherit active-edit active-canvas make-menu make-edit
		   show add-canvas remove-canvas canvases on-simple-size)
	  (rename [super-on-close on-close]
		  [super-make-menu-bar make-menu-bar]
		  [super-on-menu-command on-menu-command]
		  [super-next-menu-id next-menu-id])
	  (public
	    [allow-split? #t])
	  (private 
	    [other-offset 0]
	    [keep-buffers? (is-a? frameset mred:group:frame-group%)]
	    
	    [frames frameset]
	    [buffers (if keep-buffers? (ivar frames buffers))]
	    
	    [save-as
	     (lambda (format)
	       (let ([file (mred:finder:put-file)])
		 (if file
		     (send edit save-file file format))))])
	  
	  (public
	    [auto-save? #t]
	    [on-frame-active
	     (lambda ()
	       (if keep-buffers?
		   (send frames set-active-frame this)))]
	    [check-saved
	     (opt-lambda (canvas [reason "Close"])
	       (let* ([edit (send canvas get-media)]
		      [name (send edit get-filename)]
		      [name (if (string? name)
				name
				"Untitled")])
		 (or (not (send edit modified?))
		     (let ([action
			    (mred:gui-utils:unsaved-warning name reason #t)])
		       (case action
			 [(save) (send edit save-file)]
			 [(continue) (send edit remove-autosave) #t]
			 [else #f])))))]
	    [get-frame-group
	     (lambda ()
	       (if keep-buffers?
		   frames
		   #f))]
	    [next-menu-id
	     (lambda ()
	       other-offset)]
	    [open-file
	     (opt-lambda (orig-filename [canvas (active-canvas)]
					[check-save? (not keep-buffers?)])
	       ; filename = () => ask user
	       ; filename = #f => no file, make untitled
	       ; filename = <buffer> => use buffer
	       ; otherwise, filename = name string
	       (if (or (not check-save?)
		       (check-saved canvas))
		   (let* ([filename
			   (if (null? orig-filename)
			       (mred:finder:get-file)
			       orig-filename)]
			  ; at this point, name is either a string, buffer, or #f
			  ; Is the buffer going to be untitled?
			  [untitled? (not filename)]
			  ; If filename is still #f, make up a name
			  [filename 
			   (cond
			     ((object? filename)
			      (let ([name (send filename get-filename)])
				(if (null? name)
				    "Untitled"
				    name)))
			     (filename filename)
			     (else (if keep-buffers?
				       (send buffers 
					     make-untitled-name)
				       "Untitled")))]
			  ; Look for an existing buffer for this file
			  [edit
			   (cond
			     ((object? orig-filename) orig-filename)
			     ((or untitled? (not keep-buffers?))
			      #f)
			     (else
			      (send buffers find-buffer-by-name 'file filename)))])
		     ; If we didn't find a buffer, create one
		     (if (not edit)
			 (let ([edit (make-edit)])
			   ; Load in the file, if it exists
			   (if (and (not untitled?)
				    (file-exists? filename))
			       (send edit load-file filename)
			       (begin
				 (send edit erase)
				 (send edit set-filename filename 
				       untitled?)))
			   ; Pick the editing mode
			   (let* ([filename (send edit get-filename)]
				  [mode-handler (if (null? filename)
						    #f
						    (mred:handler:find-mode-handler
						     filename))])
			     (if mode-handler
				 (let ([mode (mode-handler edit)])
				   (send edit set-mode mode))))
			   ; We created this buffer; add to the global list
			   (if keep-buffers?
			       (send buffers add-buffer 'file #f edit))
			   (send canvas set-media edit))
			 ; Found an existing edit
			 (begin
			   (if keep-buffers?
			       (send buffers reset-buffer-state edit))
			   (send canvas set-media edit)))
		     #t)
		   #f))])
	  (private font-offset)
	  (public
	    [on-menu-command
	     (lambda (op)
	       (cond
		 [(< op font-offset)
		  (send (active-edit) do-font op)
		  #t]
		 [else (super-on-menu-command op)]))]
	    
	    [check-nonunique-or-saved
	     (lambda (canvas)
	       (let ([e (send canvas get-media)])
		 (if (ormap
		      (lambda (c)
			(and (not (eq? c canvas))
			     (eq? e (send c get-media))))
		      canvases)
		     #t
		     (check-saved canvas))))]
	    
	    [check-all-saved-for-quit
	     (lambda () 
	       (check-all-saved "Quit"))]
	    [check-all-saved
	     (opt-lambda ([reason "Close"])
	       (andmap (lambda (c) (check-saved c reason)) canvases))]
	    
	    [on-close
	     (lambda ()
	       (if (and (super-on-close)
			(or (not keep-buffers?)
			    (send frames remove-frame this)))
		   (if keep-buffers?
		       (begin
			 (for-each (lambda (canvas)
				     (send canvas set-media '()))
				   canvases)
			 #t)
		       (if (check-all-saved)
			   (begin
			     (mred:exit:remove-exit-callback exit-callback-tag)
			     (set! auto-save? #f)
			     #t)
			   #f))
		   #f))]
	    
	    [do-autosave
	     (lambda ()
	       (if auto-save?
		   (for-each (lambda (canvas)
			       (let ([m (send canvas get-media)])
				 (unless (null? m)
				   (send m do-autosave))))
			     canvases)))]


	    [file-menu:open (lambda () 		
			      (let ((file (mred:finder:get-file)))
				(if file
				    (open-file file))))]
	    [file-menu:new (lambda ()
			     (let ((file (mred:finder:put-file)))
			       (if file
				   (open-file file))))]
	    [file-menu:revert (lambda () (send (active-edit) load-file))]
	    [file-menu:save (lambda () (send (active-edit) save-file))]
	    [file-menu:save-as (lambda () (save-as wx:const-media-ff-same))]
	    [file-menu:close (lambda () (if (on-close) (show #f)))]
	    [file-menu:between-open-and-save
	     (lambda (file-menu)
	       (if keep-buffers?
		   (send file-menu append-item "Switch to..."
			 (lambda () (send buffers pick (active-canvas)))))
	       (send file-menu append-separator)
	       (send file-menu append-item "New Frame"
		     (lambda () 
		       (if keep-buffers?
			   (send frames new-frame #f)
			   (mred:handler:edit-file #f))))
	       (send file-menu append-separator))]
	    [file-menu:between-save-and-close
	     (lambda (file-menu)
	       (send file-menu append-item "Save As Text..."
		     (lambda () (save-as wx:const-media-ff-text)))
	       (send file-menu append-item "Save As Text and Styles..."
		     (lambda () (save-as wx:const-media-ff-std)))
	       (send file-menu append-item "Print..."
		     (lambda () (send (active-edit) print '())))
	       (when allow-split?
		 (send file-menu append-separator)
		 (send file-menu append-item "Split"
		       (lambda () 	       
			 (let ([new-canvas (add-canvas)])
			   (send new-canvas set-media (active-edit)))))
		 (send file-menu append-item "Collapse"
		       (lambda ()
			 (when (> (length canvases) 1)
			   (let ([canvas (active-canvas)])
			     (if (and canvas
				      (or keep-buffers?
					  (check-nonunique-or-saved canvas)))
				 (remove-canvas canvas)))))))
	       (send file-menu append-item "Set Mode..."
		     (lambda ()
		       (let* ([modes (map mred:handler:handler-name 
					  mred:handler:mode-handlers)]
			      [name (wx:get-single-choice
				     "Select a Mode" "Mode"
				     modes)])
			 (unless (null? name)
			   (let* ([handler (mred:handler:find-named-mode-handler name)]
				  [mode (handler (active-edit))])
			     (send (active-edit) set-mode mode))))))
	       (send file-menu append-separator))])

	  (private
	    [edit-menu:do (lambda (const) (lambda () (send (active-edit) do-edit const)))])

	  (public
	    [edit-menu:undo (edit-menu:do wx:const-edit-undo)]
	    [edit-menu:redo (edit-menu:do wx:const-edit-redo)]
	    [edit-menu:cut (edit-menu:do wx:const-edit-cut)]
	    [edit-menu:clear (edit-menu:do wx:const-edit-clear)]
	    [edit-menu:copy (edit-menu:do wx:const-edit-copy)]
	    [edit-menu:paste (edit-menu:do wx:const-edit-paste)]
	    [edit-menu:select-all
	     (lambda ()
	       (send (active-edit) set-position
		     0 (send (active-edit) last-position)))]

	    [edit-menu:after-std-items
	     (lambda (edit-menu)
	       (send edit-menu append-separator)
	       (send edit-menu append-item "Insert Text Box"
		     (edit-menu:do wx:const-edit-insert-text-box))
	       (send edit-menu append-item "Insert Graphic Box"
		     (edit-menu:do wx:const-edit-insert-graphic-box))
	       (send edit-menu append-item "Insert Image"
		     (edit-menu:do wx:const-edit-insert-image))
	       (send edit-menu append-item "Toggle Wrap Text"
		     (lambda ()
		       (let ([edit (active-edit)])
			 (send edit set-auto-set-wrap
			       (not (ivar edit auto-set-wrap?)))
			 (on-simple-size))))
	       (send edit-menu append-separator)
	       (set! font-offset (send (active-edit) append-font-items edit-menu 0)))])

	  (sequence
	    (super-init))
	  
	  (public
	    [exit-callback-tag
	     (if (not keep-buffers?)
		 (mred:exit:insert-exit-callback check-all-saved-for-quit))])
	  
	  (sequence
	    (when show? (show #t))
	    
	    (if keep-buffers?
		(send frames insert-frame this)
		(mred:autosave:register-autosave this))
	    
	    (let ([filename (if (string? filename)
				(mzlib:file:normalize-path filename)
				filename)])
	      (open-file filename))
	    
	    (when show?
	      (wx:yield)
	      (send (active-canvas) set-focus)))
	  (public
	    [edit (active-edit)]))))

    (define editor-frame% (make-editor-frame% standard-menus-frame%))

    (define make-pasteboard-frame%
      (lambda (super%)
	(class-asi super%
		   (public
		    [edit% mred:edit:pasteboard%]))))
    (define pasteboard-frame% (make-pasteboard-frame% editor-frame%))

    (define make-status-frame%
      (lambda (super%)
	(class-asi super%
	  (rename [super-get-canvas% get-canvas%])
	  (public
	    [get-canvas%
	     (lambda ()
	       (class-asi super-get-canvas%
		 (public [use-panel? #t])))]))))))

