;;; Define the standard editing window class

;; mini-panels must inheirit from wx:panel% and must provide a
;; desired-height method that returns the desired height in pixels.

(define mred:editor-frame@
  (unit/sig mred:editor-frame^
    (import [mred:debug : mred:debug^]
	    [mred:preferences : mred:preferences^]
	    [mred:edit : mred:edit^]
	    [mred:frame : mred:frame^]
	    [mred:container : mred:container^]
	    [mred:canvas : mred:canvas^]
	    [mred:find-string : mred:find-string^]
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
	    
    (mred:debug:printf 'invoke "mred:editor-frame@")

    (define make-editor-frame%
      (lambda (super%)
	(class super% ([filename #f][show? #t][frameset mred:group:frames])
	  (inherit active-edit active-canvas make-menu make-edit
		   show add-canvas remove-canvas canvases panel)
	  (rename [super-on-close on-close]
		  [super-make-menu-bar make-menu-bar]
		  [super-on-menu-command on-menu-command]
		  [super-next-menu-id next-menu-id])
	  (public
	    [allow-split? #t])
	  (private 
	    [other-offset 0]
	    
	    [frames frameset]
	    [keep-buffers? (is-a? frameset mred:group:frame-group%)]
	    [buffers (if keep-buffers? (ivar frames buffers))]
	    
	    [save-as
	     (opt-lambda ([format wx:const-media-ff-same])
	       (let ([file (mred:finder:put-file)])
		 (when file
		   (send (active-edit) save-file file format))))])
	  
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
	       '(printf "check-save?: ~a canvas: ~a~n" check-save? canvas)
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
		     '(printf "final-filename: ~a edit: ~a exists: ~a dir: ~a~n"
			     filename edit (file-exists? filename)
			     (current-directory))
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
	  (private [font-offset 0])
	  (public
	    [on-menu-command
	     (lambda (op)
	       (cond
		 [(< op font-offset) (begin (send (active-edit) do-font op)
					    #t)]
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

	    [file-menu:new (lambda () 
			     (if (is-a? frameset mred:group:frame-group%)
				 (send frames new-frame #f)
				 (mred:handler:edit-file #f))
			     #t)]
	    [file-menu:revert 
	     (lambda () 
	       (let* ([e (active-edit)]
		      [b (box #f)]
		      [filename (send e get-filename b)])
		 (if (or (null? filename) (unbox b))
		     (wx:bell)
		     (send e load-file filename))
		 #t))]
	    [file-menu:save (lambda () 
			      (send (active-edit) save-file)
			      #t)]
	    [file-menu:save-as (lambda () (save-as) #t)]
	    [file-menu:close (lambda () 
			       (when (on-close) (show #f))
			       #t)]
	    [file-menu:between-open-and-save
	     (lambda (file-menu)
	       (if keep-buffers?
		   (send file-menu append-item "Switch to..."
			 (lambda () (send buffers pick (active-canvas)))))
	       (send file-menu append-separator))]
	    [file-menu:print (lambda () (send (active-edit) print '()) #t)]
	    [file-menu:between-save-and-print
	     (lambda (file-menu)
	       (send file-menu append-item "Save As Text..."
		     (lambda () (save-as wx:const-media-ff-text)))
	       (send file-menu append-item "Save As Text and Styles..."
		     (lambda () (save-as wx:const-media-ff-std)))
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
	    [edit-menu:do (lambda (const) (lambda () (send (active-edit) do-edit const) #t))])

	  (public
	    [edit-menu:undo (edit-menu:do wx:const-edit-undo)]
	    [edit-menu:redo (edit-menu:do wx:const-edit-redo)]
	    [edit-menu:cut (edit-menu:do wx:const-edit-cut)]
	    [edit-menu:clear (edit-menu:do wx:const-edit-clear)]
	    [edit-menu:copy (edit-menu:do wx:const-edit-copy)]
	    [edit-menu:paste (edit-menu:do wx:const-edit-paste)]
	    [edit-menu:select-all (edit-menu:do wx:const-edit-select-all)]
	    [edit-menu:replace (lambda ()
				 (mred:find-string:find-string
				  (active-canvas)
				  (active-edit)
				  -1 -1 (list 'replace 'ignore-case)))]

	    [edit-menu:between-replace-and-preferences
	     (lambda (edit-menu)
	       (send edit-menu append-separator)
	       (send edit-menu append-item "Insert Text Box"
		     (edit-menu:do wx:const-edit-insert-text-box))
	       (send edit-menu append-item "Insert Graphic Box"
		     (edit-menu:do wx:const-edit-insert-graphic-box))
	       (send edit-menu append-item "Insert Image..."
		     (edit-menu:do wx:const-edit-insert-image))
	       (send edit-menu append-item "Toggle Wrap Text"
		     (lambda ()
		       (let ([edit (active-edit)])
			 (send edit set-auto-set-wrap (not (ivar edit auto-set-wrap?)))
			 (send (active-canvas) force-redraw))))
	       (send edit-menu append-separator))]

	    [allow-font-menu? #t]
	  [make-menu-bar
	   (lambda ()
	     (let ([mb (super-make-menu-bar)])
	       (when allow-font-menu?
		 (let ([font-menu (make-menu)])
		   (send mb append font-menu "Fon&t")
		   (set! font-offset (send (make-object wx:media-edit%)
					   append-font-items 
					   font-menu 0))))
	       mb))])
	  (sequence
	    (mred:debug:printf 'super-init "before mred:editor-frame%")
	    (begin0
	      (super-init)
	      (mred:debug:printf 'super-init "after mred:editor-frame%")))
	  
	  (public
	    [exit-callback-tag
	     (if (not keep-buffers?)
		 (mred:exit:insert-exit-callback check-all-saved-for-quit))])
	  
	  (public
	    [edit (active-edit)])
	  (sequence
	    
	    (if keep-buffers?
		(send frames insert-frame this)
		(mred:autosave:register-autosave this))
	    
	    (let ([filename (if (string? filename)
				(mzlib:file:normalize-path filename)
				filename)])
	      (open-file filename))
	    (when show? 
	      (show #t)
	      (send (active-canvas) set-focus))))))

    (define editor-frame% (make-editor-frame%
			   (mred:find-string:make-searchable-frame%
			    mred:frame:simple-menu-frame%)))

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

