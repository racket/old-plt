  (unit/sig mred:editor-frame^
    (import [wx : wx^]
	    [mred:constants : mred:constants^]
	    [mred:preferences : mred:preferences^]
	    [mred:edit : mred:edit^]
	    [mred:frame : mred:frame^]
	    [mred:container : mred:container^]
	    [mred:canvas : mred:canvas^]
	    [mred:find-string : mred:find-string^]
	    [mred:menu : mred:menu^] 
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
	(class super% ([filename #f][show? #t])
	  (inherit make-menu show save-as
		   make-edit active-edit
		   get-edit get-canvas)
	  (rename [super-can-close? can-close?]
		  [super-make-menu-bar make-menu-bar]
		  [super-on-menu-command on-menu-command]
		  [super-next-menu-id next-menu-id])
	  (private 
	    [other-offset 0])
	  
	  (public
	    [file-menu:between-save-and-print
	     (lambda (file-menu)
	       (send file-menu append-item "Save As Text..."
		     (lambda () (save-as wx:const-media-ff-text)))
	       (send file-menu append-item "Save As Text and Styles..."
		     (lambda () (save-as wx:const-media-ff-std)))
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
	       (send file-menu append-separator))]
	    [check-saved
	     (opt-lambda ([reason "Close"])
	       (let* ([edit (get-edit)]
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
	    [next-menu-id
	     (lambda ()
	       other-offset)]
	    [pick-mode
	     (lambda (edit)
	       (let* ([filename (send edit get-filename)]
		      [mode-handler (if (null? filename)
					#f
					(mred:handler:find-mode-handler
					 filename))])
		 (when mode-handler
		   (let ([mode (mode-handler edit)])
		     (send edit set-mode mode)))))]

	    [open-file
	     (opt-lambda (orig-filename [canvas (get-canvas)])
	       ; filename = () => ask user
	       ; filename = #f => no file, make untitled
	       ; filename = <buffer> => use buffer
	       ; otherwise, filename = name string
	       (if (check-saved)
		   (let* ([filename
			   (if (null? orig-filename)
			       (parameterize ([mred:finder:dialog-parent-parameter
					       this])
				 (mred:finder:get-file))
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
			     (else "Untitled"))]
			  ; Look for an existing buffer for this file
			  [edit
			   (if (object? orig-filename)
			       orig-filename
			       #f)])
		     ; If we didn't find a buffer, create one
		     (if (not edit)
			 (let ([edit (make-edit)])
			   ; Load in the file, if it exists
			   (if (and (not untitled?)
				    (file-exists? filename))
			       (let ([completed? (send edit load-file 
						       filename #f)])
				 (unless completed?
				   (mred:gui-utils:message-box
				    (format
				     "Unable to load file ~a"
				     filename)
				    "Error loading file")))
			       (begin
				 (send edit erase)
				 (send edit set-filename filename 
				       untitled?)))
			   ; Pick the editing mode
			   (pick-mode edit)
			   (send canvas set-media edit))
			 ; Found an existing edit
			 (send canvas set-media edit))
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
	    
	    [can-close?
	     (lambda ()
	       (and (check-saved)
		    (super-can-close?)))])
	  (public
	    [get-edit% (lambda () mred:edit:backup-autosave-edit%)])

	  (public
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
	    (super-init)
	    (mred:debug:printf 'super-init "after mred:editor-frame%"))
	  
	  (sequence
	    (let ([filename (if (and (string? filename) (file-exists? filename))
				(mzlib:file:normalize-path filename)
				filename)])
	      (when filename
		(send (get-edit) load-file filename)
		(pick-mode (get-edit))
		(open-file (get-edit))))
	    (when show?
	      (show #t)
	      (send (get-canvas) set-focus))))))

    (define editor-frame% (make-editor-frame% 
			   mred:frame:info-frame%))

    (define make-status-frame%
      (lambda (super%)
	(class-asi super%
	  (rename [super-get-canvas% get-canvas%])
	  (public
	    [get-canvas%
	     (lambda ()
	       (class-asi super-get-canvas%
		 (public [use-panel? #t])))])))))

