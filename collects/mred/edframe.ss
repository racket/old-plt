
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
	  (inherit make-menu show save-as
		   make-edit active-edit
		   get-edit get-canvas)
	  (rename [super-on-close on-close]
		  [super-make-menu-bar make-menu-bar]
		  [super-on-menu-command on-menu-command]
		  [super-next-menu-id next-menu-id])
	  (private 
	    [other-offset 0]
	    
	    [frames frameset]
	    [keep-buffers? (is-a? frameset mred:group:frame-group%)]
	    [buffers (if keep-buffers? (ivar frames buffers))])
	  
	  (public
	    [file-menu:new (lambda () 
			     (if (is-a? frameset mred:group:frame-group%)
				 (send frames new-frame #f)
				 (mred:handler:edit-file #f))
			     #t)]


	    [file-menu:between-open-and-save
	     (lambda (file-menu)
	       (when keep-buffers?
		 (send file-menu append-item "Switch to..."
		       (lambda ()
			 (if (get-canvas)
			     (send buffers pick (get-canvas))
			     (wx:bell)))))
	       (send file-menu append-separator))]
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
	    [auto-save? #t]
	    [on-frame-active
	     (lambda ()
	       (if keep-buffers?
		   (send frames set-active-frame this)))]
	    [check-saved
	     (opt-lambda (edit [reason "Close"])
	       (let* ([name (send edit get-filename)]
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
	     (opt-lambda (orig-filename [canvas (get-canvas)]
					[check-save? (not keep-buffers?)])
	       ; filename = () => ask user
	       ; filename = #f => no file, make untitled
	       ; filename = <buffer> => use buffer
	       ; otherwise, filename = name string
	       '(printf "open-file; test: ~a~n" (or (not check-save?)
						   (check-saved (send canvas get-media))))
	       (if (or (not check-save?)
		       (check-saved (send canvas get-media)))
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
			   (pick-mode edit)
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
	    
	    [check-all-saved-for-quit
	     (lambda () 
	       (check-all-saved "Quit"))]
	    [check-all-saved
	     (opt-lambda ([reason "Close"])
	       (check-saved (get-edit) reason))]
	    
	    [on-close
	     (lambda ()
	       (if (and (super-on-close)
			(or (not keep-buffers?)
			    (send frames remove-frame this)))
		   (if keep-buffers?
		       (begin
			 (send (get-canvas) set-media '())
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
	       (when auto-save?
		 (let ([m (get-edit)])
		   (unless (null? m)
		     (send m do-autosave)))))])


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
	  
	  (public
	    [exit-callback-tag
	     (if (not keep-buffers?)
		 (mred:exit:insert-exit-callback check-all-saved-for-quit))])
	  
	  (sequence
	    
	    (if keep-buffers?
		(send frames insert-frame this)
		(mred:autosave:register-autosave this))
	    
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
			   (mred:find-string:make-searchable-frame%
			    mred:frame:simple-menu-frame%)))

    (define make-pasteboard-frame%
      (lambda (super%)
	(class-asi super%
		   (public
		    [get-edit% (lambda () mred:edit:pasteboard%)]))))

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

