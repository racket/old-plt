(define drscheme:scheme-project-frame-group%
  (class-asi mred:project-frame-group%
    (inherit project)
    (public
      [frame%
       (class drscheme:scheme-project-editor-frame% args
	 (inherit set-project)
	 (sequence
	   (apply super-init args)
	   (set-project project)))])))

(define drscheme:make-scheme-project-frame%
  (lambda (super%)
    (class mred:project-frame% ([filename #f]
				[visible? #t]
				[init-console #f]
				[weak-menus? #f])
      (inherit panel
	       set-modified for-each-file
	       enable set-cursor group
	       make-menu
	       get-selected-files
	       get-client-size
	       get-menu-bar
	       project-item-list
	       project-filename
	       WIDTH)
      (rename
	[super-on-close on-close]
	[super-parse-options parse-options]
	[super-make-options-assoc-list make-options-assoc-list]
	[super-make-menu-bar make-menu-bar]
	[super-on-size on-size]
	[super-set-title set-title])
      (private
	[scheme-type 'std]
	[use-aries? #t])
      (public
	[project-extension "spj"]
	[group% drscheme:scheme-project-frame-group%]
	[console init-console]
	[parse-options
	 (lambda (options error-escape)
	   (let ([type (assq 'scheme-type options)]
		 [aries (assq 'use-aries? options)])
	     (if type
		 (set! scheme-type (cdr type)))
	     (if aries
		 (set! use-aries? (cdr aries)))))]
	[make-options-assoc-list
	 (lambda ()
	   (list*
	    (cons 'scheme-type scheme-type)
	    (cons 'use-aries? use-aries?)
	    (super-make-options-assoc-list)))]
	[set-project-type
	 (lambda ()
	   (let ([dial (make-object wx:dialog-box% () "Project Type" #t)]
		 [new-scheme-type scheme-type]
		 [new-use-aries? use-aries?])
	     (send dial set-label-position wx:const-vertical)
	     (let* ([rbox
		     (make-object wx:radio-box% dial
				  (lambda (self event)
				    (set! new-scheme-type
					  (case (send self get-selection)
					    [0 'std]
					    [1 'wx]
					    [2 'mred])))
				  "Scheme Type:"
				  -1 -1 -1 -1
				  (list "Standard Scheme"
					"wxScheme"
					"MrEd")
				  0 wx:const-vertical)]
		    [cbox
		     (begin
		       (send dial new-line)
		       (make-object wx:check-box% dial
				    (lambda (self event)
				      (set! new-use-aries?
					    (send self get-value)))
				    "Use Aries"))])
	       (send rbox set-selection
		     (case scheme-type
		       [std 0]
		       [wx 1]
		       [else 2]))
	       (send cbox set-value use-aries?))
	     
	     (send dial new-line)
	     
	     (make-object wx:button% dial
			  (lambda (self event)
			    (send dial show #f))
			  "Cancel")
	     (make-object wx:button% dial
			  (lambda (self event)
			    (set! scheme-type new-scheme-type)
			    (set! use-aries? new-use-aries?)
			    (set-modified #t)
			    (send dial show #f)
			    (when (and console (ivar console running?))
			      (send console shut-down)
			      (set! console #f)))
			  "Ok")
	     
	     (send dial fit)
	     (send dial show #t)))]
	[make-menu-bar
	 (lambda ()
	   (let ([mb (super-make-menu-bar)]
		 [project-menu (make-menu)])
	     
	     (send mb append project-menu "Project")

	     (unless weak-menus?
	       (send project-menu append-item "Set Project Type..."
		     set-project-type)
	       (send project-menu append-separator))
	     (send project-menu append-item "Execute Project"
		   load-project-into-scheme)
	     (send project-menu append-item "Execute Selected Files"
		   load-selected-into-scheme)
	     
	     (unless weak-menus?
	       (send project-menu append-separator)
	       (send project-menu append-item "Compile Project..."
		     compile-project))
	     
	     mb))]
	[on-close
	 (lambda ()
	   (if (super-on-close)
	       (begin
		 (if (and console (ivar console running?))
		     (send console shut-down))
		 #t)
	       #f))]
	[on-break
	 (lambda args
	   (if console
	       (send console break)))]
	[set-title
	 (lambda (s)
	   (super-set-title s)
	   (if console
	       (send console set-title-prefix
		     (string-append s ": "))))]
	
	[insure-console-ready
	 (lambda ()
	   (unless (and console (ivar console running?))
	     (set! console 
		   (make-object drscheme:spawned-process-console-frame%
				scheme-type
				use-aries?))
	     (send console set-title-prefix
		   (string-append (file-name-from-path project-filename)
				  ": ")))
	   (send console show #t))]
	
	[slatex-buffer
	 (lambda (buffer)
	   (send console slatex-buffer buffer group))]
	
	[load-buffer-into-scheme
	 (lambda (buffer)
	   (insure-console-ready)
	   (send console load-buffer-into-scheme buffer group))]
	
	[slatex-file
	 (lambda (filename)
	   (if (string? filename)
	       (begin
		 (insure-console-ready)
		 (let* ([len (string-length filename)]
			[ext (substring filename (- len 3) len)])
		   (if (string=? ext ".ss")
		       (let ([buffer (send (ivar group buffers)
					   find-buffer-by-name
					   'file
					   filename)])
			 (if buffer
			     (slatex-buffer buffer)
			     (send console slatex-file
				   filename group))))))
	       (slatex-buffer filename)))]
	
	[the-debugger-buffer
	 (lambda (buffer)
	   (send console the-debugger-buffer buffer group))]
	
	[the-debugger-file
	 (lambda (filename)
	   (if (string? filename)
	       (begin
		 (insure-console-ready)
		 (let* ([len (string-length filename)]
			[ext (substring filename (- len 3) len)])
		   (if (string=? ext ".ss")
		       (let ([buffer (send (ivar group buffers)
					   find-buffer-by-name
					   'file
					   filename)])
			 (if buffer
			     (the-debugger-buffer buffer)
			     (send console the-debugger-file
				   filename group))))))
	       (the-debugger-buffer filename)))]
	
	[load-file-into-scheme
	 (lambda (filename)
	   (if (string? filename)
	       (begin
		 (insure-console-ready)
		 (let* ([len (string-length filename)]
			[ext (substring filename (- len 3) len)])
		   (if (string=? ext ".ss")
		       (let ([buffer (send (ivar group buffers)
					   find-buffer-by-name
					   'file
					   filename)])
			 (if buffer
			     (load-buffer-into-scheme buffer)
			     (send console load-file-into-scheme 
				   filename group))))))
	       (load-buffer-into-scheme filename)))]
	
	[do-locking-thread
	 (lambda (thunk)
	   (thread
	    (lambda ()
	      (let ([old-cursor (set-cursor 
				 (make-object wx:cursor%
					      wx:const-cursor-watch))])
		(dynamic-wind
		 (lambda ()
		   (send (get-menu-bar) enable-all #f)
		   (send execute-button enable #f)
		   (send project-item-list enable #f))
		 thunk
		 (lambda ()
		   (send (get-menu-bar) enable-all #t)
		   (send execute-button enable #t)
		   (send project-item-list enable #t)
		   (set-cursor (if (null? old-cursor)
				   (make-object wx:cursor
						wx:const-cursor-arrow)
				   old-cursor))))))))]
	[load-selected-into-scheme
	 (lambda ()
	   (do-locking-thread
	    (lambda ()
	      (catch escape
		(for-each
		 (lambda (filename)
		   (if (not (load-file-into-scheme filename))
		       (escape #f)))
		 (get-selected-files))))))]
	
	[load-project-into-scheme
	 (lambda ()
	   (do-locking-thread
	    (lambda ()
	      (catch escape
		(for-each-file
		 (lambda (filename)
		   (if (not (load-file-into-scheme filename))
		       (escape #f))))))))]
	
	[the-debugger-one-file
	 (lambda (file)
	   (do-locking-thread
	    (lambda ()
	      (the-debugger-file file))))]
	
	[slatex-one-file
	 (lambda (file)
	   (do-locking-thread
	    (lambda ()
	      (slatex-file file))))]
	
	[load-one-file-into-scheme
	 (lambda (file)
	   (do-locking-thread
	    (lambda ()
	      (load-file-into-scheme file))))]
	
	[compile-project
	 (lambda ()
	   (wx:message-box "Project compilation is not ready yet."
			   "Sorry"))])
      
      (sequence
	(super-init filename visible?))
      
      (public
	[button-panel (make-object mred:horizontal-panel% panel)]
	[break-button (make-object mred:button% button-panel
				   (lambda args (on-break)) "Break")]
	[execute-button (make-object mred:button% button-panel
				     (lambda args 
				       (load-project-into-scheme))
				     "Execute Project")])
      (sequence
	(send panel delete-child button-panel)
	(send button-panel show #t)
	(send panel change-children (lambda (l) (cons button-panel l))) 
	(send button-panel stretchable-in-y? #f)))))

(define drscheme:scheme-project-frame%
  (drscheme:make-scheme-project-frame% mred:project-frame%))

(define drscheme:scheme-project-editor-frame%
  (class mred:editor-frame% args
    (inherit panel active-canvas active-edit)

    (sequence
      (apply super-init args))

    (public
      [project (void)]
      [set-project
       (lambda (p)
	 (set! project p))])))

(mred:insert-format-handler "Scheme Project" "spj"
			    (lambda (filename group-ignored)
			      (make-object drscheme:scheme-project-frame% 
					   filename)))
