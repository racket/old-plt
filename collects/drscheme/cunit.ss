;; need to fix: set project type

(define drscheme:project@
  (unit/sig drscheme:project^
    (import [mred : mred^]
	    [mzlib : mzlib:core^]
	    [drscheme:aries : drscheme:aries^]
	    [drscheme:edit : drscheme:edit^]
	    [drscheme:spawn : drscheme:spawn^])

    (mred:debug:printf 'invoke "drscheme:project@")

    (define scheme-project-frame-group%
      (class mred:project-frame-group% args
	(inherit project)
	(public
	 [frame%
	  (class drscheme:aries:frame% args
	    (inherit set-project)
	    (sequence
	      (mred:debug:printf 'super-init "before.1")
	      (apply super-init args)
	      (mred:debug:printf 'super-init "after.1")
	      (set-project project)))])
	(sequence
	  (mred:debug:printf 'super-init "before.9")
	  (apply super-init args)
	  (mred:debug:printf 'super-init "after.9"))))

    (mred:set-preference-default 'drscheme:project-visible? #f)

    (define make-scheme-project-frame%
      (lambda (super%)
	(class mred:project-frame% ([filename #f]
				    [weak-menus? #f])
	  (inherit panel show
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
	    [super-remove-file remove-file]
	    [super-parse-options parse-options]
	    [super-make-options-assoc-list make-options-assoc-list]
	    [super-make-menu-bar make-menu-bar]
	    [super-on-size on-size]
	    [super-set-title set-title])
	  (private
	    [scheme-type 'std]
	    [use-aries? #t])
	  (public
	    [quit (lambda () (send console-edit shut-down))]
	    [visible? (mred:get-preference 'drscheme:project-visible?)]
	    [project-extension "spj"]
	    [group% scheme-project-frame-group%]
	    
	    [console-edit (make-object drscheme:aries:console-edit%)])
	  
	  (public
	    
	    [file-menu:close (if mred:debug:on? (lambda () (show #f)) #f)]
	    
	    [remove-file
	     (lambda (name)
	       (super-remove-file name)
	       (when (and (zero? (send project-item-list number))
			  (not visible?))
		 (quit)))]
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
	       '(let ([dial (make-object wx:dialog-box% () "Project Type" #t)]
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
		 
		 (send mb append project-menu "P&roject")
		 
		 '(unless weak-menus?
		    (send project-menu append-item "Set Project Type..."
			  set-project-type)
		    (send project-menu append-separator))
		 (send project-menu append-item "&Execute Project"
		       load-project-into-scheme)
		 (send project-menu append-item "Execute &Selected Files"
		       load-selected-into-scheme)
		 
		 (unless weak-menus?
		   (send project-menu append-separator)
		   (send project-menu append-item "&Compile Project..."
			 compile-project))
		 
		 mb))]
	    [on-break
	     (lambda args
	       (send console-edit break))]
	    [load-buffer-into-scheme
	     (lambda (buffer)
	       (send console load-buffer-into-scheme buffer group))]
	    
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
				       (make-object wx:cursor%
						    wx:const-cursor-arrow)
				       old-cursor))))))))]
	    [load-selected-into-scheme
	     (lambda ()
	       (do-locking-thread
		(lambda ()
		  (catch escape
		    (for-each
		     (lambda (filename)
		       (if (not (send console-edit load-file-into-scheme filename))
			   (escape #f)))
		     (get-selected-files))))))]
	    
	    [load-project-into-scheme
	     (lambda ()
	       (do-locking-thread
		(lambda ()
		  (catch escape
		    (for-each-file
		     (lambda (filename)
		       (if (not (send console-edit load-file-into-scheme filename))
			   (escape #f))))))))]
	    
	    [load-one-file-into-scheme
	     (lambda (file)
	       (do-locking-thread
		(lambda ()
		  (send console-edit load-file-into-scheme file))))]
	    
	    [compile-project
	     (lambda ()
	       (wx:message-box "Project compilation is not ready yet."
			       "Sorry"))])
	  
	  (sequence
	    (mred:debug:printf 'super-init "before.10")
	    (super-init filename #f)
	    (mred:debug:printf 'super-init "after.10"))
	  
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
	    (send button-panel stretchable-in-y? #f)
	    
	    (mred:debug:printf 'super-init "before visibility check")
	    (if visible?
		(show #t)
		(make-object drscheme:aries:frame% #f #t group))
	    (mred:debug:printf 'super-init "after visibility check")
	    
	    (mred:show-busy-cursor
	     (lambda ()
	       (mred:debug:printf 'super-init "initializing console")
	       (send console-edit do-initialize)
	       (mred:debug:printf 'super-init "initialized console")
	       (for-each (lambda (x) (send console-edit load-file-into-scheme x))
			 (mred:get-preference 'drscheme:startup-files))))))))

    (define scheme-project-frame%
      (make-scheme-project-frame% mred:project-frame%))

    (mred:debug:printf 'super-init "before console")
    (define console (make-object scheme-project-frame%))
    (mred:debug:printf 'super-init "after console")
    (define eval-string (ivar (ivar console console-edit) do-eval))

    (mred:insert-format-handler "Scheme Project" "spj"
				(lambda (filename group-ignored)
				  (make-object scheme-project-frame% filename)))))