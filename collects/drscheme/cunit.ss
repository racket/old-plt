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
	 (inherit set-modified for-each-file
		  enable set-cursor group
		  make-menu
		  get-selected-files
		  get-client-size
		  get-menu-bar
		  project-item-list
		  project-filename
		  TOP-MARGIN
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

	       (send mb append project-menu "Project")
	       
	       mb))]
	  [on-close
	   (lambda ()
	     (if (super-on-close)
		 (begin
		   (if (and console (ivar console running?))
		       (send console shut-down))
		   #t)
		 #f))]
	  [on-size
	   (lambda (w h)
	     (let ([wbox (box 0)]
		   [hbox (box 0)])
	       (get-client-size wbox hbox)
	       (when (object? button-panel)
		     (send button-panel set-size
			   0 0 (unbox wbox) -1)))
	     (super-on-size w h))]
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
	  [button-panel (make-object wx:panel% this
				     0 0 WIDTH 100)]
	  [break-button (make-object wx:button% button-panel
				     (lambda args (on-break)) "Break")]
	  [execute-button (make-object wx:button% button-panel
				       (lambda args 
					 (load-project-into-scheme))
				       "Execute Project")])

	 (sequence
	   (send button-panel fit)

	   (set! TOP-MARGIN (+ TOP-MARGIN (send button-panel get-height)))

	   (on-size 0 0)))))

(define drscheme:scheme-project-frame%
  (drscheme:make-scheme-project-frame% mred:project-frame%))

(define drscheme:scheme-project-editor-canvas%
  (class-asi mred:simple-frame-canvas%
    (inherit get-parent get-media)
    (rename [super-make-panel make-panel])
    (public
     [use-panel? #t]
     [make-panel
      (lambda (frame x y)
	(let ([panel (super-make-panel frame x y)])
	  (if drscheme:allow-execute?
	      (make-object wx:button% panel
			   (lambda args
			     (send (get-parent) load-frame-buffer-into-scheme
				   (get-media)))
			   "Execute"))
	  (if drscheme:allow-mrslatex?
	      (make-object wx:button% panel
			   (lambda args
			     (send (get-parent) slatex-frame-buffer
				   (get-media)))
			   "Color"))
	  panel))])))

(define drscheme:scheme-project-editor-frame%
  (class-asi mred:frame%
    (public
     [canvas% drscheme:scheme-project-editor-canvas%]
     project
     [set-project
      (lambda (p)
	(set! project p))]
     [slatex-frame-buffer
       (lambda (buffer)
	 (send project slatex-one-file buffer))]
     [load-frame-buffer-into-scheme
      (lambda (buffer)
	(send project load-one-file-into-scheme buffer))])))

(mred:insert-format-handler "Scheme Project" "spj"
			    (lambda (filename group-ignored)
			      (make-object drscheme:scheme-project-frame% 
					   filename)))
