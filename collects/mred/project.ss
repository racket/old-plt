(define-sigfunctor (mred:project@ mred:project^)
  (import mred:group^ mred:gui-utils^ mred:exit^ mred:finder^
	  mred:frame^ mred:handler^
	  mzlib:file^ mzlib:function^)
  
  (define project-frame-group%
    (class mred:group^:frame-group% (p . args)
      (public
	[project p])
      (sequence
	(apply super-init args))))
  
  (define make-project-frame%
    (lambda (super%)
      (class super% ([filename #f][visible? #t])
	(inherit show get-client-size set-title make-menu)
	(rename [super-on-size on-size]
		[super-on-close on-close]
		[super-make-menu-bar make-menu-bar])
	(public
	  [WIDTH 200]
	  [HEIGHT 300]
	  [TOP-MARGIN 0])
	(private 
	  [file-count 0]
	  [project-modified? #f]
	  [project-ok-unsaved? #f])
	(public
	  [project-extension "mpj"]
	  [group% project-frame-group%]
	  [get-group%
	   (lambda ()
	     (class-asi group%
	       (public
		 [ask-before-closing-last? #f])))]
	  [panel% wx:panel%]
	  [get-panel%
	   (lambda ()
	     (class-asi panel%
	       (public
		 [on-default-action
		  (lambda (which)
		    (if (eq? which project-item-list)
			(open-selection)))])))])
	(public
	  [group (make-object (get-group%) this)]
	  [check-project-saved-for-quit
	   (lambda ()
	     (check-project-saved "Quit"))]
	  [check-project-saved
	   (opt-lambda ([reason "Close Project"])
	     (if (and project-modified? (not project-ok-unsaved?))
		 (let ([act (mred:gui-utils^:unsaved-warning project-filename
							     reason #t)])
		   (case act
		     [cancel #f]
		     [continue (set! project-ok-unsaved? #t) #t]
		     [save (save-project #f) #t]))
		 #t))]
	  [check-project-all-saved
	   (opt-lambda ([reason "Close Project"])
	     (and (check-project-saved reason)
		  (send (ivar group buffers) check-buffers reason)))]
	  [on-close
	   (lambda ()
	     (if (and (check-project-all-saved)
		      (super-on-close)
		      (send group close-all))
		 (begin
		   (mred:exit^:remove-exit-callback exit-callback-tag)
		   #t)
		 #f))]
	  [set-modified
	   (lambda (mod?)
	     (set! project-modified? mod?)
	     (set! project-ok-unsaved? #f))]
	  [modified?
	   (lambda ()
	     project-modified?)]
	  [add-file 
	   (lambda (name)
	     (let ([name (mzlib:file^:find-relative-path project-dir name)])
	       (set! file-count (add1 file-count))
	       (send project-item-list append name)
	       (set-modified #t)
	       name))]
	  [open-file
	   (lambda (name)
	     (let ([name (mzlib:file^:normalize-path name project-dir)])
	       (mred:gui-utils^:show-busy-cursor
		(lambda ()
		  (send group open-file name)))))]
	  [get-selected-files
	   (lambda ()
	     (let* ([sels (box ())]
		    [count (send project-item-list get-selections sels)])
	       (let loop ([sels (unbox sels)])
		 (if (null? sels)
		     ()
		     (cons (mzlib:file^:normalize-path (send project-item-list get-string 
							     (car sels))
						       project-dir)
			   (loop (cdr sels)))))))]
	  [open-selection
	   (lambda ()
	     (map open-file (get-selected-files)))]
	  [remove-selected
	   (lambda ()
	     (let loop ([n 0])
	       (if (< n file-count)
		   (if (send project-item-list selected? n)
		       (begin
			 (send project-item-list delete n)
			 (set! file-count (sub1 file-count))
			 (set-modified #t)
			 (loop n))
		       (loop (add1 n))))))]
	  [save-project
	   (lambda (as?)
	     (let* ([new? (or as? (not project-filename))]
		    [file (if new?
			      (mred:finder^:put-file 
			       () () #f "Save Project As"
			       (string-append ".*\\." project-extension)
			       (format "Project names must end with \".~a\"."
				       project-extension))
			      project-filename)])
	       (when file
		 (if new?
		     (let* ([new-dir (mzlib:file^:path-only file)]
			    [names
			     (let loop ([n 0])
			       (if (< n file-count)
				   (cons (let* ([s (send project-item-list 
							 get-string n)]
						[s (mzlib:file^:normalize-path 
						    s project-dir)])
					   (mzlib:file^:find-relative-path 
					    new-dir s))
					 (loop (add1 n)))
				   ()))])
		       (send project-item-list clear)
		       (send project-item-list set names)))
		 (set! project-filename file)
		 (set! project-dir (mzlib:file^:path-only file))
		 (with-output-to-file file
		   (lambda ()
		     (write (make-options-assoc-list))
		     (newline)
		     (let loop ([n 0])
		       (if (< n file-count)
			   (let ([s (send project-item-list get-string n)])
			     (write s)
			     (newline)
			     (loop (add1 n)))))
		     (set-modified #f))
		   'truncate)
		 (let ([name (or (mzlib:file^:file-name-from-path file) "Project")])
		   (send group set-frame-title-prefix name)
		   (set-title name)))))]
	  [for-each-file
	   (lambda (f)
	     (let loop ([n 0])
	       (if (< n file-count)
		   (let ([s (send project-item-list get-string n)])
		     (f (mzlib:file^:normalize-path s project-dir))
		     (loop (add1 n))))))]
	  [map-file
	   (lambda (f)
	     (let loop ([n 0])
	       (if (< n file-count)
		   (let ([s (send project-item-list get-string n)])
		     (cons
		      (f (mzlib:file^:normalize-path s project-dir))
		      (loop (add1 n))))
		   '())))]
	  [file-in-project?
	   (lambda (name)
	     (ormap mzlib:function^:identity (map-file (lambda (n) (string=? n name)))))]
	  [parse-options
	   (lambda (options error-escape)
	     (unless (list? options)
	       (wx:message-box
		"Error reading Scheme project options."
		"Error")
	       (error-escape #f)))]
	  [read-project
	   (opt-lambda (filename [reason "Read Project"])
	     (if (check-project-all-saved reason)
		 (begin
		   (send group close-all)
		   (send project-item-list clear)
		   (set! file-count 0)
		   (send group clear)
		   (when filename
		     (set! project-filename (mzlib:file^:normalize-path filename))
		     (set! project-dir (mzlib:file^:path-only project-filename)))
		   (if (and filename (file-exists? filename))
		       (catch read-error
			 (with-input-from-file filename
			   (lambda ()
			     (parse-options (read) read-error)
			     (let loop ()
			       (let ([item (read)])
				 (unless (eof-object? item)
				   (when (not (string? item))
				     (wx:message-box
				      "Error reading Scheme project file items."
				      "Error")
				     (read-error #f))
				   (set! file-count (add1 file-count))
				   (send project-item-list append item)
				   (loop))))))))
		   (set! project-modified? #f)
		   (let ([name (or (and filename (mzlib:file^:file-name-from-path filename))
				   "Untitled Project")])
		     (send group set-frame-title-prefix name)
		     (set-title name)))
		 #f))]
	  [make-options-assoc-list
	   (lambda ()
	     '())]
	  [make-menu-bar
	   (lambda ()
	     (let ([mb (super-make-menu-bar)]
		   [file-menu (make-menu)])
	       (send file-menu append-item "Save Project"
		     (lambda () (save-project #f)))
	       (send file-menu append-item "Save Project As..."
		     (lambda () (save-project #t)))
	       (send file-menu append-separator)
	       (send file-menu append-item "Open Selected"
		     open-selection)
	       (send file-menu append-separator)
	       (send file-menu append-item "Add Files..."
		     (lambda ()
		       (let ([l (mred:finder^:common-get-file-list)])
			 (if l
			     (map add-file l)))))
	       (send file-menu append-item "Create and Add File..."
		     (lambda ()
		       (let ([name (mred:finder^:put-file)])
			 (when name
			   (add-file name)
			   (open-file name)))))
	       (send file-menu append-item "Remove Selected"
		     remove-selected)
	       (send file-menu append-separator)
	       (send file-menu append-item "Close Project"
		     (lambda ()
		       (if (on-close)
			   (show #f))))
	       
	       (send mb append file-menu "File")
	       mb))]
	  [on-size
	   (lambda (w h)
	     (super-on-size w h)
	     (let ([wbox (box 0)]
		   [hbox (box 0)])
	       (get-client-size wbox hbox)
	       (when (object? panel)
		 (send panel set-size 0 TOP-MARGIN
		       (unbox wbox) (- (unbox hbox) TOP-MARGIN)))
	       (when (object? panel)
		 (send project-item-list set-size 0 0 
		       (unbox wbox) (- (unbox hbox) TOP-MARGIN)))))]
	  
	  [do-project-item-list-event
	   (lambda args #f)])
	
	(public
	  [project-filename (if filename
				(mzlib:file^:normalize-path filename)
				filename)])
	(private
	  [project-dir (if filename
			   (mzlib:file^:path-only filename)
			   (current-directory))])
	
	(sequence
	  (let ([name (or (and project-filename
                               (mzlib:file^:file-name-from-path project-filename))
                          "Untitled Project")])
	    (send group set-frame-title-prefix name)
	    
	    (super-init () name
			-1 -1 WIDTH HEIGHT
			(+ wx:const-default-frame wx:const-sdi)
			name)))
	
	(public
	  [panel (let ([v (make-object (get-panel%) this
				       0 0 WIDTH HEIGHT)])
		   (send v set-label-position wx:const-vertical)
		   v)]
	  [project-item-list (make-object wx:list-box%
					  panel
					  do-project-item-list-event
					  ()
					  (if (eq? wx:window-system 
						   'motif)
					      wx:const-extended
					      wx:const-multiple)
					  0 0 WIDTH HEIGHT)])
	
	(sequence
	  (if visible?
	      (show #t)))
	
	(private
	  [exit-callback-tag
	   (mred:exit^:insert-exit-callback check-project-saved-for-quit)])
	
	(sequence
	  (read-project filename)))))
  
  (define project-frame% (make-project-frame% mred:frame^:menu-frame%))
  
  (mred:handler^:insert-format-handler "Project" "mpj"
				       (lambda (filename group-ignored)
					 (make-object project-frame% 
						      filename))))