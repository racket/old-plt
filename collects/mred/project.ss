  (unit/sig mred:project^
    (import mred:wx^
	    [mred:constants : mred:constants^]
	    [mred:group : mred:group^]
	    [mred:container : mred:container^]
	    [mred:gui-utils : mred:gui-utils^]
	    [mred:exit : mred:exit^]
	    [mred:finder : mred:finder^]
	    [mred:frame : mred:frame^]
	    [mred:handler : mred:handler^]
	    [mzlib:file : mzlib:file^]
	    [mzlib:function : mzlib:function^])
	    
    (mred:debug:printf 'invoke "mred:project@")

    (define project-frame-group%
      (class mred:group:frame-group% (p . args)
	     (public
	      [project p])
	     (sequence
	       (apply super-init args))))
    
    (define make-project-frame%
      (lambda (super%)
	(class super% ([filename #f] [visible? #t])
	  (inherit file-menu file-menu:open-id show
		   get-client-size set-title make-menu
		   panel)
	  (rename [super-get-panel% get-panel%]
		  [super-on-size on-size]
		  [super-on-close on-close])
	  (public
	    [WIDTH 200]
	    [HEIGHT 300]
	    [TOP-MARGIN 0])
	  (private 
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
	    [get-panel%
	     (lambda ()
	       (class-asi (super-get-panel%)
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
		   (let ([act (mred:gui-utils:unsaved-warning 
			       project-filename
			       reason #t)])
		     (case act
		       [(cancel) #f]
		       [(continue) (set! project-ok-unsaved? #t) #t]
		       [(save) (save-project #f) #t]))
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
		     (mred:exit:remove-exit-callback exit-callback-tag)
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
	       (let ([name (mzlib:file:find-relative-path project-dir name)])
		 (send project-item-list append name)
		 (set-modified #t)
		 name))]
	    [remove-file
	     (lambda (name)
	       (let ([name (mzlib:file:find-relative-path project-dir name)])
		 (send project-item-list delete
		       (send project-item-list find-string name))))]
	    [open-file
	     (lambda (name)
	       (let ([name (mzlib:file:normalize-path name project-dir)])
		 (send group open-file name)))]
	    [get-selected-files
	     (lambda ()
	       (let* ([sels (box ())]
		      [count (send project-item-list get-selections sels)])
		 (let loop ([sels (unbox sels)])
		   (if (null? sels)
		       ()
		       (cons (mzlib:file:normalize-path (send project-item-list get-string 
							      (car sels))
							project-dir)
			     (loop (cdr sels)))))))]
	    [open-selection
	     (lambda ()
	       (map open-file (get-selected-files)))]
	    [remove-selected
	     (lambda ()
	       (let ([selections (box null)])
		 (send project-item-list get-selections selections)
		 (set! selections (mzlib:function:quicksort (unbox selections) >=))
		 (for-each (lambda (n) (send project-item-list delete n))
			   selections)
		 (unless (null? selections)
		   (set-modified #t))))]
	    [save-project
	     (lambda (as?)
	       (let* ([new? (or as? (not project-filename))]
		      [file (if new?
				(mred:finder:put-file 
				 () () #f "Save Project As"
				 (string-append ".*\\." project-extension)
				 (format "Project names must end with \".~a\"."
					 project-extension))
				project-filename)])
		 (when file
		   (if new?
		       (let* ([new-dir (mzlib:file:path-only file)]
			      [file-count (send project-item-list number)]
			      [names
			       (let loop ([n 0])
				 (if (< n file-count)
				     (cons (let* ([s (send project-item-list 
							   get-string n)]
						  [s (mzlib:file:normalize-path 
						      s project-dir)])
					     (mzlib:file:find-relative-path 
					      new-dir s))
					   (loop (add1 n)))
				     ()))])
			 (send project-item-list clear)
			 (send project-item-list set names)))
		   (set! project-filename file)
		   (set! project-dir (mzlib:file:path-only file))
		   (with-output-to-file file
		     (lambda ()
		       (write (make-options-assoc-list))
		       (newline)
		       (let ([file-count (send project-item-list number)])
			 (let loop ([n 0])
			   (if (< n file-count)
			       (let ([s (send project-item-list get-string n)])
				 (write s)
				 (newline)
				 (loop (add1 n))))))
		       (set-modified #f))
		     'truncate)
		   (let ([name (or (mzlib:file:file-name-from-path file) "Project")])
		     (send group set-frame-title-prefix name)
		     (set-title name)))))]
	    [for-each-file
	     (lambda (f)
	       (let ([file-count (send project-item-list number)])
		 (let loop ([n 0])
		   (if (< n file-count)
		       (let ([s (send project-item-list get-string n)])
			 (f (mzlib:file:normalize-path s project-dir))
			 (loop (add1 n)))))))]
	    [map-file
	     (lambda (f)
	       (let ([file-count (send project-item-list number)])
	       (let loop ([n 0])
		 (if (< n file-count)
		     (let ([s (send project-item-list get-string n)])
		       (cons
			(f (mzlib:file:normalize-path s project-dir))
			(loop (add1 n))))
		     '()))))]
	    [file-in-project?
	     (lambda (name)
	       (ormap mzlib:function:identity (map-file (lambda (n) (string=? n name)))))]
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
		     (send group clear)
		     (when filename
		       (set! project-filename (mzlib:file:normalize-path filename))
		       (set! project-dir (mzlib:file:path-only project-filename)))
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
				     (send project-item-list append item)
				     (loop))))))))
		     (set! project-modified? #f)
		     (let ([name (or (and filename (mzlib:file:file-name-from-path filename))
				     "Untitled Project")])
		       (send group set-frame-title-prefix name)
		       (set-title name)))
		   #f))]
	    [make-options-assoc-list
	     (lambda ()
	       '())]
	    [file-menu:new-string "File In Project"]
	    [file-menu:new
	     (lambda ()
	       (let ([name (mred:finder:put-file)])
		 (when name
		   (add-file name)
		   (open-file name))))]
	    [file-menu:open-string "Selected"]
	    [file-menu:open (lambda () (open-selection))]
	    [file-menu:save-string "Project"]
	    [file-menu:save (lambda () (save-project #t))]
	    [file-menu:save-as (lambda () (save-project #t))]
	    [file-menu:close-string "Project"]
	    [file-menu:close 
	     (lambda ()
	       (if (on-close)
		   (show #f)))]
	    [file-menu:print #f]
	    [file-menu:remove-selected-id #f]
	    [file-menu:between-save-and-print
	     (lambda (file-menu)
	       (send file-menu append-separator)
	       (send file-menu append-item "Add Files..."
		     (lambda ()
		       (let ([l (mred:finder:common-get-file-list)])
			 (if l
			     (map add-file l)))))
	       (set! file-menu:remove-selected-id 
		     (send file-menu append-item "Remove Selected" remove-selected)))]
	    [do-project-item-list-event
	     (lambda (item event)
	       (let ([items-selected (send project-item-list get-selections (box null))])
		 (for-each (if (zero? items-selected)
			       (lambda (x) (send file-menu enable x #f))
			       (lambda (x) (send file-menu enable x #t)))
			   (list file-menu:remove-selected-id
				 file-menu:open-id))))])
	  (public
	    [project-filename (if filename
				  (mzlib:file:normalize-path filename)
				  "Untitled Project")])
	  (private
	    [project-dir (if filename
			     (mzlib:file:path-only filename)
			     (current-directory))])
	  
	  (sequence
	    (let ([name (or (mzlib:file:file-name-from-path project-filename)
			    "Untitled Project")])
	      (send group set-frame-title-prefix name)
	      (super-init () name
			  -1 -1 WIDTH HEIGHT
			  (+ wx:const-default-frame wx:const-sdi)
			  name)
	      (send* file-menu 
		     (enable file-menu:remove-selected-id #f)
		     (enable file-menu:open-id #f))))
	  
	  (public
	    [list-box% (class-asi mred:container:list-box%
			 (rename [super-deselect deselect])
			 (public
			   [deselect (lambda (n)
				       (super-deselect n))]))]
	    [project-item-list (make-object list-box%
					    panel
					    do-project-item-list-event
					    () wx:const-multiple)])
	  
	  (sequence
	    (if visible?
		(show #t)))
	  
	  (private
	    [exit-callback-tag
	     (mred:exit:insert-exit-callback check-project-saved-for-quit)])
	  
	  (sequence
	    (read-project filename)))))
    
    (define project-frame% (make-project-frame% mred:frame:standard-menus-frame%))
    
    (mred:handler:insert-format-handler "Project" "mpj"
					 (lambda (filename group-ignored)
					   (make-object project-frame% 
					     filename))))
