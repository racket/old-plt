(unit/sig ()
  (import mred^
	  mzlib:core^
	  framework^
	  mzlib:print-convert^
	  (drscheme : drscheme:export^)
	  zodiac:system^)

  (define (all-collections)
    (let ([colls (make-hash-table)])
      (for-each
       (lambda (collection-path-dir)
	 (for-each
	  (lambda (collection)
	    (when (and (directory-exists? (build-path collection-path-dir collection))
		       (not (string=? collection "CVS")))
	      (hash-table-put! colls (string->symbol collection) #t)))
	  (directory-list collection-path-dir)))
       (current-library-collection-paths))
      (function:quicksort (hash-table-map colls (lambda (x v) (symbol->string x))) string<=?)))

  (define (make-project-aware-unit-frame super%)
    (class/d super% args

      ((inherit get-menu-bar))

      (apply super-init args)
      (define mb (get-menu-bar))
      (define project-menu (make-object menu% "Project" mb))
      (make-object menu-item% "New Project" project-menu (lambda x (new-project)))
      (define collection-projects (make-object menu% "Collection Projects" project-menu))
      (for-each
       (lambda (collection)
	 (let ([info (and (file-exists? (build-path (collection-path collection) "info.ss"))
			  (require-library/proc "info.ss" collection))])
	   (when info
	     (let* ([raw-project-file (info 'project-filename (lambda () #f))]
		    [project-file (and raw-project-file
				       (if (relative-path? raw-project-file)
					   (build-path (collection-path collection) raw-project-file)
					   raw-project-file))])
	       (when project-file
		 (make-object menu-item%
		   collection
		   collection-projects
		   (lambda xxx
		     (open-project project-file))))))))
       (all-collections))

      (frame:reorder-menus this)))


  (define project-frame%
    (class/d frame:standard-menus% (filename)

      ((inherit get-area-container get-menu-bar))

      (define files null)

      (define (refresh-files-list-box)
	(send files-list-box clear)
	(for-each
	 (lambda (file)
	   (send files-list-box append 
		 (if (string? file)
		     (format "raw: ~s" file)
		     (format "coll: ~s" file))))
	 files))

      (define (add-files)
	(finder:common-get-file-list))
      
      (define (load-file filename)
	(with-handlers ([(lambda (x) #t)
			 (lambda (x)
			   (message-box "Error loading project file"
					(format "~a" (if (exn? x)
							 (exn-message x)
							 (format "~s" x)))))])
	  (let* ([all (call-with-input-file filename read)]
		 [files (function:first all)])
	    (set! files files))

	  (refresh-files-list-box)))

      (super-init (if (string? filename) filename "Project") #f 400)

      (define mb (get-menu-bar))
      (define project-menu (make-object menu% "Project" mb))
      (make-object menu-item% "New Project" project-menu (lambda x (new-project)))
      (make-object menu-item% "Add Files" project-menu (lambda x (add-files)))

      (make-object button% "Execute" (get-area-container) void)
      (define top-panel (make-object horizontal-panel% (get-area-container)))
      (define files-list-box (make-object list-box% #f null top-panel void '(single)))
      (define button-panel (make-object vertical-panel% top-panel))
      (make-object button% "Up" button-panel void)
      (make-object button% "Open" button-panel void)
      (make-object button% "Down" button-panel void)
      (send button-panel stretchable-width #f)

      (when (and filename
		 (file-exists? filename))
	(load-file filename))))

  (define (open-project filename)
    (send (make-object project-frame% filename) show #t))

  (define (new-project)
    (send (make-object project-frame% #f) show #t))

  (drscheme:get/extend:extend-unit-frame make-project-aware-unit-frame))