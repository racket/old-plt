(unit/sig ()
  (import mred^
	  mzlib:core^
	  framework^
	  mzlib:print-convert^
	  (drscheme : drscheme:export^)
	  (zodiac : zodiac:system^))

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
		 (cond
		  [(string? file) (format "raw: ~s" file)]
		  [(pair? file) (format "coll: ~s" file)]
		  [else (format "huh?: ~s" file)])))
	 files))

      (define (nth-cdr n l)
	(cond
	 [(zero? n) l]
	 [(null? l) (error 'nth-cdr "got to end of list with ~s cdrs left" n)]
	 [else (nth-cdr (- n 1) (cdr l))]))

      (define (add-files)

	(define (prompt-user-collection? filename collection collection-dir)
	  (define answer #t)
	  (define dialog (make-object dialog% "Use a collection-based-path?" #f 400 400))
	  (define text (make-object text%))
	  (define ec (make-object editor-canvas% dialog text))

	  (define bp (make-object horizontal-panel% dialog))
	  (define no-all-button (make-object button% "No to all" bp
					      (lambda xx
						(set! answer #f)
						(set! prompt-user-collection?
						      (lambda xxx #f))
						(send dialog show #f))))
	  (define no-button (make-object button% "No" bp
					  (lambda xx (set! answer #f) (send dialog show #f))))
	  (define yes-button (make-object button% "Yes" bp
					  (lambda xx (set! answer #t) (send dialog show #f))))
	  (define yes-all-button (make-object button% "Yes to all" bp
					      (lambda xx
						(set! answer #t)
						(set! prompt-user-collection?
						      (lambda xxx #t))
						(send dialog show #f))
					      '(border)))

	  (send yes-all-button focus)
	  (send bp stretchable-height #f)
	  (send bp set-alignment 'center 'center)
	  (send text insert
		(format "~
~nThe file:~
~n~
~n   ~a~
~n~
~nis in the collection:~
~n~
~n   ~s~
~n~
~nin the collections directory:~
~n~
~n   ~a~
~n~
~nShould this file be collection relative?"
			filename
			collection
			collection-dir))

	  (send text change-style (make-object style-delta% 'change-family 'modern)
		0
		(send text last-position))
	  (send text lock #t)
	  (send text hide-caret #t)

	  (send dialog show #t)
	  answer)

	(define (sublist-equal? shorter longer)
	  (cond
	   [(null? shorter) #t]
	   [(null? longer) (error 'sublist-equal? "second argument was shorter")]
	   [(equal? (car shorter) (car longer)) (sublist-equal? (cdr shorter) (cdr longer))]
	   [else #f]))


	(let ([new-files (finder:common-get-file-list)])
	  (when new-files
	    (let ([collections (all-collections)]
		  [exploded-collection-paths (map file:explode-path (current-library-collection-paths))])
	      (for-each
	       (lambda (new-file)
		 (let ([exploded (file:explode-path new-file)])
		   (let loop ([exploded-collection-paths exploded-collection-paths ])
		     (cond
		      [(null? exploded-collection-paths)
		       (set! files (append files (list new-file)))]
		      [else
		       (if (sublist-equal? (car exploded-collection-paths) exploded)
			   (let* ([filename #f]
				  [collections
				   (let loop ([pieces
					       (nth-cdr (length (car exploded-collection-paths))
							exploded)])
				     (cond
				      [(null? pieces) null]
				      [(null? (cdr pieces))
				       (set! filename (car pieces))
				       null]
				      [else (cons (car pieces)
						  (loop (cdr pieces)))]))])
			     (if (and (string=?
				       (file:normalize-path
					(build-path (apply collection-path collections)
						    filename))
				       (file:normalize-path
					new-file))
				      (prompt-user-collection?
				       filename
				       collections
				       (apply build-path
					      (car exploded-collection-paths))))
				 (set! files (append files (list (cons filename collections))))
				 (set! files (append files (list new-file)))))
			   (loop (cdr exploded-collection-paths)))]))))
	       new-files))
	    (refresh-files-list-box))))
			    
      
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
      (make-object menu-item% "Add Files..." project-menu (lambda x (add-files)))

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

  (new-project)

  (drscheme:get/extend:extend-unit-frame make-project-aware-unit-frame))