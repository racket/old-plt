;;
;; TODO
;; 
;; - thread synchronization1 for save state
;; - remember shown-state of loaded window
;; - make sure that project menus are the same on unit frames and
;;   project frames (collection-project menu)

(require-library "errortrace.ss" "errortrace")

(unit/sig ()
  (import mred^
	  mzlib:core^
	  framework^
	  [drscheme : drscheme:export^]
	  [zodiac : zodiac:system^]
	  [hierlist : hierlist^])

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
      (make-object menu-item% "Open Project" project-menu (lambda x (open-project)))
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
      ((inherit get-area-container get-menu-bar)
       (override file-menu:save file-menu:save-as
		 can-close?))

      (define changed? #f)

      (define (can-close?)
	(if changed?
	    (case (gui-utils:unsaved-warning
		   filename
		   "close"
		   #t)
	      [(continue) #t]
	      [(save) (save-file filename) #t]
	      [(cancel) #f])
	    #t))


      (define (file-menu:save . xxx)
	(if filename
	    (save-file filename)
	    (file-menu:save-as)))

      (define (file-menu:save-as . xxx)
	(let ([new-fn (get-file "Choose a project filename" this)])
	  (when new-fn
	    (set! filename new-fn)
	    (save-file filename))))

      (define project-name
	(if filename
	    (let-values ([(base top __) (split-path filename)])
	      top)
	    "Untitled"))

      (define files null)
      (define language-settings (preferences:get 'drscheme:settings))

      (define project-custodian (make-custodian))
      (define drs-eventspace (current-eventspace))

      (define (show-error/open-file msg zodiac)
	(let* ([dialog (make-object dialog% (format "Error running project ~a" project-name) this 400 200)]
	       [text (make-object text%)]
	       [editor-canvas (make-object editor-canvas% dialog text)]
	       [filename (zodiac:location-file (zodiac:zodiac-start zodiac))]
	       [open-callback
		(lambda ()
		  (let ([fr (handler:edit-file filename)])
		    (when (is-a? fr drscheme:unit:frame%)
		      (let ([definitions (ivar fr definitions-text)]
			    [interactions (ivar fr interactions-text)])
			(send interactions highlight-error definitions
			      (zodiac:location-offset (zodiac:zodiac-start zodiac))
			      (+ 1 (zodiac:location-offset (zodiac:zodiac-finish zodiac))))))))]
	       [button-panel (make-object horizontal-panel% dialog)])
	  (send button-panel stretchable-height #f)
	  (send button-panel set-alignment 'center 'center)

	  (send (make-object button% (format "Open ~a" filename)
			     button-panel
			     (lambda x
			       (open-callback)
			       (send dialog show #f)))
		focus)
	  (make-object button% "Close Window" button-panel (lambda x (send dialog show #f)))

	  (send text insert msg)
	  (send text auto-wrap #t)
	  (send dialog show #t)))

      (define (execute-project)
	(let ([orig-eventspace (current-eventspace)]
	      [orig-exn-handler (current-exception-handler)])
	  (reset-hierlist)
	  (custodian-shutdown-all project-custodian)
	  (set! project-custodian (make-custodian))
	  (parameterize ([current-custodian project-custodian])
	    (parameterize ([current-eventspace (make-eventspace)])
	      (queue-callback
	       (lambda ()
		 (drscheme:basis:initialize-parameters project-custodian language-settings)

		 (exit-handler
		  (lambda x
		    (custodian-shutdown-all project-custodian)))

		 (drscheme:basis:error-display/debug-handler
		  (let ([project-manager-error-display/debug-handler
			 (lambda (msg zodiac exn)
			   (parameterize ([current-eventspace drs-eventspace])
			     (if (and zodiac
				      (zodiac:zodiac? zodiac))
				 (show-error/open-file msg zodiac)
				 (message-box (format "Error running project ~a" project-name) msg))))])
		    project-manager-error-display/debug-handler))

		 (let ([ol (current-load)])
		   (current-load
		    (lambda (l)
		      (dynamic-wind
		       (lambda () (push-file l))
		       (lambda () (ol l))
		       (lambda () (pop-file))))))
		 
		 (for-each
		  (lambda (file)
		    (cond
		     [(string? file) (load file)]
		     [else (apply require-library/proc file)]))
		  files)))))))

      (define (configure-language)
	(let ([new-settings (drscheme:language:language-dialog language-settings)])
	  (when new-settings
	    (set! language-settings new-settings)
	    (set! changed? #t))))

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

      (define (swap index)
	(set! changed? #t)
	(let loop ([n index]
		   [files files]
		   [previous-pair #f])
	  (cond
	   [(zero? n)
	    (let ([tmp (car previous-pair)])
	      (set-car! previous-pair (car files))
	      (set-car! files tmp))
	    (let ([tmp (send files-list-box get-string index)])
	      (send files-list-box set-string index (send files-list-box get-string (- index 1)))
	      (send files-list-box set-string (- index 1) tmp))]
	   [else
	    (loop (- n 1)
		  (cdr files)
		  files)])))

      (define (move-file-down)
	(let ([selection (car (send files-list-box get-selections))])
	  (swap (+ selection 1))
	  (send files-list-box select (+ selection 1))
	  (update-buttons)))

      (define (move-file-up)
	(let ([selection (car (send files-list-box get-selections))])
	  (swap selection)
	  (send files-list-box select (- selection 1))
	  (update-buttons)))

      (define (open-file)
	(let* ([file (list-ref files (car (send files-list-box get-selections)))]
	       [filename (cond
			  [(string? file) file]
			  [else (build-path (apply collection-path (cdr file))
					    (car file))])])
	  (handler:edit-file filename)))
			  
      (define (remove-file)
	(let* ([index (car (send files-list-box get-selections))])
	  (set! files
		(let loop ([n index]
			   [files files])
		  (cond
		   [(null? files) null]
		   [(zero? n) (cdr files)]
		   [else (cons (car files) (loop (- n 1) (cdr files)))])))
	  (send files-list-box delete index)
	  (let ([max (send files-list-box get-number)])
	    (cond
	     [(= 0 max) (void)]
	     [(<= 0 index (- max 1))
	      (send files-list-box select index)]
	     [else
	      (send files-list-box select (- max 1))]))
	  (update-buttons)
	  (set! changed? #t)))

      (define (update-buttons)
	(let ([selection-list (send files-list-box get-selections)])
	  (if (null? selection-list)
	      (begin
		(send down-button enable #f)
		(send up-button enable #f)
		(send remove-button enable #f)
		(send open-button enable #f))
	      (let ([selection (car selection-list)])
		(send open-button enable #t)
		(send remove-button enable #t)
		(cond
		 [(= 1 (send files-list-box get-number))
		  (send down-button enable #f)
		  (send up-button enable #f)]
		 [(= 0 selection)
		  (send down-button enable #t)
		  (send up-button enable #f)]
		 [(= selection (- (send files-list-box get-number) 1))
		  (send down-button enable #f)
		  (send up-button enable #t)]
		 [else
		  (send down-button enable #t)
		  (send up-button enable #t)])))))

      (define (files-list-box-callback selection)
	(case selection
	  [(list-box-dclick) (open-file)]
	  [(list-box)
	   (update-buttons)]))

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
	  (let* ([all (call-with-input-file filename read 'text)]
		 [loaded-files (assoc 'files all)]
		 [loaded-language-settings (assoc 'settings all)]
		 [loaded-open-table (assoc 'open-table all)])

	    (if (and loaded-language-settings
		     (= (arity drscheme:basis:make-setting)
			(length (function:second loaded-language-settings))))
		(set! language-settings
		      (apply drscheme:basis:make-setting (function:second loaded-language-settings)))
		(message-box "Loading Project"
			     (format "Ignoring out of date language settings (from previous version)~n~s"
				     loaded-language-settings)))

	    (when loaded-files
	      (set! files (function:second loaded-files)))

	    (when loaded-open-table
	      (set! open-table (make-hash-table))
	      (for-each (lambda (t) (hash-table-put! open-table (car t) (make-open-info #t (cadr t))))
			(function:second loaded-open-table)))

	    (refresh-files-list-box))))

      (define (save-file filename)
	(call-with-output-file filename
	  (lambda (port)
	    (write (list
		    `(open-table ,(hash-table-map open-table (lambda (x v) (list x (open-info-open? v)))))
		    `(files ,files)
		    `(settings ,(cdr (vector->list (struct->vector language-settings)))))
		   port))
	  'truncate 'text)
	(set! changed? #f))

      (define show/hide-loaded-files
	(let ([shown? #f])
	  (lambda (button event)
	    (send button set-label
		  (if shown?
		      "Show Loaded Files"
		      "Hide Loaded Files"))
	    (send loaded-files-outer-panel change-children
		  (lambda (l)
		    (if shown?
			null
			(list loaded-files-panel))))
	    (set! shown? (not shown?))
	    (send loaded-files-outer-panel stretchable-height shown?))))

      (define (hierlist-item-mixin class%)
	(class/d class% args
	  ((public set-dir get-dir set-file get-file))

	  (define dir #f)
	  (define file #f)

	  (define (get-dir) dir)
	  (define (get-file) file)

	  (define (set-dir nd)
	    (set! dir nd))
	  (define (set-file f)
	    (set! file f))

	  (apply super-init args)))

      (define ignore-open/closes? #t)

      (define hierlist%
	(class/d (hierlist-item-mixin hierlist:hierarchical-list%) args
	  ((override on-item-opened on-item-closed on-double-select on-select))

	  (define (on-item-opened i)
	    (unless ignore-open/closes?
	      (let ([info (hash-table-get open-table (string->symbol (send i get-file)))])
		(set-open-info-open?! info #t)
		(set! changed? #t))))

	  (define (on-item-closed i)
	    (unless ignore-open/closes? 
	      (let ([info (hash-table-get open-table (string->symbol (send i get-file)))])
		(set-open-info-open?! info #f)
		(set! changed? #t))))

	  (define (on-select i)
	    (send open-loaded-file-button enable i))

	  (define (on-double-select i)
	    (handler:edit-file (send i get-file)))

	  (apply super-init args)))

      (define (open-loaded-file)
	(let ([sel (send loaded-files-hierarchical-list get-selected)])
	  (when sel
	    (handler:edit-file (send sel get-file)))))

      ;; (union 'init-hl-stack (list-of (union string hier-list-item)))
      (define hl-stack 'init-hl-stack)

      (define (reset-hierlist)
	;; reset open/closed cache
	(for-each (lambda (l)
		    (let ([file-sym (car l)]
			  [info (cadr l)])
		      (if (open-info-touched? info)
			  (set-open-info-touched?! info #f)
			  (hash-table-remove! open-table file-sym))))
		    (hash-table-map open-table (lambda (x y) (list x y))))

	;; clear loaded window
	(for-each (lambda (i) (send loaded-files-hierarchical-list delete-item i))
		  (send loaded-files-hierarchical-list get-items))

	;; reset loaded stack
	(set! hl-stack (list loaded-files-hierarchical-list)))

      (define (create-list-item previous file make-new)
	(let ([new (make-new previous)]
	      [previous-dir (send previous get-dir)]
	      [normalized-file (file:normalize-path file)])
	  
	  (let-values ([(dir _1 _2) (split-path normalized-file)])
	    (send new set-dir dir))
	  (send new set-file normalized-file)
	  
	  (send (send new get-editor) insert
		(if previous-dir
		    (file:find-relative-path previous-dir normalized-file)
		    file))
	  new))

      (define-struct open-info (touched? open?))
      (define open-table (make-hash-table))

      (define (push-file file)
	(set! ignore-open/closes? #t)
	(let* ([previous (car hl-stack)])
	  (when (string? previous)
	    (set! previous (create-list-item
			    (cadr hl-stack)
			    previous
			    (lambda (o) (send o new-list hierlist-item-mixin))))
	    (set-car! hl-stack previous)
	    (let* ([sym (string->symbol (send previous get-file))]
		   [open-info
		    (hash-table-get
		     open-table
		     sym
		     (lambda ()
		       (let ([info (make-open-info #t #t)])
			 (set! changed? #t)
			 (hash-table-put! open-table sym info)
			 info)))])
	      (set-open-info-touched?! open-info #t)
	      (print-struct #t)
	      (send previous open)
	      (if (open-info-open? open-info)
		  (send previous open)
		  (send previous close))))
	  (set! hl-stack (cons file hl-stack)))
	(set! ignore-open/closes? #f))

      (define (pop-file)
	(let ([top (car hl-stack)])
	  (when (string? top)
	    (create-list-item
	     (cadr hl-stack)
	     top
	     (lambda (o) (send o new-item hierlist-item-mixin)))))
	(set! hl-stack (cdr hl-stack)))

      (super-init project-name #f 400 450)

      (define mb (get-menu-bar))
      (define project-menu (make-object menu% "Project" mb))
      (make-object menu-item% "New Project" project-menu (lambda x (new-project)))
      (make-object menu-item% "Open Project" project-menu (lambda x (open-project)))
      (make-object menu-item% "Add Files..." project-menu (lambda x (add-files)))
      (make-object menu-item% "Configure Language..." project-menu (lambda x (configure-language)))

      (make-object button% "Execute" (get-area-container) (lambda x (execute-project)))
      (define top-panel (make-object horizontal-panel% (get-area-container)))
      (define left-panel (make-object vertical-panel% top-panel))
      (define to-load-files-panel (make-object horizontal-panel% left-panel '(border)))
      (define files-list-box (make-object list-box% #f null to-load-files-panel
					  (lambda (lb evt)
					    (files-list-box-callback
					     (send evt get-event-type)))
					  '(single)))
      (define to-load-button-panel (make-object vertical-panel% to-load-files-panel))
      (define up-button (make-object button% "Up" to-load-button-panel (lambda x (move-file-up))))
      (define open-button (make-object button% "Open" to-load-button-panel (lambda x (open-file))))
      (define remove-button (make-object button% "Remove" to-load-button-panel (lambda x (remove-file))))
      (define down-button (make-object button% "Down" to-load-button-panel (lambda x (move-file-down))))
      (send to-load-button-panel stretchable-width #f)
      (send to-load-button-panel set-alignment 'center 'center)

      (define loaded-files-outer-panel (make-object vertical-panel% left-panel))
      (define loaded-files-panel (make-object horizontal-panel% loaded-files-outer-panel '(border)))
      (define loaded-files-hierarchical-list (make-object hierlist% loaded-files-panel))
      (define loaded-files-button-panel (make-object vertical-panel% loaded-files-panel))
      (define open-loaded-file-button (make-object button% "Open" loaded-files-button-panel (lambda x (open-loaded-file))))
      (send loaded-files-button-panel set-alignment 'center 'center)
      (send loaded-files-button-panel stretchable-width #f)

      (send loaded-files-outer-panel change-children (lambda (l) null))
      (send loaded-files-outer-panel stretchable-height #f)

      (let* ([buttons (list up-button open-button down-button remove-button open-loaded-file-button)]
	     [max-width (apply max (map (lambda (x) (send x get-width)) buttons))])
	(for-each (lambda (button) (send button min-width max-width))
		  buttons))

      (send open-loaded-file-button enable #f)

      (update-buttons)

      (make-object button% "Show Loaded Files" (get-area-container) (lambda (b e) (show/hide-loaded-files b e)))

      (when (and filename
		 (file-exists? filename))
	(load-file filename))))

  (define open-project
    (case-lambda
     [(filename)
      (send (make-object project-frame% filename) show #t)]
     [()
      (let ([filename (get-file)])
	(when filename
	  (open-project filename)))]))

  (define (new-project)
    (send (make-object project-frame% #f) show #t))

  (drscheme:get/extend:extend-unit-frame make-project-aware-unit-frame))