;;
;; TODO
;; - track files in project.
;;   - when project is opened check already open files.
;;   - when executing the project, offer to save files.
;; - remember shown-state of loaded window
;; - add a show menu for showing loaded files and ??? files
;; - make the two files windows in the project manager same min-size
;; - leak in project execute?

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
	 (when (directory-exists? collection-path-dir)
	   (for-each
	    (lambda (collection)
	      (when (and (directory-exists? (build-path collection-path-dir collection))
			 (not (string=? collection "CVS")))
		(hash-table-put! colls (string->symbol collection) #t)))
	    (directory-list collection-path-dir))))
       (current-library-collection-paths))
      (function:quicksort (hash-table-map colls (lambda (x v) (symbol->string x))) string<=?)))

  (define (add-common-project-menu-items project-menu)
    (local [(define new-project-item (make-object menu-item% "New Project" project-menu (lambda x (new-project))))
	    (define open-project-item (make-object menu-item% "Open Project" project-menu (lambda x (open-project))))
	    (define collection-projects (make-object menu% "Collection Projects" project-menu))]
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
       (all-collections))))
  
  (define project-aware-frame<%>
    (interface ()
      project:set-project-window
      project:get-project-window))
  
  (define (make-project-aware-unit-frame super%)
    (class/d* super% (project-aware-frame<%>) args

      ((inherit get-menu-bar definitions-text set-label-prefix)
       (override change-to-file)
       (rename [super-change-to-file change-to-file])
       (public 
         project:set-project-window ;; : ((union #f (instance project-frame%)) -> void)
	 project:get-project-window ;; : (-> (union #f (instance project-frame%)))
         ))

      (define (change-to-file filename)
	(super-change-to-file filename)
	(set-project-window filename))

      (define (set-project-window fn)
	(let ([f (ormap (lambda (f) 
			  (if (send f has-file? (send definitions-text get-filename))
			      f
			      #f))
			project-frames)])
	  (project:set-project-window f)))

      (define project-window #f)
      (define (project:get-project-window) project-window)
      (define (project:set-project-window p)
        (send project-menu-item set-label 
              (if p
                  (format "Bring project ~a to the front" (ivar p project-name))
                  "Bring project to the front"))
        (send project-menu-item enable p)
	(if p
	    (set-label-prefix (ivar p project-name))
	    (set-label-prefix "DrScheme"))
        (set! project-window p))
      

      (apply super-init args)

      (define mb (get-menu-bar))
      (define project-menu (make-object menu% "Project" mb))
      (add-common-project-menu-items project-menu)
      (make-object separator-menu-item% project-menu)
      
      (define project-menu-item (make-object menu-item% 
                                  "Bring project to the front"
                                  project-menu
                                  (lambda xxx
                                    (when project-window
                                      (send project-window show #t)))))
      (send project-menu-item enable #f)
      
      (and (send definitions-text get-filename)
	   (set-project-window (send definitions-text get-filename)))

      (frame:reorder-menus this)))

  (define project-frames null)
  
  (define project-frame%
    (class/d (drscheme:frame:basics-mixin frame:standard-menus%) (filename)
      ((inherit get-area-container get-menu-bar)
       (override file-menu:save file-menu:save-as
		 can-close?
                 on-close)
       (public project-name ;; : string
               has-file? ;; : (string -> boolean)
               ))

      (define (on-close)
        (set! project-frames (function:remove this project-frames))
	(send (group:get-the-frame-group)
	      for-each-frame
	      (lambda (frame)
		(when (and (is-a? frame project-aware-frame<%>)
			   (eq? this (send frame project:get-project-window)))
		  (send frame project:set-project-window #f))))
        (shutdown-project))
      
      (define (has-file? file)
        (let ([n (file:normalize-path file)])
          (ormap (lambda (name) (string=? file (file:normalize-path name)))
                 (append (map (lambda (file)
                                (if (string? file)
                                    file
                                    (build-path (apply collection-path (cdr file)) (car file))))
                              files)
                         loaded-files))))
      
      (define-values (is-changed? is-changed is-not-changed)
	(let ([changed? #f]
	      [sema (make-semaphore 1)])
	  (values
	   (lambda ()
	     changed?)
	   (lambda ()
	     (semaphore-wait sema)
	     (set! changed? #t)
	     (semaphore-post sema))
	   (lambda (f)
	     (dynamic-wind
	      (lambda ()
		(semaphore-wait sema))
	      (lambda ()
		(f)
		(set! changed? #f))
	      (lambda ()
		(semaphore-post sema)))))))

      (define (can-close?)
	(if (is-changed?)
	    (case (gui-utils:unsaved-warning
		   (if (string? filename)
		       filename
		       "Untitled")
		   "Close"
		   #t)
	      [(continue) #t]
	      [(save) (save)]
	      [(cancel) #f])
	    #t))

      (define (save)
	(if filename
	    (begin (save-file filename) #t)
	    (save-as)))

      (define (save-as)
	(let ([new-fn (get-file "Choose a project filename" this)])
	  (if new-fn
	      (begin (set! filename new-fn)
		     (save-file filename)
		     #t)
	      #f)))

      (define (file-menu:save . xxx)
	(save))

      (define (file-menu:save-as . xxx)
	(save-as))

      (define project-name
	(if filename
	    (let-values ([(base top __) (split-path filename)])
	      top)
	    "Untitled"))

      (define files null)
      (define language-settings 
        (let ([re:mred (regexp "MrEd")])
          (let loop ([settings drscheme:basis:settings])
            (cond
              [(null? settings) (preferences:get 'drscheme:settings)]
              [else (let ([setting (car settings)])
                      (cond
                        [(regexp-match re:mred (drscheme:basis:setting-name setting))
                         setting]
                        [else
                         (loop (cdr settings))]))]))))
      (define collection-paths #f)

      (define project-custodian (make-custodian))

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

      ;; get-collection-paths : (-> (union (listof string) #f))
      (define (get-collection-paths)
	(define-struct error (msg))

	(if collection-paths
	    (let ([cust (make-custodian)]
		  [done-sema (make-semaphore 0)]
		  [protect-sema (make-semaphore 1)]
		  [running-thread #f]
		  [dialog #f]

		  [got-value? #f]
		  [value #f])

	      ;; run the program
	      (parameterize ([current-custodian cust])
		(parameterize ([current-eventspace (make-eventspace)])
		  (queue-callback
		   (lambda ()
		     (semaphore-wait protect-sema)
		     (set! running-thread (current-thread))
		     (semaphore-post protect-sema)

		     (let* ([str (apply string-append (map (lambda (x) (string-append x (string #\newline)))
							   collection-paths))]
			    [ptr 0]
			    [port (make-input-port
				   (lambda ()
				     (if (= ptr (string-length str))
					 eof
					 (begin0 (string-ref str ptr)
						 (set! ptr (+ ptr 1)))))
				   (lambda () #t)
				   (lambda () (void)))])
		       (let ([ans #f])
			 (let/ec k
			   (parameterize ([current-exception-handler
					   (lambda (e)
					     (set! value
						   (make-error
						    (if (exn? e)
							(exn-message e)
							(format "~s" e))))
					     (k (void)))])
			     (break-enabled #t)
			     (set! value (eval (read port)))))
			 (break-enabled #f)
			 (set! got-value? #t)))

		     (semaphore-post done-sema)))))

	      ;; wake up main thread after five seconds
	      ;; even if no value is yet obtained.
	      (thread
	       (lambda ()
		 (sleep 5)
		 (semaphore-wait protect-sema)
		 (if got-value?
		     (semaphore-post protect-sema)
		     (begin (semaphore-post done-sema)
			    (semaphore-post protect-sema)))))
	      
	      ;; wait for wakeup here.
	      (semaphore-wait done-sema)

	      ;; if no value is yet ready, sleep again,
	      ;; but this time with a dialog that offers killing the evaluation.
	      (semaphore-wait protect-sema)
	      (if got-value?
		  (begin
		    (semaphore-post protect-sema))
		  (begin
		    (set! dialog (make-object dialog% "Evaluating collection path expression" this))
		    (let ([p (make-object horizontal-panel% dialog)])
		      (make-object button% "Break evaluation" p
				   (lambda xxx
				     (when running-thread
				       (break-thread running-thread))))
		      (make-object button% "Kill evaluation" p
				   (lambda xxx
				     (custodian-shutdown-all cust)
				     (set! value (make-error "Killed evaluation"))
				     (set! got-value? #t)
				     (semaphore-post done-sema))))
		    (thread
		     (lambda ()
		       (semaphore-wait done-sema)
		       (send dialog show #f)))
		    (semaphore-post protect-sema)
		    (send dialog show #t)))

	      (if (error? value)
		  (begin
		    (message-box "Collection Path Evaluation Unsuccessful"
				 (error-msg value))
		    #f)
		  (if (and (list? value)
			   (andmap string? value))
		      value
		      (begin
			(message-box 
			 "Collection Path Evaluation Unsuccessful"
			 (format "expected result to be a list of strings, got: ~e"
				 value))
			#f))))
	    #f))

      (define (shutdown-project)
        (reset-hierlist)
        (custodian-shutdown-all project-custodian))
      
      ;; offer-to-save-files : (-> boolean)
      ;; returns true when the user has either saved or okay'd not saving
      ;; each file that is unsaved.
      (define (offer-to-save-files)
	(let ([frames null])
	  (send (group:get-the-frame-group)
		for-each-frame
		(lambda (frame)
		  (when (and (is-a? frame project-aware-frame<%>)
			     (eq? this (send frame project:get-project-window))
			     (send (ivar frame definitions-text) is-modified?))
		    (set! frames (cons frame frames)))))
	  (let loop ([frames frames])
	    (cond
	     [(null? frames) #t]
	     [else
	      (let* ([frame (car frames)]
		     [fn (or (send (ivar frame definitions-text) get-filename)
			     (send frame get-title))])
		(case (gui-utils:unsaved-warning
		       fn
		       "Execute"
		       #t)
		  [(continue) (loop (cdr frames))]
		  [(save)
		   (send (ivar frame definitions-text) save-file)
		   (loop (cdr frames))]
		  [(cancel) #f]))]))))

      (define (execute-project)
	(when (offer-to-save-files)
	  (shutdown-project)
	  (set! project-custodian (make-custodian))

	  (let ([collection-paths (get-collection-paths)])
	    (parameterize ([current-custodian project-custodian])
	      (let ([project-eventspace (make-eventspace)])
		(parameterize ([current-eventspace project-eventspace])
		  (queue-callback
		   (lambda ()
		     (when collection-paths
		       (current-library-collection-paths collection-paths))
		     (drscheme:basis:initialize-parameters project-custodian language-settings)

		     (exit-handler
		      (lambda x
			(custodian-shutdown-all project-custodian)))


		     (let* ([error-escape-k #f]
			    [project-manager-bottom-escape-handler
			     (lambda ()
			       (error-escape-k))]
			    [primitive-event-dispatch-handler (event-dispatch-handler)]
			    [project-manager-event-dispatch-handler
			     (lambda (eventspace)
			       (if (eq? eventspace project-eventspace)
				   (let/ec k
				     (set! error-escape-k k)
				     (primitive-event-dispatch-handler eventspace))
				   (primitive-event-dispatch-handler)))])
		       (drscheme:basis:bottom-escape-handler project-manager-bottom-escape-handler)
		       (event-dispatch-handler
			project-manager-event-dispatch-handler))

		     (drscheme:basis:error-display/debug-handler
		      (let ([project-manager-error-display/debug-handler
			     (lambda (msg zodiac exn)
			       (if (and zodiac
					(zodiac:zodiac? zodiac))
				   (show-error/open-file msg zodiac)
				   (message-box (format "Error running project ~a" project-name) msg)))])
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
		      files)))))))))

      (define (configure-collection-paths)
	(let* ([dialog (make-object dialog% "Configure Collection Paths" this 200 400)]
	       [text (make-object (scheme:text-mixin text:basic%))]
	       [message (make-object message% "Specify an expression for the collection paths" dialog)]
	       [canvas (make-object editor-canvas% dialog text)]
	       [button-panel (make-object horizontal-panel% dialog)]
	       [cancel? #f]
	       [ok (make-object button% "OK" button-panel (lambda xxx (send dialog show #f)))]
	       [cancel (make-object button% "Cancel" button-panel
                         (lambda xxx
                           (set! cancel? #t) 
                           (send dialog show #f)))])
	  (send canvas focus)
	  (send ok min-width (send cancel get-width))
	  (send button-panel stretchable-height #f)
	  (send button-panel set-alignment 'right 'center)
	  (when collection-paths
	    (for-each (lambda (s) (send text insert s) (send text insert #\newline))
		      collection-paths))
	  (send dialog show #t)
	  (unless cancel?
	    (set! collection-paths
		  (let loop ([n (+ (send text last-paragraph) 1)]
			     [acc null])
		    (cond
		     [(zero? n) acc]
		     [else
		      (loop (- n 1)
			    (cons (send text get-text
					(send text paragraph-start-position (- n 1))
					(send text paragraph-end-position (- n 1)))
				  acc))])))
	    (is-changed))))

      (define (configure-language)
	(let ([new-settings (drscheme:language:language-dialog language-settings)])
	  (when new-settings
	    (set! language-settings new-settings)
	    (is-changed))))

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
	(is-changed)
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
					    (car file))])]
               [frame (handler:edit-file filename)])
          (when (is-a? frame project-aware-frame<%>)
            (send frame project:set-project-window this))))
        
			  
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
	  (is-changed)))

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

	(define new-files (finder:common-get-file-list))

	(define (prompt-user-collection? filename collection collection-dir)
	  (define answer #t)
	  (define dialog (make-object dialog% "Use a collection-based-path?" #f 400 400))
	  (define text (make-object text%))
	  (define ec (make-object editor-canvas% dialog text))

	  (define bp (make-object horizontal-panel% dialog))

	  (define just-one? (or (not new-files)
				(null? new-files)
				(null? (cdr new-files))))

	  (define no-all-button (make-object button% "No to all" bp
					     (lambda xx
					       (set! answer #f)
					       (set! prompt-user-collection?
						     (lambda xxx #f))
					       (send dialog show #f))))
	  (define no-button (make-object button% "No" bp
					 (lambda xx (set! answer #f) (send dialog show #f))))
	  (define yes-button (make-object button% "Yes" bp
					  (lambda xx (set! answer #t) (send dialog show #f))
					  (if just-one?
					      '(border)
					      '())))
	  (define yes-all-button (make-object button% "Yes to all" bp
					      (lambda xx
						(set! answer #t)
						(set! prompt-user-collection?
						      (lambda xxx #t))
						(send dialog show #f))
					      (if just-one?
						  '()
						  '(border))))

	  (if just-one?
	      (send yes-button focus)
	      (send yes-all-button focus))

	  (when just-one?
	    (send bp change-children (lambda (l) (list no-button yes-button))))

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
~nShould this file be collection relative~
~n(that is, loaded with require-library instead of load)?"
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

	(when new-files
	  (let ([collections (let ([drs-collections (all-collections)]
				   [proj-collections (get-collection-paths)])
			       (if proj-collections
				   (append proj-collections
					   drs-collections)
				   drs-collections))]
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
	  (is-changed)
	  (refresh-files-list-box)))

      (define (load-file filename)
	(with-handlers ([(lambda (x) #t)
			 (lambda (x)
			   (message-box "Error loading project file"
					(format "~a" (if (exn? x)
							 (exn-message x)
							 (format "~s" x)))))])
	  (let* ([raw (call-with-input-file filename read 'text)])
	    (unless (and (list? raw)
			 (= 2 (length raw))
			 (eq? 'quote (car raw)))
	      (error 'bad-format))
	    (let ([all (cadr raw)])
	      (unless (and (list? all)
			   (andmap (lambda (x)
				     (and (list? x)
					  (= 2 (length x))
					  (symbol? (car x))))
				   all))
		(error 'bad-format))
	      (let ([loaded-files (assoc 'files all)]
		    [loaded-language-settings (assoc 'settings all)]
		    [loaded-open-table (assoc 'open-table all)]
		    [loaded-collection-paths (assoc 'collection-paths all)]
		    [loaded-to-load-files-shown? (assoc 'to-load-files-shown? all)]
		    [loaded-loaded-files-shown? (assoc 'loaded-files-shown? all)])

		(if (and loaded-language-settings
			 (= (arity drscheme:basis:make-setting)
			    (length (function:second loaded-language-settings))))
		    (set! language-settings
			  (apply drscheme:basis:make-setting (function:second loaded-language-settings)))
		    (message-box 
		     "Loading Project"
		     (format "Resetting language settings to default; saved language settings are from old version")))

		;; need to update the gui at this point...
		(when loaded-loaded-files-shown?
		  (set! loaded-files-shown? (function:second loaded-loaded-files-shown?)))
		(update-loaded-files-shown)
		(when loaded-to-load-files-shown?
		  (set! to-load-files-shown? (function:second loaded-to-load-files-shown?)))
		(update-to-load-files-shown)
		
		(when loaded-files
		  (set! files (function:second loaded-files)))

		(when loaded-open-table
		  (set! open-table (make-hash-table))
		  (for-each (lambda (t) (hash-table-put! open-table (car t) (make-open-info #t (cadr t))))
			    (function:second loaded-open-table)))

		(when loaded-collection-paths
		  (set! collection-paths (function:second loaded-collection-paths)))

		(refresh-files-list-box))))))

      (define project-save-file-tag ";; project file")

      (define (save-file filename)
	(is-not-changed
	 (lambda ()
	   (call-with-output-file filename
	     (lambda (port)
	       (fprintf port "~a~n" project-save-file-tag)
	       (newline port)
	       (fprintf port "; save data~n")
	       (write `'((loaded-files-shown? ,loaded-files-shown?)
			 (to-load-files-shown? ,to-load-files-shown?)
			 (collection-paths ,collection-paths)
			 (open-table ,(hash-table-map open-table (lambda (x v) (list x (open-info-open? v)))))
			 (files ,files)
			 (settings ,(cdr (vector->list (struct->vector language-settings)))))
		      port)
	       (newline port)
	       (newline port)
	       (fprintf port "; load commands~n")
	       (for-each (lambda (file)
			   (write
			    (cond
			     [(string? file)
			      `(load ,file)]
			     [else
			      `(require-library ,@file)])
			    port)
			   (newline port))
			 files))
	     'truncate 'text))))

      (define loaded-files-shown? #f)
      (define (update-loaded-files-shown)
	(send loaded-files-menu-item set-label
	      (if loaded-files-shown?
		  "Hide Loaded Files"
		  "Show Loaded Files"))
	(send loaded-files-outer-panel change-children
	      (lambda (l)
		(if loaded-files-shown?
		    (list loaded-files-panel)
		    null)))
	(send loaded-files-outer-panel stretchable-height loaded-files-shown?))
      (define (show/hide-loaded-files)
        (set! loaded-files-shown? (not loaded-files-shown?))
        (is-changed)
        (update-loaded-files-shown)
        (unless (or to-load-files-shown?
                    loaded-files-shown?)
          (show/hide-to-load-files)))

      (define to-load-files-shown? #t)
      (define (update-to-load-files-shown)
        (send to-load-files-menu-item set-label
	      (if to-load-files-shown?
		  "Hide Project Files"
		  "Show Project Files"))
	(send to-load-files-outer-panel change-children
	      (lambda (l)
		(if to-load-files-shown?
		    (list to-load-files-panel)
		    null)))
	(send to-load-files-outer-panel stretchable-height to-load-files-shown?))
      (define (show/hide-to-load-files)
	(set! to-load-files-shown? (not to-load-files-shown?))
        (is-changed)
        (update-to-load-files-shown)
        (unless (or to-load-files-shown?
                    loaded-files-shown?)
          (show/hide-loaded-files)))

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
		(is-changed))))

	  (define (on-item-closed i)
	    (unless ignore-open/closes? 
	      (let ([info (hash-table-get open-table (string->symbol (send i get-file)))])
		(set-open-info-open?! info #f)
		(is-changed))))

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

      (define loaded-files null)
      
      (define (push-file file)
	(set! ignore-open/closes? #t)
        (set! loaded-files (cons file loaded-files))
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
			 (is-changed)
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

      (set! project-frames (cons this project-frames))
      
      (define mb (get-menu-bar))

      (define show-menu (make-object menu% "Show" mb))
      (define to-load-files-menu-item
        (make-object menu-item% "Hide Project Files" show-menu (lambda xxx (show/hide-to-load-files))))
      (define loaded-files-menu-item
        (make-object menu-item% "Show Loaded Files" show-menu (lambda xxx (show/hide-loaded-files))))

      (define project-menu (make-object menu% "Project" mb))
      (add-common-project-menu-items project-menu)
      (make-object separator-menu-item% project-menu)
      (make-object menu-item% "Execute" project-menu (lambda x (execute-project)) #\t)
      (make-object menu-item% "Add Files..." project-menu (lambda x (add-files)))
      (make-object menu-item% "Configure Language..." project-menu (lambda x (configure-language)) #\l)
      (make-object menu-item% "Configure Collection Paths..." project-menu (lambda x (configure-collection-paths)))

      (make-object button% "Execute" (get-area-container) (lambda x (execute-project)))
      (define top-panel (make-object horizontal-panel% (get-area-container)))
      (define left-panel (make-object vertical-panel% top-panel))
      (define to-load-files-outer-panel (make-object vertical-panel% left-panel))
      (define to-load-files-panel (make-object horizontal-panel% to-load-files-outer-panel '(border)))
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

      (send loaded-files-button-panel min-height (send to-load-button-panel get-height))

      (send loaded-files-outer-panel change-children (lambda (l) null))
      (send loaded-files-outer-panel stretchable-height #f)

      (let* ([buttons (list up-button open-button down-button remove-button open-loaded-file-button)]
	     [max-width (apply max (map (lambda (x) (send x get-width)) buttons))])
	(for-each (lambda (button) (send button min-width max-width))
		  buttons))

      (send open-loaded-file-button enable #f)

      (update-buttons)

      (frame:reorder-menus this)

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