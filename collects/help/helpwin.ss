
(unit/sig help:help^
  (import help:search^
	  browser^
	  mzlib:function^
	  mzlib:string^
	  mzlib:file^
	  mzlib:url^
	  mred^
	  (framework : framework^)
	  (frame-mixin))

  (define last-url-string #f)

  (define (open-url-from-user parent goto-url)
    (letrec ([d (make-object dialog% "Open URL" parent 500)]
	     [t (make-object text-field% "URL:" d
			     (lambda (t e)
			       (update-ok)))]
	     [p (make-object horizontal-panel% d)]
	     [browse (make-object button% "Browse..." p
				  (lambda (b e)
				    (let ([f (get-file)])
				      (when f
					(send t set-value (string-append "file:" f))
					(update-ok)))))]
	     [spacer (make-object vertical-pane% p)]
	     [ok (make-object button% "Open" p
			      (lambda (b e)
				(let* ([s (send t get-value)]
				       [done (lambda ()
					       (set! last-url-string s)
					       (send d show #f))])
				  (with-handlers ([void
						   (lambda (x)
						     (if (exn:file-saved-instead? x)
							 (done)
							 (unless (exn:cancelled? x)
							   (message-box "Bad URL" 
									(format "Bad URL: ~a" (exn-message x))
									d))))])
				    (let ([url (string->url
						(cond
						 [(regexp-match ":" s) s]
						 [(regexp-match "^[a-zA-Z][a-zA-Z.]*($|/)" s)
						  (string-append "http://" s)]
						 [else
						  (string-append "file:" s)]))])
				      (goto-url url)
				      (done)))))
			      '(border))]
	     [update-ok (lambda () (send ok enable 
					 (positive? (send (send t get-editor) 
							  last-position))))]
	     [cancel (make-object button% "Cancel" p 
				  (lambda (b e) (send d show #f)))])
      (when last-url-string 
	(send t set-value last-url-string))
      (send p set-alignment 'right 'center)
      (update-ok)
      (send d center)
      (send t focus)
      (send d show #t)))

  (define go-unit
    (unit (import startup-url)
	  (export)

	  (define collecting-thread #f)

	  (define results-editor% (class hyper-text% ()
				    (inherit set-title)
				    (sequence 
				      (super-init #f #f)
				      (set-title "Search Results"))))

	  (define-values (screen-w screen-h) (get-display-size))

	  (define (get-unique-title)
	    (let ([frames (send (framework:group:get-the-frame-group) get-frames)])
	      (let loop ([n 2][name "Help Desk"])
		(if (ormap (lambda (f) (string=? name (send f get-label)))
			   frames)
		    (loop (add1 n) (format "Help Desk ~a" n))
		    name))))

	  (define f (make-object (frame-mixin
				  (class framework:frame:standard-menus% args
				    (inherit get-edit-target-object)
				    (rename [super-on-subwindow-char on-subwindow-char])
				    (public
				      [search-for-help
				       (lambda (text type mode)
					 (send (send search-text get-editor) erase)
					 (run-search text 
						     (case type
						       [(keyword) 0]
						       [(keyword+index) 1]
						       [(all) 2]
						       [else (raise-type-error 'search-for-help
									       "'keyword, 'keyword-index, or 'all"
									       type)])
						     (case mode
						       [(exact) 0]
						       [(contains) 1]
						       [(regexp) 2]
						       [else (raise-type-error 'search-for-help
									       "'exact, 'contains, or 'regexp"
									       mode)])))]

				      [goto-url (lambda (url) (send results goto-url url #f))])
						  

				    (private
				      [edit-menu:do (lambda (const)
						      (lambda (menu evt)
							(let ([edit (get-edit-target-object)])
							  (when (and edit (is-a? edit editor<%>))
							    (send edit do-edit-operation const)))))])
				    
				    (override
				      [file-menu:new-string (lambda () "Help Desk")]
				      [file-menu:new (lambda (i e) (new-help-frame startup-url))]

				      [file-menu:open-string (lambda () "URL")]
				      [file-menu:open
				       (lambda (i e)
					 (open-url-from-user this goto-url))]
				      
				      [file-menu:print (lambda (i e) (send (send results get-editor) print))]

				      [edit-menu:undo (edit-menu:do 'undo)]
				      [edit-menu:redo (edit-menu:do 'redo)]
				      [edit-menu:cut (edit-menu:do 'cut)]
				      [edit-menu:clear (edit-menu:do 'clear)]
				      [edit-menu:copy (edit-menu:do 'copy)]
				      [edit-menu:paste (edit-menu:do 'paste)]
				      [edit-menu:select-all (edit-menu:do 'select-all)]

				      [edit-menu:find-string (lambda () "in Page...")]
				      [edit-menu:find (lambda (i e)
							(send results force-display-focus #t)
							(letrec ([d (make-object dialog% "Find" f 300)]
								 [enable-find (lambda ()
										(send find enable 
										      (positive? (send (send t get-editor) 
												       last-position))))]
								 [t (make-object text-field% "Find:" d
										 (lambda (t e) (enable-find)))]
								 [p (make-object horizontal-panel% d)]
								 [find (make-object button% "Find" p
										    (lambda (b e)
										      (find-str (send t get-value)))
										    '(border))]
								 [close (make-object button% "Close" p
										     (lambda (b e) (send d show #f)))])
							  (send t set-value (or last-find-str ""))
							  (enable-find)
							  (send p set-alignment 'right 'center)
							  (send d center)
							  (send t focus)
							  (send d show #t))
							(send results force-display-focus #f))]
				      [edit-menu:between-find-and-preferences
				       (lambda (menu)
					 (make-object menu-item% "Find Again" menu
						      (lambda (i e) (find-str))
						      (and (framework:preferences:get 'framework:menu-bindings)
							   #\G))
					 (make-object separator-menu-item% menu))]

				      [help-menu:about-string (lambda () "Help Desk")]
				      [help-menu:about (lambda (i e)
							 (message-box "About Help Desk"
								      (format "Help Desk is a complete source of ~
                                                                       information about PLT software, including DrScheme, ~
                                                                       MzScheme, and MrEd.~n~n~
                                                                       Copyright (c) 1995-99 PLT")))]
				      [help-menu:after-about
				       (lambda (menu)
					 (make-object menu-item% "Help" menu
						      (lambda (i e)
							(message-box
							 "Help on Help"
							 "Click the `Home' button, then follow the `How to use Help Desk' link."))))])
				    
				    (override 
				      [on-subwindow-char 
				       (lambda (w e)
					 (case (send e get-key-code)
					   [(prior) (send (send results get-editor) move-position 'up #f 'page) #t]
					   [(next) (send (send results get-editor) move-position 'down #f 'page) #t]
					   [(left) (if (send e get-meta-down)
						       (send html-panel rewind)
						       (super-on-subwindow-char w e))]
					   [(right) (if (send e get-meta-down)
							(send html-panel forward)
							(super-on-subwindow-char w e))]
					   [else (if (and (eq? #\tab (send e get-key-code))
							  (eq? w results))
						     ; Override normal behavior, which is to pass the tab on to
						     ; the edit
						     (if (send e get-shift-down)
							 (send before-results focus)
							 (let ([e (send search-text get-editor)])
							   (send search-text focus)
							   (send e set-position 0 (send e last-position)
								 #f #t 'local)))
						     (super-on-subwindow-char w e))]))])
				    (sequence (apply super-init args))))
				 (get-unique-title) #f 600 (max 440 (min 800 (- screen-h 60)))))

	  (define html-panel (make-object (class hyper-panel% ()
					    (inherit get-canvas)
					    (rename [super-leaving-page leaving-page])
					    (public
					      [stop-search
					       (lambda ()
						 (when collecting-thread
						   (semaphore-wait break-sema)
						   (break-thread collecting-thread)
						   (semaphore-post break-sema)))])
					    (override
					      [on-url-click
					       (lambda (k url)
						 ;; Watch for clicks from docs to undownloaded docs
						 ((let/ec escape
						    ;; Try to see if this is a link to missing documentation.
						    ;; Many things can go wrong; give up if anything does...
						    (with-handlers ([void void])
						      (let ([start (send (send (get-canvas) get-editor) get-url)])
							(when (or (not start)
								  (string=? "file" (url-scheme start)))
							  (let ([url (if (string? url)
									 (if start
									     (combine-url/relative start url)
									     (string->url url))
									 url)])
							    (when (string=? "file" (url-scheme url))
							      (when (not (file-exists? (url-path url)))
								;; Try comparing the base-of-base-of-url to
								;;  the collection directory. Many things can go wrong,
								;;  in which case we let the normal error happen.
								(let-values ([(doc-dir) (normalize-path (collection-path "doc"))]
									     [(dest-dir dest-doc)
									      (let-values ([(base name dir?)
											    (split-path (simplify-path (url-path url)))]) 
										(let-values ([(base name dir?)
											      (split-path base)])
										  (values (normalize-path base) name)))])
								  (when (and (string=? doc-dir dest-dir)
									     (not (string=? dest-doc "help")))
								    ;; Looks like the user needs to download some docs,
								    ;;  rather than a generic file-not-found error.
								    ;; Redirect to information about missing docs:
								    (escape
								     (lambda ()
								       (global-defined-value 'missing-doc-name dest-doc)
								       (send (get-canvas) goto-url 
									     (string-append
									      "file:"
									      (build-path (collection-path "help") "notthere.htm"))
									     #f)))))))))))
						    (k url)
						    void)))]
					      [leaving-page
					       (lambda (page new-page)
						 (unless (is-a? (page->editor new-page) results-editor%)
						   (stop-search))
						 (super-leaving-page page new-page))]
					      [filter-notes
					       (lambda (l)
						 (let ([lib (ormap (lambda (s)
								     (let ([m (regexp-match "MzLib=(.*)" s)])
								       (and m
									    (format "Mz/Mr: load with (require-library \"~a.ss\")"
										    (cadr m)))))
								   l)])
						   (if lib
						       (string-append
							(if (member "Core" l)
							    ""
							    "Beg/Int/Adv: not available   ")
							lib)
						       (let ([drlibs
							      (filter values (map
									      (lambda (s)
										(let ([m (regexp-match "teach=(.*)" s)])
										  (and m (cadr m))))
									      l))])
							 (if (pair? drlibs)
							     (format "Teaching library: select ~a using Set Library To... in DrScheme"
								     (let loop ([s (format "~s" (car drlibs))]
										[l (cdr drlibs)])
								       (if (null? l)
									   s
									   (loop (format "~a or ~s" s (car l)) (cdr l)))))
							     "")))))]
					      [on-navigate stop-search])
					    (sequence (super-init #t (send f get-area-container))))))
	  (define results (send html-panel get-canvas))

	  (define before-results
	    (let loop ([l (send html-panel get-children)])
	      (cond
	       [(null? (cdr l)) results]
	       [(eq? (cadr l) results) (let loop ([v (car l)])
					 (if (is-a? v area-container<%>)
					     (loop (car (last-pair (send v get-children))))
					     v))]
	       [else (loop (cdr l))])))

	  (define top (make-object vertical-pane% f))
	  (define search-text (make-object text-field% "Find docs for:" top
					   (lambda (t e)
					     (send search enable (positive? (send (send t get-editor) last-position))))))
	  (define search-pane (make-object horizontal-pane% top))
	  (define search (make-object button% "Search" search-pane 
				      (lambda (b e) 
					(run-search (send search-text get-value)
						    (send where get-selection)
						    (send exact get-selection)))
				      '(border)))
	  (define where (make-object choice% #f '("for Keyword"
						  "for Keyword or Index Entry"
						  "for Keyword, Index Entry, or Text")
				     search-pane void))
	  (define exact (make-object choice% #f '("exact match"
						  "containing match"
						  "regexp match")
				     search-pane void))
	  (define stop (make-object button% "Stop" search-pane
				    (lambda (b e)
				      (break-thread collecting-thread))))

	  (define (run-search text search-level exactness)
	    (semaphore-wait break-sema) ; protects from too-early break
	    (let ([e (send results get-editor)])
	      (when (is-a? e results-editor%)
		(send e lock #f)
		(send e erase)
		(send e lock #t)))
	    (set! collecting-thread (thread (lambda () (start-search text search-level exactness)))))
	  
	  (send where set-selection 1)
	  (send exact set-selection 1)
	  (send search enable #f)
	  (send stop show #f)

	  (send top stretchable-height #f)

	  (send search-text focus)

	  (define last-find-str #f)
	  (define find-str
	    (case-lambda
	     [() (find-str last-find-str)]
	     [(s)
	      (set! last-find-str s)
	      (if s
		  (let* ([e (send results get-editor)]
			 [sp (send e get-start-position)]
			 [ep (send e get-end-position)])
		    (let ([pos (send e find-string s 'forward ep 'eof #t #f)])
		      (if pos
			  (send e set-position pos (+ pos (string-length s)))
			  (let ([pos (send e find-string s 'forward 0 sp #t #f)])
			    (if pos
				(send e set-position pos (+ pos (string-length s)))
				(bell))))))
		  (bell))]))

	  (framework:frame:reorder-menus f)

	  (send f show #t)

	  (send results goto-url startup-url #f)

	  (define searching-message-on? #f)
	  (define (searching-done editor)
	    (when searching-message-on?
	      (set! searching-message-on? #f)
	      (send editor erase)))

	  (define cycle-key #f)
	  (define break-sema (make-semaphore 1))

	  (define choices-sema (make-semaphore 1))
	  (define choices null)

	  (define enbolden (make-object style-delta% 'change-bold))

	  (define (find-start key name)
	    (with-handlers ([void (lambda (x) #f)])
	      (let ([l (string-length key)])
		(let loop ([n 0])
		  (if (string=? key (substring name n (+ n l)))
		      n
		      (loop (add1 n)))))))

	  (define (add-choice key name title page label ckey)
	    (semaphore-wait choices-sema)
	    (set! choices (cons (list key name title page label) choices))
	    (semaphore-post choices-sema)
	    (queue-callback
	     (lambda ()
	       (when (eq? cycle-key ckey)
		 (semaphore-wait choices-sema)
		 (let ([l choices]
		       [editor (send results get-editor)])
		   (set! choices null)
		   (semaphore-post choices-sema)
		   (send editor lock #f)
		   (send editor begin-edit-sequence)
		   (searching-done editor)
		   (for-each
		    (lambda (i)
		      (let-values ([(key name title page label) (apply values i)])
			(if key
			    (begin
			      (send editor insert "  " (send editor last-position) 'same #f)
			      (let ([start (send editor last-position)])
				(send editor insert name start 'same #f)
				(let ([end (send editor last-position)]
				      [key-start (find-start key name)])
				  (send editor insert (format " in ~s~n" title) end 'same #f)
				  (send editor make-link-style start end)
				  (when key-start
				    (send editor change-style enbolden (+ key-start start) 
					  (+ key-start start (string-length key))))
				  (send editor set-clickback start end
					(lambda (edit start end)
					  ; We don't catch errors explicitly because the
					  ;  page is always documentation, an on-url-click
					  ;  will catch it.
					  (send html-panel on-url-click
						(lambda (url)
						  (send results goto-url url #f))
						(make-url
						 "file"
						 #f ; host
						 #f ; port
						 page
						 #f ; params
						 #f ; query
						 label)))))))
			    (begin
			      (let ([pos (send editor last-position)])
				(send editor insert (format"~a~a:~n" label name) pos 'same #f)
				(send editor change-style (make-object style-delta% (car page) (cdr page))
				      (+ pos (string-length label)) (- (send editor last-position) 2)))))))
		    (reverse l))
		   (send editor end-edit-sequence)
		   (send editor lock #t))))
	     #f))

	  (define (add-doc-section name ckey)
	    (add-choice #f name #f '(change-weight . bold) "In " ckey))

	  (define (add-kind-section name ckey)
	    '(add-choice #f name #f '(change-style . slant) " " ckey))

	  (define (start-search given-find search-level exactness)
	    (let* ([editor (let ([e (send results get-editor)])
			     (if (is-a? e results-editor%)
				 e
				 (let ([e (make-object results-editor%)])
				   (send e lock #t)
				   (send results set-page (editor->page e) #t)
				   e)))]
		   [tried-find given-find]
		   [regexp? (= 2 exactness)]
		   [exact? (= 0 exactness)]
		   [maxxed-out? #f]
		   [ckey (gensym)]
		   [result #f])
	      (send editor lock #f)
	      (send editor insert "Searching...")
	      (send editor lock #t)
	      (set! searching-message-on? #t)
	      (with-handlers ([exn:misc:user-break?
			       (lambda (x)
				 (queue-callback
				  (lambda ()
				    (when (eq? cycle-key ckey)
				      (send editor lock #f)
				      (searching-done editor)
				      (send editor insert
					    (format "(Search stopped~a.)" 
						    (if maxxed-out?
							" - found maximum allowed matches" 
							""))
					    (send editor last-position)
					    'same
					    #f)
				      (send editor lock #t)))
				  #f))])

		(dynamic-wind
		 (lambda ()
		   (begin-busy-cursor)
		   (send search enable #f)
		   (send where enable #f)
		   (send exact enable #f)
		   (set! cycle-key ckey))
		 (lambda ()
		   (semaphore-post break-sema)
		   (send stop show #t)
		   (set! result
			 (do-search given-find
				    search-level
				    regexp?
				    exact?
				    ckey 
				    (lambda ()
				      (set! maxxed-out? #t)
				      (break-thread (current-thread)))
				    add-doc-section
				    add-kind-section
				    add-choice))
		   (semaphore-wait break-sema)) ; turn off breaks...
		 (lambda ()
		   (semaphore-post break-sema) ; breaks ok now because they have no effect
		   (send stop show #f)
		   (send search enable #t)
		   (send where enable #t)
		   (send exact enable #t)
		   (end-busy-cursor)))
		
		(queue-callback
		 (lambda ()
		   (when (eq? cycle-key ckey)
		     (when result
		       (send editor lock #f)
		       (searching-done editor)
		       (send editor insert result)
		       (send editor lock #t))))
		 #f))))

	  ; Return the frame as the result
	  f))

  (define (new-help-frame startup-url)
    (invoke-unit go-unit startup-url))

  (values new-help-frame open-url-from-user))
