
(unit/sig help:help^
  (import help:search^
	  help:option^
	  browser^
	  mzlib:function^
	  mzlib:string^
	  mzlib:file^
	  mzlib:url^
	  mred^)

  (define prim-exit (exit-handler))
  (define exit-count 0)
  (define exit-sema (make-semaphore 1))

  (define (launch-help-win)
    (set! exit-count (add1 exit-count))
    (parameterize ([current-namespace (make-namespace)])
      (queue-callback
       (lambda ()
	 (exit-handler (lambda (x) (exit-help)))
	 (invoke-unit/sig
	  (require-relative-library "helpr.ss")
	  help:option^
	  mzlib:function^
	  mzlib:string^
	  mzlib:file^
	  mzlib:url^
	  mred^)))))

  (define (exit-help)
    (if (zero? exit-count)
	(prim-exit 0)
	(begin
	  ; Lock is because a separate process might be calling exit
	  (semaphore-wait exit-sema)
	  (set! exit-count (sub1 exit-count))
	  (semaphore-post exit-sema))))
	  
  (define collecting-thread #f)

  (define results-editor% (class hyper-text% ()
			    (inherit set-title)
			    (sequence 
			      (super-init #f)
			      (set-title "Search Results"))))

  (define-values (screen-w screen-h) (get-display-size))

  (define f (make-object (class frame% args
			   (rename [super-on-subwindow-char on-subwindow-char])
			   (override 
			     [on-close exit-help]
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
			   (sequence (apply super-init args)))
			 "PLT Help Desk" #f 600 (max 440 (min 800 (- screen-h 60)))))
  (define html-panel (make-object (class hyper-panel% ()
				    (rename [super-leaving-page leaving-page])
				    (public
				      [stop-search
				       (lambda ()
					 (when collecting-thread
					   (semaphore-wait break-sema)
					   (break-thread collecting-thread)
					   (semaphore-post break-sema)))])
				    (override
				      [leaving-page
				       (lambda (page new-page)
					 (unless (is-a? (page->editor new-page) results-editor%)
					   (stop-search))
					 (super-leaving-page page new-page))]
				      [on-navigate stop-search])
				    (sequence (super-init f)))))
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
				(semaphore-wait break-sema) ; protects from too-early break
				(let ([e (send results get-editor)])
				  (when (is-a? e results-editor%)
				    (send e lock #f)
				    (send e erase)
				    (send e lock #t)))
				(set! collecting-thread (thread start-search))) 
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

  (let* ([mb (make-object menu-bar% f)]
	 [file (make-object menu% "&File" mb)]
	 [edit (make-object menu% "&Edit" mb)])
    (append-editor-operation-menu-items edit)
    (make-object separator-menu-item% edit)
    (make-object menu-item% "Find on Page..." edit
		 (lambda (i e)
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
		   (send results force-display-focus #f))
		 #\F)
    (make-object menu-item% "Find Again" edit
		 (lambda (i e) (find-str))
		 #\G)

    (make-object menu-item% "Open URL..." file 
		 (lambda (i e)
		   (letrec ([d (make-object dialog% "Open URL" f 500)]
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
					       (let ([s (send t get-value)])
						 (with-handlers ([void
								  (lambda (x)
								    (message-box "Bad URL" 
										 (format "Bad URL: ~a" (exn-message x))
										 d))])
						   (let ([url (string->url s)])
						     (send results goto-url url #f)
						     (send d show #f)))))
					     '(border))]
			    [update-ok (lambda () (send ok enable 
							(positive? (send (send t get-editor) 
									 last-position))))]
			    [cancel (make-object button% "Cancel" p 
						 (lambda (b e) (send d show #f)))])
		     (send p set-alignment 'right 'center)
		     (send ok enable #f)
		     (send d center)
		     (send t focus)
		     (send d show #t)))
		 #\O)
    (make-object menu-item% "New Help Desk" file 
		 (lambda (m i) (launch-help-win))
		 #\N)
    (make-object separator-menu-item% file)
    (make-object menu-item% "Print" file
		 (lambda (m i) (send (send results get-editor) print))
		 #\P)
    (make-object separator-menu-item% file)
    (make-object menu-item% "Quit" file (lambda (i e) (exit-help) (send f show #f)) #\Q))

  (send f show #t)

  (send results goto-url startup-url #f)

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
				  (send results goto-url 
					(make-url
					 "file"
					 #f ; host
					 #f ; port
					 page
					 #f ; params
					 #f ; query
					 label)
					#f))))))
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

  (define (start-search)
    (let* ([editor (let ([e (send results get-editor)])
		     (if (is-a? e results-editor%)
			 e
			 (let ([e (make-object results-editor%)])
			   (send e lock #t)
			   (send results set-page (editor->page e) #t)
			   e)))]
	   [given-find (send search-text get-value)]
	   [tried-find given-find]
	   [search-level (send where get-selection)]
	   [regexp? (= 2 (send exact get-selection))]
	   [exact? (= 0 (send exact get-selection))]
	   [maxxed-out? #f]
	   [ckey (gensym)]
	   [result #f])
      (with-handlers ([exn:misc:user-break?
		       (lambda (x)
			 (queue-callback
			  (lambda ()
			    (when (eq? cycle-key ckey)
			      (send editor lock #f)
			      (send editor insert
			       (format "(Search stopped~a.)" 
				       (if maxxed-out?
					   " - found maximum allowed matches" 
					   "")))
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
			      (break-thread (current-thread)))))
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
	       (send editor insert result)
	       (send editor lock #t))))
	    #f))))
  
  (yield (make-semaphore 0)))
