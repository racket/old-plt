
(unit/sig ()
  (import help:option^
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
	  
  (define MAX-HIT-COUNT 300)

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
						(send search-text focus))
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
    (make-object menu-item% "Find..." edit
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
					      (send ok enable 
						    (positive? (send (send t get-editor) 
								     last-position)))))]
			    [p (make-object horizontal-panel% d)]
			    [browse (make-object button% "Browse..." p
						 (lambda (b e)
						   (let ([f (get-file)])
						     (send t set-value (string-append "file:" f)))))]
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
    (make-object menu-item% "Quit" file (lambda (i e) (exit-help) (send f show #f)) #\Q))

  (send f show #t)

  (send results goto-url startup-url #f)

  (define cycle-key #f)
  (define break-sema (make-semaphore 1))

  (define choices-sema (make-semaphore 1))
  (define choices null)

  (define (add-choice type name title page label ckey)
    (semaphore-wait choices-sema)
    (set! choices (cons (list type name title page label) choices))
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
	      (let-values ([(type name title page label) (apply values i)])
		(if type
		    (begin
		      (send editor insert " " (send editor last-position) 'same #f)
		      (let ([start (send editor last-position)])
			(send editor insert name start 'same #f)
			(let ([end (send editor last-position)])
			  (send editor insert (format " in ~s~n" title) end 'same #f)
			  (send editor make-link-style start end)
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
			(send editor insert (format "In ~a:~n" name) pos 'same #f)
			(send editor change-style (make-object style-delta% 'change-bold) 
			      (+ pos 3) (- (send editor last-position) 2)))))))
	    (reverse l))
	   (send editor end-edit-sequence)
	   (send editor lock #t))))
     #f))

  (define (add-section name ckey)
    (add-choice #f name #f #f #f ckey))

  (define not-break? (lambda (x) (not (exn:misc:user-break? x))))

  ; Define an order for the documentation:
  (define (html-doc-position d)
    (case (string->symbol d)
      [(r5rs) 0]
      [(mzscheme) 1]
      [(mred) 2]
      [(drscheme) 3]
      [(framework) 4]
      [(insidemz) 50]
      [else 100]))

  ; Locate standard HTML documentation
  (define-values (std-docs std-doc-names)
    (let* ([path (collection-path "doc")]
	   [doc-names (directory-list path)]
	   [docs (map (lambda (x) (build-path path x)) doc-names)])
      ; Order the standard docs:
      (let ([ordered (quicksort
		      (map cons docs doc-names)
		      (lambda (a b)
			(< (html-doc-position (cdr a))
			   (html-doc-position (cdr b)))))])
	(values (map car ordered) (map cdr ordered)))))


  ; Check collections for doc.txt files:
  (define-values (txt-docs txt-doc-names)
    (let loop ([collection-paths (current-library-collection-paths)]
	       [docs null]
	       [names null])
      (cond
       [(null? collection-paths)
	(values docs names)]
       [else (let ([path (car collection-paths)])
	       (let cloop ([l (with-handlers ([void (lambda (x) null)]) (directory-list path))]
			   [docs docs]
			   [names names])
		 (cond
		  [(null? l) (loop (cdr collection-paths) docs names)]
		  [(and (directory-exists? (build-path path (car l)))
			(not (member (car l) names))
			(file-exists? (build-path path (car l) "doc.txt")))
		   (cloop (cdr l) (cons (build-path path (car l)) docs)
			  (cons (car l) names))]
		  [else (cloop (cdr l) docs names)])))])))

  (define docs (append std-docs txt-docs))
  (define doc-names (append std-doc-names (map (lambda (s) (format "~a collection" s)) txt-doc-names)))
  (define doc-kinds (append (map (lambda (x) 'html) std-docs) (map (lambda (x) 'text) txt-docs)))

  (define (clean-html s)
    (regexp-replace*
     "&[^;]*;"
     (regexp-replace*
      "<[^>]*>"
      (regexp-replace* 
       "&gt;"
       (regexp-replace*
	"&lt;"
	s
	"<")
       ">")
      "")
     ""))

  (define (with-hash-table ht key compute)
    (hash-table-get
     ht
     (string->symbol key)
     (lambda ()
       (let ([v (compute)])
	 (hash-table-put! ht (string->symbol key) v)
	 v))))

  (define html-keywords (make-hash-table))
  (define (load-html-keywords doc)
    (with-hash-table
     html-keywords
     doc
     (lambda ()
       (with-handlers ([not-break? (lambda (x) null)])
	 (with-input-from-file (build-path doc "keywords")
	   read)))))

  (define html-indices (make-hash-table))
  (define (load-html-index doc)
    (with-hash-table
     html-indices
     doc
     (lambda ()
       (with-handlers ([not-break? (lambda (x) null)])
	 (with-input-from-file (build-path doc "hdindex")
	   read)))))

  (define (parse-txt-file doc ht handle-one)
    (with-hash-table
     ht
     doc
     (lambda ()
       (with-handlers ([not-break? (lambda (x) 
				     ; (printf "~a~n" (exn-message x))
				     null)])
	 (with-input-from-file (build-path doc "doc.txt")
	   (lambda ()
	     (let loop ()
	       (let ([start (file-position (current-input-port))]
		     [r (read-line)])
		 (cond
		  [(eof-object? r) null]
		  [(handle-one r start) => (lambda (vs) (append vs (loop)))]
		  [else (loop)])))))))))

  (define re:keyword-line (regexp "^>[^I]"))
  (define text-keywords (make-hash-table))
  (define (load-txt-keywords doc)
    (parse-txt-file
     doc
     text-keywords
     (lambda (r start)
       (cond
	[(regexp-match re:keyword-line r)
	 (let* ([p (open-input-string (substring r 1 (string-length r)))]
		[entry (read p)]
		[key (let loop ([entry entry])
		       (cond
			[(symbol? entry) entry]
			[(pair? entry) (loop (car entry))]
			[else (error "bad entry")]))]
		[content (if (symbol? entry)
			     (with-handlers ([not-break? (lambda (x) #f)])
			       (let ([s (read p)])
				 (if (eq? s '::)
				     (read p)
				     #f)))
			     #f)])
	   (list
	    ; Make the keyword entry:
	    (list (symbol->string key) ; the keyword name
		  (let ([p (open-output-string)])
		    (if content
			(display content p)
			(display entry p))
		    (get-output-string p)) ; the text to display
		  "doc.txt" ; file
		  start ; label (a position in this case)
		  "doc.txt")))] ; title
	[else #f]))))

  (define re:index-line (regexp "^>INDEX:(.*)"))
  (define text-indices (make-hash-table))
  (define (load-txt-index doc)
    (parse-txt-file
     doc
     text-indices
     (lambda (r start)
       (cond
	[(regexp-match re:index-line r)
	 => (lambda (m)
	      (let ([p (open-input-string (cadr m))])
		(let loop ()
		  (let ([r (read p)])
		    (if (eof-object? r)
			null
			(cons
			 ; Make an index entry:
			 (cons r start)
			 (loop)))))))]
	[else #f]))))

  (define (non-regexp s)
    (list->string
     (apply
      append
      (map
       (lambda (c)
	 (cond 
	  [(memq c '(#\$ #\| #\\ #\[ #\] #\. #\* #\? #\+ #\( #\)))
	   (list #\\ c)]
	  [(char-alphabetic? c)
	   (list #\[ (char-upcase c) (char-downcase c) #\])]
	  [else (list c)]))
       (string->list s)))))

  (define (start-search)
    (let* ([given-find (send search-text get-value)]
	   [find (let ([s given-find])
		   (case (send exact get-selection)
		     [(0) s]
		     [(1) (regexp (non-regexp s))] ; substring (not regexp) match
		     [else (regexp s)]))]
	   [search-level (send where get-selection)]
	   [regexp? (= 2 (send exact get-selection))]
	   [exact? (= 0 (send exact get-selection))]
	   [ckey (gensym)]
	   [editor (let ([e (send results get-editor)])
		     (if (is-a? e results-editor%)
			 e
			 (let ([e (make-object results-editor%)])
			   (send e lock #t)
			   (send results set-page (editor->page e) #t)
			   e)))]
	   [hit-count 0])
      (dynamic-wind
       (lambda ()
	 (begin-busy-cursor)
	 (send search enable #f)
	 (send where enable #f)
	 (send exact enable #f)
	 (set! cycle-key ckey))
       (lambda ()
	 (with-handlers ([exn:misc:user-break?
			  (lambda (x)
			    (queue-callback
			     (lambda ()
			       (when (eq? cycle-key ckey)
				 (send editor lock #f)
				 (send editor insert 
				       (format "(Search stopped~a.)" 
					       (if (= hit-count MAX-HIT-COUNT)
						   " - found maximum allowed matches" 
						   ""))
				       (send editor last-position) 'same #f)
				 (send editor lock #t)))
			     #f))])
	   (semaphore-post break-sema)
	   (send stop show #t)
	   (set! hit-count 0)
	   (for-each
	    (lambda (doc doc-name doc-kind)
	      (define found-one? #f)
	      (define (found)
		(unless found-one?
		  (set! found-one? #t)
		  (add-section doc-name ckey))
		(set! hit-count (add1 hit-count))
		(when (= hit-count MAX-HIT-COUNT)
		  (break-thread (current-thread))))
	      ;; Keyword search
	      (let ([keys (case doc-kind
			    [(html) (load-html-keywords doc)]
			    [(text) (load-txt-keywords doc)]
			    [else null])]
		    [add-key-choice (lambda (v)
				      (found)
				      (add-choice
				       "key" (cadr v) (list-ref v 4)
				       (build-path doc (list-ref v 2))
				       (list-ref v 3)
				       ckey))])
		(unless regexp?
		  (for-each
		   (lambda (v)
		     (when (string=? given-find (car v))
		       (add-key-choice v)))
		   keys))
		(unless exact?
		  (for-each
		   (lambda (v)
		     (when (regexp-match find (car v))
		       (unless (and (not regexp?) (string=? given-find (car v)))
			 (add-key-choice v))))
		   keys)))
	      ;; Index search
	      (unless (< search-level 1)
		(let ([index (case doc-kind
			       [(html) (load-html-index doc)]
			       [(text) (load-txt-index doc)]
			       [else null])]
		      [add-index-choice (lambda (name desc)
					  (case doc-kind
					    [(html)
					     (found)
					     (add-choice "idx" name
							 (list-ref desc 2)
							 (build-path doc (list-ref desc 0))
							 (list-ref desc 1)
							 ckey)]
					    [(text)
					     (found)
					     (add-choice "idx" name
							 "indexed content"
							 (build-path doc "doc.txt")
							 desc
							 ckey)]))])
		  (when index
		    (unless regexp?
		      (for-each
		       (lambda (v)
			 (when (string=? given-find (car v))
			   (add-index-choice (car v) (cdr v))))
		       index))
		    (unless exact?
		      (for-each
		       (lambda (v)
			 (when (regexp-match find (car v))
			   (unless (and (not regexp?) (string=? given-find (car v)))
			     (add-index-choice (car v) (cdr v)))))
		       index)))))
	      ;; Content Search
	      (unless (or (< search-level 2) exact?)
		(let ([files (case doc-kind
			       [(html) (with-handlers ([not-break? (lambda (x) null)]) (directory-list doc))]
			       [(text) (list "doc.txt")]
			       [else null])])
		  (for-each
		   (lambda (f)
		     (with-handlers ([not-break? (lambda (x)
						   ; (printf "~a~n" (exn-message x))
						   #f)])
		       (with-input-from-file (build-path doc f)
			 (lambda ()
			   (let loop ()
			     (let ([pos (file-position (current-input-port))]
				   [r (read-line)])
			       (unless (eof-object? r)
				 (when (regexp-match find r)
				   (found)
				   (add-choice "txt" 
					       (if (eq? doc-kind 'html)
						   (clean-html r)
						   r)
					       "content"
					       (build-path doc f)
					       (if (eq? doc-kind 'text) pos "NO TAG")
					       ckey))
				 (loop))))))))
		   files))))
	    docs doc-names doc-kinds)
	   (queue-callback
	    (lambda ()
	      (when (eq? cycle-key ckey)
		(when (zero? (send editor last-position))
		  (send editor lock #f)
		  (send editor insert (format "Found nothing for \"~a\"." given-find))
		  (send editor lock #t))))
	    #f))
	 (semaphore-wait break-sema)) ; turn off breaks...
       (lambda ()
	 (semaphore-post break-sema) ; breaks ok now because they have no effect
	 (send stop show #f)
	 (send search enable #t)
	 (send where enable #t)
	 (send exact enable #t)
	 (end-busy-cursor)))))

  (yield (make-semaphore 0)))
