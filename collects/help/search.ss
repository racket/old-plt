(unit/sig help:search^
  (import mzlib:function^)
  
  ; Define an order for the documentation:
  (define html-doc-position (car (require-library "docpos.ss" "help")))


  ; These are set by reset-doc-lists:
  (define docs null)
  (define doc-names null)
  (define doc-kinds null)
  (define doc-collection-date #f)

  (define colldocs (require-library "colldocs.ss" "help"))

  (define (reset-doc-lists)
    ; Locate standard HTML documentation
    (define-values (std-docs std-doc-names)
      (let* ([path (with-handlers ([void (lambda (x) #f)])
		     (collection-path "doc"))]
	     [doc-names (if path
			    (directory-list path)
			    null)]
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
      (colldocs quicksort))
    
    (set! docs (append std-docs txt-docs))
    (set! doc-names (append std-doc-names (map (lambda (s) (format "~a collection" s)) txt-doc-names)))
    (set! doc-kinds (append (map (lambda (x) 'html) std-docs) (map (lambda (x) 'text) txt-docs)))

    (with-handlers ([void (lambda (x)
			    (set! doc-collection-date 'none))])
      (set! doc-collection-date 
	    (file-or-directory-modify-seconds
	     (collection-path "doc")))))

  (define MAX-HIT-COUNT 300)
  
  (define (clean-html s)
    (regexp-replace*
     "&[^;]*;"
     (regexp-replace*
      "<[^>]*>"
      (regexp-replace* 
       "&amp;"
       (regexp-replace* 
	"&gt;"
	(regexp-replace*
	 "&lt;"
	 s
	 "<")
	">")
       "\\&")
      "")
     ""))

  (define not-break? (lambda (x) (not (exn:misc:user-break? x))))

  ; One lock for all hash table operations is good enough
  (define ht-lock (make-semaphore 1))

  (define (with-hash-table ht key compute)
    (dynamic-wind
     (lambda () (semaphore-wait ht-lock))
     (lambda ()
       (let ([sym (string->symbol key)])
	 (hash-table-get
	  ht
	  sym
	  (lambda ()
	    (let ([v (compute)])
	      (hash-table-put! ht sym v)
	      v)))))
     (lambda () (semaphore-post ht-lock))))

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
				     (printf "~a~n" (exn-message x))
				     null)])
	 (with-input-from-file doc
	   (lambda ()
	     (let loop ([start 0])
	       (let* ([r (read-line (current-input-port) 'any)]
		      [next (if (eof-object? r)
				start
				(+ start (string-length r) 1))])
		 (cond
		  [(eof-object? r) null]
		  [(handle-one r start) => (lambda (vs) (append vs (loop next)))]
		  [else (loop next)])))))))))

  (define re:keyword-line (regexp "^>"))
  (define text-keywords (make-hash-table))
  (define (load-txt-keywords doc)
    (parse-txt-file
     doc
     text-keywords
     (lambda (r start)
       (cond
	[(regexp-match re:keyword-line r)
	 (let* ([p (open-input-string (substring r 1 (string-length r)))]
		[entry (parameterize ([read-accept-bar-quote #f])
			 (read p))]
		[key (let loop ([entry entry])
		       (cond
			[(symbol? entry) entry]
			[(pair? entry) (if (eq? (car entry) 'quote)
					   (loop (cadr entry))
					   (loop (car entry)))]
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
			(if (and (pair? entry) 
				 (eq? (car entry) 'quote))
			    (fprintf p "'~s" (cadr entry))
			    (display entry p)))
		    (get-output-string p)) ; the text to display
		  #f ; file
		  start ; label (a position in this case)
		  "doc.txt")))] ; title
	[else #f]))))

  (define re:index-line (regexp "_([^_]*)_(.*)"))
  (define text-indices (make-hash-table))
  (define (load-txt-index doc)
    (parse-txt-file
     doc
     text-indices
     (lambda (r start)
       (cond
	[(regexp-match re:index-line r)
	 => (lambda (m)
	      (let loop ([m m])
		(let ([s (cadr m)])
		  (cons 
		    ; Make an index entry:
		   (cons s start)
		   (let ([m (regexp-match re:index-line (caddr m))])
		     (if m
			 (loop m)
			 null))))))]
	[else #f]))))
  
  (define re:splitter (regexp "^ *([^ ]+)(.*)"))
  (define (split-words s)
    (let ([m (regexp-match re:splitter s)])
      (if m
	  (cons (cadr m)
		(split-words (caddr m)))
	  null)))

  (define (non-regexp s)
    (list->string
     (apply
      append
      (map
       (lambda (c)
	 (cond 
	  [(memq c '(#\$ #\| #\\ #\[ #\] #\. #\* #\? #\+ #\( #\) #\^))
	   (list #\\ c)]
	  [(char-alphabetic? c)
	   (list #\[ (char-upcase c) (char-downcase c) #\])]
	  [else (list c)]))
       (string->list s)))))
  
  (define (doc-collections-changed)
    (set! doc-collection-date #f))

  
  ;; do-search : ((? -> ?)
  ;;              ??
  ;;              boolean
  ;;              boolean
  ;;              ??
  ;;              ??
  ;;              (?? -> ??)
  ;;              (?? -> ??)
  ;;              (?? ?? ?? ?? ?? ?? -> ??)
  ;;              ->
  ;;              (union string #f))
  (define (do-search given-find search-level regexp? exact? ckey maxxed-out
		     add-doc-section add-kind-section add-choice)
    ; When new docs are installed, the directory's modification date changes:
    (unless (eq? doc-collection-date 'none)
      (when (or (not doc-collection-date)
		(> (file-or-directory-modify-seconds (collection-path "doc"))
		   doc-collection-date))
	(reset-doc-lists)))
    (let* ([hit-count 0]
	   [string-finds (list given-find)]
	   [finds (cond
		   [exact? (list given-find)]
		   [regexp? (list (regexp given-find))]
		   [else (let ([wl (split-words given-find)])
			   (set! string-finds wl)
			   (map regexp (map non-regexp wl)))])])
      (for-each
       (lambda (doc doc-name doc-kind)
	 (define found-one #f)
	 (define (found kind)
	   (unless found-one
	     (add-doc-section doc-name ckey))
	   (unless (equal? found-one kind)
	     (set! found-one kind)
	     (add-kind-section kind ckey))
	   (set! hit-count (add1 hit-count))
	   (unless (< hit-count MAX-HIT-COUNT)
	     (maxxed-out)))

	 ;; Keyword search
	 (let ([keys (case doc-kind
		       [(html) (load-html-keywords doc)]
		       [(text) (load-txt-keywords doc)]
		       [else null])]
	       [add-key-choice (lambda (v)
				 (found "keyword entries")
				 (add-choice
				  (car v) ; key
				  (cadr v) ; display
				  (list-ref v 4) ; title
				  (if (list-ref v 2)
				      (build-path doc (list-ref v 2))
				      doc)
				  (list-ref v 3) ; label
				  ckey))])

	   (unless regexp?
	     (for-each
	      (lambda (v)
		(when (string=? given-find (car v))
		  (add-key-choice v)))
	      keys))
	   (unless (or exact? (null? finds))
	     (for-each
	      (lambda (v)
		(when (andmap (lambda (find) (regexp-match find (car v))) finds)
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
					(found "index entries")
					(add-choice "" name
						    (list-ref desc 2)
						    (build-path doc (list-ref desc 0))
						    (list-ref desc 1)
						    ckey)]
				       [(text)
					(found "index entries")
 					(add-choice "" name
						    "indexed content"
						    doc
						    desc
						    ckey)]))])
	     (when index
	       (unless regexp?
		 (for-each
		  (lambda (v)
		    (when (string=? given-find (car v))
		      (add-index-choice (car v) (cdr v))))
		  index))
	       (unless (or exact? (null? finds))
		 (for-each
		  (lambda (v)
		    (when (andmap (lambda (find) (regexp-match find (car v))) finds)
		      (unless (and (not regexp?) (string=? given-find (car v)))
			(add-index-choice (car v) (cdr v)))))
		  index)))))
	 ;; Content Search
	 (unless (or (< search-level 2) exact? (null? finds))
	   (let ([files (case doc-kind
			  [(html) (with-handlers ([not-break? (lambda (x) null)]) 
                                    (map (lambda (x) (build-path doc x)) 
                                         (directory-list doc)))]
			  [(text) (list doc)]
			  [else null])])
	     (for-each
	      (lambda (f)
		(with-handlers ([not-break? (lambda (x)
					      ; (printf "~a~n" (exn-message x))
					      #f)])
		  (with-input-from-file f
		    (lambda ()
		      (let loop ()
			(let ([pos (file-position (current-input-port))]
			      [r (read-line)])
			  (unless (eof-object? r)
			    (let ([m (andmap (lambda (find) (regexp-match find r)) finds)])
			      (when m
				(found "text")
				(add-choice (car m)
					    ; Strip leading space and clean HTML
					    (regexp-replace
					     "^ [ ]*"
					     (if (eq? doc-kind 'html)
						 (clean-html r)
						 r)
					     "")
					    "content"
					    (build-path doc f)
					    (if (eq? doc-kind 'text) pos "NO TAG")
					    ckey)))
			    (loop))))))))
	      files))))
       docs doc-names doc-kinds)
      (if (= 0 hit-count)
	  (apply
	   string-append
	   "Nothing found for "
	   (cond
	    [(null? string-finds) (list "the empty search.")]
	    [else
	     (append
	      (cons (format "\"~a\"" (car string-finds))
		    (map (lambda (i) (format " and \"~a\"" i))
			 (cdr string-finds)))
	      (list "."))]))
	  #f))))
