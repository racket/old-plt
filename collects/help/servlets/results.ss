(require (lib "unitsig.ss")
	 (lib "string.ss")
	 (lib "file.ss")
	 (lib "pregexp.ss")
         (lib "servlet-sig.ss" "web-server")
         (lib "servlet-helpers.ss" "web-server")
         (lib "xml.ss" "xml")
	 (lib "help-desk.ss" "help"))

(require "private/util.ss")
(require "private/search-util.ss")
(require "private/hd-css.ss")

(unit/sig ()
  (import servlet^)

  ; doc subcollection name -> boolean
  (define installed-table #f)

  (define search-sem (make-semaphore 1))

  (define (search-type->search-level st)
    (let loop ([n 0]
	       [lst (map car search-types)])
      (when (null? lst)
	    (raise 'bad-search-type))
      (if (string=? (car lst) st)
	  n
	  (loop (add1 n) (cdr lst)))))
      
  (define search-response #f)
  (define current-kind #f)
  (define last-header #f)

  (define (maxxed-out)
    (set! search-response
	  `((DIV ()
		(H2 ((STYLE "color:red"))
		    "Too many search results")
		(H3 ((STYLE "color:red")) 
		    "Narrow your search and try again"))))
    (raise 'maxxed-out))

  (define (add-header s key)
    (set! last-header s)
    (set! search-response
	  (cons `(B ((STYLE "font-family:Verdana,Helvetica,sans-serif")) 
		     ,s) 
		(cons `(BR)
		      search-response))))

  (define (set-current-kind! s key)
    (set! current-kind
	  (cadr (assoc s kind-types))))

  (define windows? (eq? (system-type) 'windows))

  (define (windowize s)
   (if windows?
       (pregexp-replace* "\\\\" s "/")
       s))

  (define web-root
    (let ([s (simplify-path (build-path (collection-path "mzlib") 'up))])
      (string-lowercase! s)
      s))

  (define web-root-len (string-length web-root))

  (define servlet-path
    (build-path (collection-path "doc") "help" "servlets"))	

  (define (tidy-html-url url)
    (let* ([url-tail (substring url web-root-len (string-length url))]
	   [tidy-tail (string-append "/" (windowize url-tail))])
      (or (pregexp-replace ; server uses directory to load as a servlet
	   "^/doc/help/servlets/"
	   tidy-tail
	   "/servlets/")
	  tidy-tail)))

  (define (pretty-label label) ; TO DO, maybe
	label)

  (define (maybe-extract-coll s)
    (let ([len (string-length s)])
      (if (and (> len 17)
	       (string=? (substring s 0 4) "the ")
	       (string=? (substring s (- len 11) len)
			 " collection"))
	  (substring s 4 (- len 11))
	  s)))

  (define no-anchor-format 
    (string-append
     "/servlets/doc-anchor.ss?"
     "file=~a&"
     "caption=~a&"
     "name=~a"))

  (define with-anchor-format 
    (string-append no-anchor-format "&offset=~a#temp"))

  (define (make-caption coll)
    (format "Documentation for the ~a collection" coll))

  (define (make-search-link href label src)
    `(TABLE ((CELLSPACING "0")
	     (CELLPADDING "0"))
	    (TR ()
		(TD ()
		    (A ((HREF ,href)) ,(pretty-label label)) " in "
		    "\"" ,src "\""))))


  (define (make-entry-url page-label path)
    (if (and page-label
	     (string? page-label)
	     (not (string=? page-label "?")))
	(string-append path "#" page-label)
	path))

  (define (doc-txt? url)
    (let ([len (string-length url)])
      (and (> len 8)
	   (string=? 
	    (substring url
		       (- len 7)
		       len)
	    "doc.txt"))))
  
  (define (servlet? url)
    (regexp-match "^/doc/help/../../servlets/" url))

  (define (make-html-href url)
    (let ([tidy-url (tidy-html-url url)])
      (cond
       [(servlet? tidy-url)
	tidy-url]
       [(doc-txt? url)
	(let ([maybe-coll (maybe-extract-coll last-header)])
	  (format 
	   no-anchor-format
	   (hexify-string url)
	   (hexify-string (make-caption maybe-coll))
	   maybe-coll))]
       [else ; manual
	(let* ([tidy-url (tidy-html-url url)]
	       [exploded-url (explode-path tidy-url)]
	       [doc-dir (caddr exploded-url)]
	       [full-doc-dir (build-path (collection-path "doc") doc-dir)]
	       [installed? (hash-table-get installed-table
					   doc-dir
					   (lambda () 'installed-unknown))])
	  (when (eq? installed? 'installed-unknown)
		(let ([has-index?
		       (ormap file-exists? 
			      (map (lambda (s)
				     (build-path full-doc-dir s))
				   '("index.htm" "index.html")))])
		  (hash-table-put! installed-table doc-dir has-index?)
		  (set! installed? has-index?)))
	  (if installed?
	      tidy-url
	      (let* ([doc-entry (assoc doc-dir known-docs)]
		     [manual-label (or (and doc-entry (cdr doc-entry)) doc-dir)])
		(format "/servlets/missing-manual.ss?manual=~a&name=~a"
			(hexify-string manual-label) doc-dir))))])))
                            

  (define (make-text-href url page-label)
    (let ([maybe-coll (maybe-extract-coll last-header)])
      (format 
       with-anchor-format
       (hexify-string url)
       (hexify-string (make-caption maybe-coll))
       maybe-coll
       (or (and (number? page-label)
		page-label)
	   0))))

  (define (html-entry? path)
    (or (eq? current-kind 'html)
	(let ([path-len (string-length path)])
	  (string=? (substring path 
			       (- path-len 5)
			       path-len)
		    ".html"))))

  (define (goto-lucky-entry ekey label src path page-label key)
    (let* ([url (make-entry-url page-label path)]
	   [href (if (html-entry? path)
		     (make-html-href url)
		     (make-text-href url page-label))])
      ; can use refresh here, instead of Javscript here - no semicolon in URL
      (send/finish 
       `(HTML
	 (HEAD
	  (META ((HTTP-EQUIV "refresh")
		 (CONTENT ,(format "0;URL=~a" href)))))
	 (BODY
	  "If this page does not refresh, "
	  (A ((HREF ,href)) "click here") ".")))))

  (define (add-entry ekey label src path page-label key)
    (let* ([url (make-entry-url page-label path)]
	   [entry (if (html-entry? path)
		      (make-search-link 
		       (make-html-href url)
		       label src)
		      (make-search-link 
		       (make-text-href url page-label)
		       label src))])
      (set! search-response
	    (cons entry search-response))))

  (define (search-results lucky? search-string search-type match-type)
    (semaphore-wait search-sem)
    (set! search-response '())
    (set! installed-table (make-hash-table 'weak 'equal))
    (let* ([search-level (search-type->search-level search-type)]
	   [regexp? (string=? match-type "regexp-match")]
	   [exact-match? (string=? match-type "exact-match")]
	   [key (gensym)]
	   [result (with-handlers
		    (((lambda (exn) (eq? exn 'maxxed-out))
		      (lambda (exn) #f)))
		    (do-search search-string 
			       search-level
			       regexp?
			       exact-match?
			       key
			       maxxed-out
			       add-header
			       set-current-kind!
			       (if lucky? goto-lucky-entry add-entry)))]
	   [html `(HTML
		   (HEAD ,hd-css)
		   (BODY
		    (FONT ((SIZE "+2")) 
			  (B "Search results"))
		    (BR)
		    ,@(if (string? result)
			  `((H2 ((STYLE "color:red")) ,result))
			  (reverse search-response))))])
      (semaphore-post search-sem)
      html))

  (define empty-search-page
    `(HTML
      (HEAD
       ,(make-javascript
	 "window.history.back()"))
      (BODY
       (H2 "Empty search string")
       (P)
       "Click on the back button on your browser "
       "to return to the previous page.")))
  
  (define (lucky-search? bindings)
    (with-handlers
     ([void (lambda _ #f)])
     (let ([result (extract-binding/single 'lucky bindings)])
       (not (string=? result "false")))))
    
  (let* ([bindings (request-bindings initial-request)]
	 [binding-vals (map (lambda (sym)
			      (with-handlers
			       ([void (lambda _ "")])
			       (extract-binding/single sym bindings)))
			    '(search-string search-type match-type))])
    (if (= (string-length (car binding-vals)) 0)
	empty-search-page
	(apply search-results 
	       (lucky-search? bindings)
	       (map (lambda (sym)
		      (with-handlers
		       ([void (lambda _ "")])
		       (extract-binding/single sym bindings)))
		    '(search-string search-type match-type))))))
