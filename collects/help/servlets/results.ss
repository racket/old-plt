(require (lib "unitsig.ss")
	 (lib "string.ss")
	 (lib "file.ss")
	 (lib "list.ss")
	 (lib "pregexp.ss")
         (lib "servlet-sig.ss" "web-server")
         (lib "servlet-helpers.ss" "web-server")
         (lib "xml.ss" "xml")
         (lib "search.ss" "help" "private"))

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
      
  (define search-responses #f)
  (define current-kind #f)
  (define last-header #f)

  (define max-reached #f)
  (define (build-maxxed-out k)
    (lambda ()
      (unless max-reached
	      (set! max-reached #t)	    
	      (set! search-responses
		    (cons `(B ,(color-with 
				"red"
				"Search aborted: too many responses"))
			  search-responses)))
    (k #f)))

  (define (add-header s key)
    (unless max-reached
      (set! last-header s)
      (set! search-responses
	    (cons `(B ((STYLE "font-family:Verdana,Helvetica,sans-serif")) 
		      ,s)
		  (cons `(BR)
			search-responses)))))

  (define (set-current-kind! s key)
    (set! current-kind
	  (cadr (assoc s kind-types))))

  (define exp-web-root
    (explode-path 
     (normalize-path 
      (build-path (collection-path "mzlib") 'up))))
  (define web-root-len (length exp-web-root))

  ; given a manual path, convert to absolute Web path
  ; manual path is an anchored path to a collects/doc manual, never a servlet
  (define (tidy-manual-path manual-path)
    (let* ([exp-manual-path (explode-path manual-path)]
	   [exp-tidy-path 
	    (let loop ([path exp-manual-path]
		       [n 0])
	      (if (>= n web-root-len)
		  path
		  (loop (cdr path) (add1 n))))])
      ; insert internal slashes, make absolute by prepending slash
      (string-append "/" (fold-into-web-path exp-tidy-path))))

  (define (keyword-string? ekey)
    (and (string? ekey)
	 (not (string=? ekey ""))))

  (define (pretty-label label keyword?) 
    (if keyword?
	`(FONT ((FACE "monospace"))
	       ,label)
	label))

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

  (define (make-search-link href label src ekey)
    `(TABLE ((CELLSPACING "0")
	     (CELLPADDING "0"))
	    (TR 
	     (TD 
	      (A ((HREF ,href)) ,(pretty-label label (keyword-string? ekey)))
	      " in "
	      "\"" ,src "\""))))

  ; page-label is #f or a string that labels an HTML anchor
  ; path is either an absolute pathname (possibly not normalized) 
  ; in the format of the native OS, or, in the case of Help Desk 
  ; servlets, a forward-slashified path beginning with "/servlets/"
  (define (make-anchored-path page-label path)
    (let ([normal-path 
	   (if (hd-servlet? path) 
	       path
	       (normalize-path path))])
      (if (and page-label
	       (string? page-label)
	       (not (or (string=? page-label "NO TAG") 
			(regexp-match "\\?|&" page-label))))
	  (string-append normal-path "#" page-label)
	  normal-path)))

  (define (doc-txt? url)
    (let ([len (string-length url)])
      (and (> len 8)
	   (string=? 
	    (substring url
		       (- len 7)
		       len)
	    "doc.txt"))))
  
  (define (make-html-href page-label path)
    (let ([anchored-path (make-anchored-path page-label path)])
      (cond
       [(hd-servlet? anchored-path) 
	anchored-path]
       [(doc-txt? anchored-path) ; collection doc.txt
	(let ([maybe-coll (maybe-extract-coll last-header)])
	  (format 
	   no-anchor-format
	   (hexify-string anchored-path)
	   (hexify-string (make-caption maybe-coll))
	   maybe-coll))]
       [else ; manual, so have absolute path
	(let* ([tidy-path (tidy-manual-path anchored-path)]
	       [base-path (path-only (normalize-path path))]
	       [exp-base-path (explode-path base-path)]
	       [doc-dir (car (last-pair exp-base-path))]
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
	    tidy-path
	    (let* ([doc-entry (assoc doc-dir known-docs)]
		   [manual-label (or (and doc-entry (cdr doc-entry)) doc-dir)])
	      (format "/servlets/missing-manual.ss?manual=~a&name=~a"
		      doc-dir (hexify-string manual-label)))))])))
                            
  ; path is absolute pathname
  (define (make-text-href page-label path)
    (let* ([maybe-coll (maybe-extract-coll last-header)]
	   [hex-path (hexify-string (normalize-path path))]
	   [hex-caption (if (eq? maybe-coll last-header)
			hex-path
			(hexify-string (make-caption maybe-coll)))])
      (format 
       with-anchor-format
       hex-path
       hex-caption
       (hexify-string maybe-coll)
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
    (let* ([href (if (html-entry? path)
		     (make-html-href page-label path)
		     (make-text-href page-label path))])
      ; can use refresh here, instead of Javscript here - no semicolon in URL
      (send/finish 
       `(HTML
	 (HEAD
	  (META ((HTTP-EQUIV "refresh")
		 (CONTENT ,(format "0;URL=~a" href))))
	  (TITLE "PLT Help Desk lucky search result"))
	 (BODY
	  "If this page does not refresh, "
	  (A ((HREF ,href)) "click here") ".")))))

  (define (add-entry ekey label src path page-label key)
    (let* ([entry (if (html-entry? path)
		      (make-search-link 
		       (make-html-href page-label path)
		       label src ekey)
		      (make-search-link 
		       (make-text-href page-label path)
		       label src ekey))])
      (set! search-responses
	    (cons entry search-responses))))

  (define (make-results-page items)
    `(HTML
      (HEAD ,hd-css
	    (TITLE "PLT Help Desk search results"))
      (BODY
       (FONT ((SIZE "+1"))
	     ,(color-with "blue" `(B "Search results")))
       (BR)
       ,@items)))

  (define (search-results lucky? search-string search-type match-type)
    (semaphore-wait search-sem)
    (set! search-responses '())
    (set! installed-table (make-hash-table 'weak 'equal))
    (set! max-reached #f)
    (let* ([search-level (search-type->search-level search-type)]
	   [regexp? (string=? match-type "regexp-match")]
	   [exact-match? (string=? match-type "exact-match")]
	   [key (gensym)]
	   [result (let/ec k
		    (do-search search-string 
			       search-level
			       regexp?
			       exact-match?
			       key
			       (build-maxxed-out k)
			       add-header
			       set-current-kind!
			       (if lucky? goto-lucky-entry add-entry)))]
	   [html (make-results-page
		  (if (string? result) ; error message
		      `((H2 ((STYLE "color:red")) ,result))
		      (reverse search-responses)))])
      (semaphore-post search-sem)
      html))

  (define empty-search-page
    `(HTML
      (HEAD
       ,(make-javascript
	 "window.history.back()")
       (TITLE "Empty search string in PLT Help Desk"))
      (BODY
       (H2 "Empty search string")
       (P)
       "Click on the Back button on your browser "
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
