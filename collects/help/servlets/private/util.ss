(module util mzscheme
  (require (lib "file.ss")
	   (lib "list.ss")
	   (lib "xml.ss" "xml")
	   (lib "finddoc.ss" "help" "private")
	   (lib "string-constant.ss" "string-constants")
	   (lib "docpos.ss" "help" "private"))

  (provide get-pref/default
	   get-bool-pref/default
	   put-prefs
	   cvs?
	   search-height-default
	   search-bg-default
	   search-text-default
	   search-link-default
	   color-highlight
	   color-with
	   hexify-string
	   main-manual-page
	   manual-entry
	   collection-doc-link
	   fold-into-web-path
	   home-page
	   format-collection-message
	   nl
           plt-version
	   make-javascript
	   redir-javascript
	   onload-redir)

  ;; would be nice if this could use version:version from the framework.
  (define (plt-version)
    (let ([mz-version (version)]
          [stamp-collection
           (with-handlers ([not-break-exn? (lambda (exn) #f)])
             (collection-path "cvs-time-stamp"))])
      (if (and stamp-collection (file-exists? (build-path stamp-collection "stamp.ss")))
          (format "~a-cvs~a" mz-version (dynamic-require '(lib "stamp.ss" "cvs-time-stamp") 'stamp))
          mz-version)))
  
  
  (define home-page 
    `(A ((HREF "/servlets/home.ss") (TARGET "_top"))
	,(string-constant plt:hd:home)))

  (define (get-pref/default pref default)
    (get-preference pref (lambda () default)))

  (define (get-bool-pref/default pref default)
    (let ([raw-pref (get-pref/default pref default)])
      (if (string=? raw-pref "false") #f #t)))

  (define (put-prefs names vals)
    (put-preferences names vals)) 

  (define search-height-default "85")
  (define search-bg-default "lightsteelblue")
  (define search-text-default "black")
  (define search-link-default "darkblue")

  (define *the-highlight-color* "forestgreen")

  ; string xexpr ... -> xexpr
  (define (color-with color . s)
    `(FONT ((COLOR ,color)) ,@s))

  ; xexpr ... -> xexpr
  (define (color-highlight . s)
    (apply color-with *the-highlight-color* s))

  (define (cvs?)
    (directory-exists? 
     (build-path (collection-path "help") "CVS")))

  ; manual is doc collection subdirectory, e.g. "mred"
  (define (main-manual-page manual)
    (let* ([entry (assoc manual known-docs)]
	   [name (or (and entry (cdr entry))
		      manual)]
	   [main-page (build-path (collection-path "doc")
				  manual
				  "index.htm")]
	   [href (if (file-exists? main-page)
		     (string-append "/doc/" manual "/")
		     (string-append "/servlets/missing-manual.ss?"
				    "manual=" manual "&"
				    "name="
				    (hexify-string name)))])
      `(A ((HREF ,href)) ,name)))

  ; string string string -> xexpr
  ; man is manual name
  ; ndx is index into the manual
  ; txt is the link text
  (define (manual-entry man ndx txt)
    `(A ((HREF 
	  ,(with-handlers
	    ([void (lambda _
		     (format "/servlets/missing-manual.ss?manual=~a&name=~a"
			     man 
			     (hexify-string
			      (let ([pr (assoc man known-docs)])
				(or (and pr
					 (cdr pr))
				    man)))))])
	    (finddoc-page man ndx))))
	,txt))

  (define hexifiable '(#\: #\; #\? #\& #\% #\# #\< #\>))
  ; string -> string
  (define (hexify-string s)
    (apply string-append 
	   (map (lambda (c) 
		  (if (or (char-whitespace? c) (memq c hexifiable))
		      (format "%~X" (char->integer c))
		      (format "~a" c)))
		(string->list s))))

  ; string string -> xexpr
  (define (collection-doc-link coll txt)
    (let ([coll-file (build-path 
		      (collection-path coll) "doc.txt")])
      (if (file-exists? coll-file)
	  `(A ((HREF 
		,(format 
		  "/servlets/doc-anchor.ss?file=~a&name=~a&caption=Documentation for the ~a collection"
		  (hexify-string coll-file)
		  coll
		  coll)))
	      ,txt)
	  "")))

  ; (listof string) -> string
  ; result is forward-slashed web path
  ;  e.g. ("foo" "bar") -> "foo/bar"
  (define (fold-into-web-path lst)
      (foldr (lambda (s a)
	       (if a
		   (string-append s "/" a)
		   s))
	     #f
	     lst))
  
  ;; ??
  ;(define (text-frame) "_top")

  (define (format-collection-message s)
    `(B ((STYLE "color:green")) ,s))

  (define nl (string #\newline))

  (define (make-javascript . ss)
    `(SCRIPT ((LANGUAGE "Javascript"))
	     ,(make-comment
	       (apply string-append 
		      nl
		      (map (lambda (s)
			     (string-append s nl))
			   ss)))))

  (define (redir-javascript k-url)
    (make-javascript
     "function redir() {"
     (string-append
       "  document.location.href=\"" k-url "\"") 
     "}"))

  (define (onload-redir secs)
    (string-append 
     "setTimeout(\"redir()\","
     (number->string (* secs 1000))
     ")")))





