(module link-check mzscheme

  (require (lib "class.ss"))
  (require (lib "mysterx.ss" "mysterx"))
  (require (lib "help-desk-mz.ss" "help"))

  (provide run-link-checker)

  (define hd-cookie (start-help-server))
  (define hd-port (hd-cookie->port hd-cookie))

  (define servlet-format 
    (string-append (format "http://127.0.0.1:~a/servlets/"
			   hd-port)
		   "~a"))

  (define doc-prefix
    (format "http://127.0.0.1:~a/doc/" hd-port))
  (define doc-prefix-len
    (string-length doc-prefix))

  (define (start-help-browser) 
    (instantiate mx-browser% 
		 () ; no by-position initializers
		 (label "Help Desk")
		 (height 600)
		 (width 800)
		 (y 'default)
		 (x 'default)
		 (style-options '(scrollbars))))

  (define dont-follow-servlets
    '("bug-report.ss"
      "toggle-frames.ss"
      "refresh-manuals.ss"))

  (define dont-follow-urls
    '("http://www.cs.rice.edu/cgi-bin/finger;xyz?name=shriram&host=nw#top"
      "http://www.plt-scheme.org/servlets/my-servlet"
      "http://www.plt-scheme.org/servlets/my-servlet/extra"
      "http://www.plt-scheme.org/servlets/my-servlet/extra/directories"
      "http://my-host/conf/refresh-passwords"
      "http://my-host/conf/refresh-servlets"))

  (define already-seen-table (make-hash-table 'equal))
  (define dont-follow-table (make-hash-table 'equal))
  (define hash-failure (lambda _ #f))

  (define the-void-value (void))

  (for-each
   (lambda (s)
     (hash-table-put! dont-follow-table
		      (format servlet-format s) the-void-value))
   dont-follow-servlets)

  (for-each
   (lambda (s)
     (hash-table-put! dont-follow-table s the-void-value))
   dont-follow-urls)

  (define skip-regexps
    '("missing-manual.ss" "refresh" 
      "download-manual.ss"
      "http://download.plt-scheme.org/patches/"))

  ; url's we want to avoid
  ;  manuals
  ;  missing manuals
  ;  non-http schemes
  (define (doc-or-worse? url)
    (let ([len (string-length url)])
      (or (< len 17)
	  (not (string=? "http://" (substring url 0 7)))
	  (ormap
	   (lambda (s)
	     (regexp-match s url))
	   skip-regexps)
	  (and (>= len doc-prefix-len)
	       (string=? doc-prefix
			 (substring url 0 doc-prefix-len))))))

  (define error-count 0)

  (define (check-links curr-url browser)
    (printf "Checking URL ~a~n" curr-url)
    (let* ([results (send browser navigate/status curr-url)]
	   [code (cadr results)])
      (unless (eq? code 200)
	      (set! error-count (add1 error-count))
	      (fprintf (current-error-port)
		       "Navigation failed~nURL: ~a  Code: ~a~n"
		       curr-url code))
      ; if offsite, don't look for links
      (when (string=? "http://127.0.0.1:"
			(substring curr-url 0 17))
        (let* ([curr-doc (send browser current-document)]
	       [anchors (send curr-doc elements-with-tag "A")]
	       [hrefs (map (lambda (a)
			     (send a attribute "HREF"))
			   anchors)])
	  (for-each
	   (lambda (url)
	     (unless (or (doc-or-worse? url)
			 (hash-table-get already-seen-table url hash-failure)
			 (hash-table-get dont-follow-table url hash-failure))
		     (hash-table-put! already-seen-table url the-void-value)
		     (check-links url browser)))
	   hrefs)))))

  (define (run-link-checker)
    (let ([b (start-help-browser)]
	  [home-url (format servlet-format "home.ss")])
      (check-links home-url b)
      (if (> error-count 0)
	  (fprintf (current-error-port)
		   "There were ~a navigation errors~n" error-count)
	  (printf "There were no navigation errors~n")))))








