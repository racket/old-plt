(unit/sig ()
  (import (argv)
	  mzlib:command-line^
	  help:search^)
  
  (define html-prefix "http://www.cs.rice.edu/CS/PLT/unreleased/")
  (define collects-prefix "http://www.cs.rice.edu/~robby/plt/collects/")

  (define re:html (regexp "^.*plt/collects/doc/(.*)$"))
  (define re:collects (regexp "^.*plt/collects/(.*)$"))

  (define found-something? #f)

  (define (add-doc-section name ckey)
    (printf "<h3>~a</h3>~n" name))
  (define (add-kind-section name ckey)
    (void))

  (define (build-url page label)
    (let ([join
	   (lambda (prefix after label)
	     (if (and label
		      (not (string=? label "")))
		 (format "~a~a#~a" prefix after label)
		 (format "~a~a" prefix after)))])
      (cond
	[(regexp-match re:html page)
	 =>
	 (lambda (m) (join html-prefix (cadr m) label))]
	[(regexp-match re:collects page)
	 =>
	 (lambda (m) (join collects-prefix (cadr m) #f))]
	[else page])))

  (define (find-start key name)
    (let ([l (string-length key)])
      (let loop ([n 0])
	(if (string=? key (substring name n (+ n l)))
	    n
	    (loop (add1 n))))))

  (define (add-choice key name title page label ckey)
    (unless found-something?
      (set! found-something? #t)
      (printf "<!-- RESULT LIST START -->"))
    (let ([bold-name
	   (if (string=? "" key)
	       name
	       (let* ([start (find-start key name)]
		      [end (+ start (string-length key))])
		 (format "~a<b>~a</b>~a"
			 (substring name 0 start)
			 key
			 (substring name end (string-length name)))))])

      (printf "<!-- RESULT ITEM START -->~n")
      (printf "<tt><a href=\"~a\">~a</a></tt> in ~a<br>~n"
	      (build-url page label)
	      bold-name
	      title)
      (printf "<!-- RESULT ITEM END -->~n")))

  (define (output . x)
    (let ([s (apply format x)])
      (display s)
      (newline)))

  (define regexp? #f)
  (define exact? #f)
  (define search-level 1)
  (define given-find (begin "set-alignment" "help desk"))

  (define table
    `((once-any
       [("--regexp" "-r")
	,(lambda (flg) (set! regexp? #t))
	("Use regular expression matching")]
       [("--exact" "-e")
	,(lambda (flg) (set! exact? #t))
	("Use exact matching")])
      (once-each
       [("--search-level" "-s")
	,(lambda (flg x)
	   (cond
	     [(string->number x) => (lambda (n) (set! search-level n))]
	     [else (error 'raw-help-desk "--search-level: not a number: ~a" x)]))
	("Set the searching level to <level>" "level")])))

  (define (dump-out-file filename)
    (call-with-input-file filename
      (lambda (port)
	(let loop ()
	  (let ([l (read-line port)])
	    (unless (eof-object? l)
	      (display l)
	      (newline)
	      (loop)))))))

  (parse-command-line
   "raw-help-desk"
   argv
   table
   (lambda (accum search-string)
     (set! given-find search-string))
   '("search string"))

  (printf "<html>~n")
  (printf "<head>~n")
  (printf "<!-- BANNER START -->~n")
  (printf "<h2><a target=_top href=\"http://www.cs.rice.edu/CS/PLT\"><img align=center border=0 src=\"http://www.cs.rice.edu/CS/PLT/pltlogo.gif\">PLT</a> search results</h2>~n")
  (printf "<!-- BANNER END -->~n")
  (printf "</head>~n")
  (dump-out-file "/net/www1/htdocs/CS/PLT/Formats/body-tag.shtml") ; (printf "<body>~n")

  (let/ec k
    (let ([err-msg (do-search
		    given-find
		    search-level
		    regexp?
		    exact?
		    (gensym)
		    (lambda ()
		      (output "(maximum searches reached)")
		      (k (void)))
		    add-doc-section
		    add-kind-section
		    add-choice)])
      (cond
       [err-msg
	(display err-msg)
	(newline)]
       [else (printf "<!-- RESULT LIST END -->")])))

  (printf "</body>~n")
  (printf "</html>~n"))
