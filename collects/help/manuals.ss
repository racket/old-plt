
(let* ([d (collection-path "doc")]
       [docs (let loop ([l (directory-list d)])
	       (cond
		[(null? l) null]
		[(file-exists? (build-path d (car l) "index.htm"))
		 (cons (build-path d (car l))
		       (loop (cdr l)))]
		[else (loop (cdr l))]))]
       [names
	(map
	 (lambda (d)
	   (with-input-from-file (build-path d "index.htm")
	     (lambda ()
	       (let loop ()
		 (let ([r (read-line)])
		   (cond
		    [(eof-object? r) "(Unknown title)"]
		    [(regexp-match "<TITLE>(.*)</TITLE>" r) => cadr]
		    [else (loop)]))))))
	 docs)])
  (apply
   string-append
   "<TITLE>Installed Manuals</TITLE>"
   "<H1>Installed Manuals</H1>"
   "<UL>"
   (append
    (map
     (lambda (doc name)
       (format "<LI> <A HREF=\"file:~a\">~a</A>"
	       (build-path doc "index.htm")
	       name))
     docs
     names)
    (list "</UL>"))))

