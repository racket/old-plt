
(lambda ()
  (let* ([quicksort
	  (invoke-unit/sig
	   (compound-unit/sig
	    (import)
	    (link [F : (quicksort) ((require-library "functior.ss"))]
		  [I : () ((unit/sig () (import (quicksort)) quicksort) F)])
	    (export)))]
	 [docpos (car (require-library "docpos.ss" "help"))]
	 [known-docs (cdr (require-library "docpos.ss" "help"))]
	 [d (with-handlers ([void (lambda (x) #f)])
	      (collection-path "doc"))]
	 [docs (let loop ([l (if d
				 (directory-list d)
				 null)])
		 (cond
		  [(null? l) null]
		  [(file-exists? (build-path d (car l) "index.htm"))
		   (cons (car l) (loop (cdr l)))]
		  [else (loop (cdr l))]))]
	 [docs (quicksort docs (lambda (a b)
				 (let ([ap (docpos a)]
				       [bp (docpos b)])
				   (cond
				    [(= ap bp) (string<? a b)]
				    [else (< ap bp)]))))]
	 [doc-paths (map (lambda (doc) (build-path d doc)) docs)]
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
	   doc-paths)])
    (let-values ([(collections collection-names)
		  ((require-library "colldocs.ss" "help") quicksort)])

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
	 doc-paths
	 names)
	(list "</UL><P><UL>")
	(map
	 (lambda (collection name)
	   (format "<LI> <A HREF=\"file:~a\">~a collection</A>"
		   (build-path collection "doc.txt")
		   name))
	 collections
	 collection-names)
	(list "</UL>")
	(let ([uninstalled (let loop ([l known-docs])
			     (cond
			      [(null? l) null]
			      [(member (caar l) docs) (loop (cdr l))]
			      [else (cons (car l) (loop (cdr l)))]))])
	  (if (null? uninstalled)
	      (list "")
	      (list*
	       "<H3>Uninstalled Manuals</H3>"
	       "<UL>"
	       (append
		(map
		 (lambda (doc-pair)
		   (format "<LI> <A HREF=\"file:~a\">~a</A>~a"
			   (build-path d (car doc-pair) "index.htm")
			   (cdr doc-pair)
			   (if (file-exists? (build-path d (car doc-pair) "hdindex"))
			       " (index installed)"
			       "")))
		 uninstalled)
		(list "</UL>"))))))))))
