
(let* ([quicksort
	(invoke-unit/sig
	 (compound-unit/sig
	  (import)
	  (link [F : (quicksort) ((require-library "functior.ss"))]
		[I : () ((unit/sig () (import (quicksort)) quicksort) F)])
	  (export)))]
       [docpos (require-library "docpos.ss" "help")]
       [d (collection-path "doc")]
       [docs (let loop ([l (directory-list d)])
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
		(let loop ([collection-paths (current-library-collection-paths)]
			   [docs null]
			   [names null])
		  (cond
		   [(null? collection-paths)
		    (let ([sorted
			   (quicksort (map cons docs names)
				      (lambda (a b)
					(string<? (cdr a) (cdr b))))])
		      (values (map car sorted) (map cdr sorted)))]
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
			      [else (cloop (cdr l) docs names)])))]))])
    
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
      (list "</UL>")))))