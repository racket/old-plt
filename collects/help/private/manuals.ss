(module manuals mzscheme
  (require (lib "list.ss")
           (lib "file.ss")
           (lib "date.ss")
           (lib "string-constant.ss" "string-constants")
	   (lib "pregexp.ss")
           "colldocs.ss"
           "docpos.ss"
	   (lib "util.ss" "help" "servlets" "private"))

  ; to get CSS style spec
  (require (lib "xml.ss" "xml"))
  (require (lib "hd-css.ss" "help" "servlets" "private"))  

  (provide find-manuals)
 
  (define re:title (regexp "<[tT][iI][tT][lL][eE]>(.*)</[tT][iI][tT][lL][eE]>"))

  (define (find-manuals)
    (let* ([sys-type (system-type)]
	   [cvs-user? (directory-exists? (build-path (collection-path "help") "CVS"))]
           [doc-collection-path (with-handlers ([void (lambda (x) #f)])
                                  (collection-path "doc"))]
           [docs (let loop ([l (if doc-collection-path
                                   (directory-list doc-collection-path)
                                   null)])
                   (cond
                     [(null? l) null]
                     [(file-exists? (build-path doc-collection-path (car l) "index.htm"))
                      (cons (car l) (loop (cdr l)))]
                     [else (loop (cdr l))]))]
	   [compare-docs (lambda (a b)
			   (let ([ap (standard-html-doc-position a)]
				 [bp (standard-html-doc-position b)])
			     (cond
			      [(= ap bp) (string<? a b)]
			      [else (< ap bp)])))]
           [docs (quicksort docs compare-docs)]
           [doc-paths (map (lambda (doc) (string-append "/doc/" doc "/")) docs)]
           [get-name
	    (lambda (d)
	      (let-values ([(_1 short-path _2) (split-path d)])
		(let ([ass (assoc short-path known-docs)])
		  (if ass
		      (cdr ass)
		      (with-input-from-file (build-path d "index.htm")
			(lambda ()
			  (let loop ()
			    (let ([r (read-line)])
			      (cond
			       [(eof-object? r) "(Unknown title)"]
			       [(regexp-match re:title r) => cadr]
			       [(regexp-match "<[tT][iI][tT][lL][eE]>(.*)$" r)
				;; Append lines until we find it 
				(let aloop ([r r])
				  (let ([a (read-line)])
				    (cond
				     [(eof-object? a) (loop)] ; give up
				     [else (let ([r (string-append r a)])
					     (cond
					      [(regexp-match re:title r) => cadr]
					      [else (aloop r)]))])))]
			       [else (loop)])))))))))]
	   [names (map get-name doc-paths)]
	   [names+paths (map cons names doc-paths)]
	   [is-lang? (lambda (name+path)
		       (or (regexp-match "Language" (car name+path))
			   (regexp-match "MrEd" (car name+path))))] ;; hack
	   [lang-names+paths (filter is-lang? names+paths)]
	   [tool-names+paths (filter (lambda (x) (not (is-lang? x))) names+paths)]
	   [lang-names (map car lang-names+paths)]
	   [lang-doc-paths (map cdr lang-names+paths)]
	   [tool-names (map car tool-names+paths)]
	   [tool-doc-paths (map cdr tool-names+paths)]
	   [mk-link (lambda (doc name)
                      (let ([index-file (build-path 
					 (collection-path "doc")
					 (car (last-pair (explode-path doc)))
					 "index.htm")])
                        (format "<LI> <A HREF=\"~a\">~a</A>~a"
			        doc
                                name
                                (if (and cvs-user?
                                         (file-exists? index-file))
                                    (string-append 
                                     "<br>"
                                     (format (string-constant manual-installed-date)
                                             (date->string
                                              (seconds->date
                                               (file-or-directory-modify-seconds
                                                index-file)))))
                                    ""))))]
	   [break-between (lambda (re l)
			    (if (null? l)
				l
				(if (regexp-match re (car l))
				    (let loop ([l l])
				      (cond 
				       [(null? l) null]
				       [(regexp-match re (car l))
					(cons (car l) (loop (cdr l)))]
				       [else (cons "<P>" l)]))
				    l)))])
      (let-values ([(collections-doc-files collection-names) (colldocs)])
        (apply
	 string-append
	 "<html><head>"
	 "<TITLE>Installed Manuals</TITLE>"
	(xexpr->string hd-css)
	 "</head>"
	 "<body>"
	 (append 
	  
	  (list "<H1>Installed Manuals</H1>")
	  
	  (if cvs-user?
	      (list "<b>CVS:</b> <a href=\"/servlets/refresh-manuals.ss\" target=\"outer\">refresh all manuals</a>")
	      '())
	  
	  (list "<H3>Languages</H3>"
		"<UL>")
	  (break-between "Student" (map mk-link lang-doc-paths lang-names))
	  (list "</UL>"
		"<H3>Tools, Libraries, and Extensions</H3>"
		"<UL>")
	  (break-between "DrScheme" (map mk-link tool-doc-paths tool-names))
	  (list "</UL><P><UL>")
	  (map
	   (lambda (collection-doc-file name)
	     (format "<LI> <A HREF=\"/servlets/doc-anchor.ss?file=~a&name=~a&caption=Documentation for the ~a collection\">~a collection</A>"
					; escape colons and other junk
		     (hexify-string
		      (build-path (car collection-doc-file) 
				  (cadr collection-doc-file)))
	             name
		     name
		     name))
	   collections-doc-files
	   collection-names)
	  (list "</UL>")
	  (let ([uninstalled (let loop ([l known-docs])
			       (cond
				[(null? l) null]
				[(member (caar l) docs) (loop (cdr l))]
				[else (cons (car l) (loop (cdr l)))]))])
	    (cond
	     [(null? uninstalled)
	      (list "")]
	     [(not doc-collection-path)
	      (list "<font color=\"red\">"
		    "Please create a doc collection."
		    "You will not be able to install any manuals until you do."
		    "Installing help-doc.plt will create doc collection automaically."
		    "</font>")]
	     [else
	      (list*
	       "<H3>Uninstalled Manuals</H3>"
	       "<UL>"
	       (append
		(map
		 (lambda (doc-pair)
		   (format "<LI> <A HREF=\"/servlets/missing-manual.ss?manual=~a&name=~a\">~a</A>~a"
			   (car doc-pair)
			   (hexify-string (cdr doc-pair))
			   (cdr doc-pair)
			   (if (file-exists? (build-path doc-collection-path (car doc-pair) "hdindex"))
			       " (index installed)"
			       "")))
		 uninstalled)
		(list "</UL>")))]))
	  (list "</body></html>")))))))
