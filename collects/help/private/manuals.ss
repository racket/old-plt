
(module manuals mzscheme
  (require (lib "list.ss")
           (lib "date.ss")
           (lib "string-constant.ss" "string-constants")
	   (lib "xml.ss" "xml")
           (lib "contract.ss")
           "colldocs.ss"
           "docpos.ss"
           "path.ss"
           "../servlets/private/util.ss"
           "../servlets/private/headelts.ss")

  (provide main-manual-page
	   manual-entry)
  (provide finddoc
	   findreldoc
           finddoc-page
	   finddoc-page-anchor)
  
  (provide/contract [find-doc-directories (-> (listof string?))]
                    [find-doc-directory (string? . -> . (union false? string?))]
                    [find-doc-names (-> (listof (cons/p string? string?)))])
  
  (provide find-manuals)

  
  ;; Creates a "file:" link into the indicated manual.
  ;; The link doesn't go to a particular anchor,
  ;; because "file:" does not support that.
  (define (finddoc manual index-key label)
    (let ([m (finddoc-lookup manual index-key label)])
      (if (string? m)
          m
          (format "<A href=\"file:~a\">~a</A>"
                  (build-path (car m) (caddr m))
                    label))))

  ;; Given a Unix-style relative path to reach the "doc"
  ;; collection, creates a link that can go to a
  ;; particular anchor.
  (define (findreldoc todocs manual index-key label)
    (let ([m (finddoc-lookup manual index-key label)])
      (if (string? m)
          m
          (format "<A href=\"~a/~a/~a#~a\">~a</A>"
                  todocs
                  manual
                  (caddr m)
                  (cadddr m)
                  label))))

  (define (finddoc-page-help manual index-key anchor?)
    (let ([m (finddoc-lookup manual index-key "dummy")])
      (if (string? m)
          (error (format "Error finding index \"~a\" in manual \"~a\""
                         index-key manual))
          (let ([path (if anchor?
                          (string-append (caddr m) "#" (cadddr m))
                          (caddr m))])
            (if (servlet-path? path)
                path
                (format "/doc/~a/~a" manual path))))))
  
  ; finddoc-page : string string -> string
  ; returns path for use by PLT Web server
  ;  path is of form /doc/manual/page, or
  ;  /servlet/<rest-of-path>
  (define (finddoc-page manual index-key)
    (finddoc-page-help manual index-key #f))

  ; finddoc-page-anchor : string string -> string
  ; returns path (with anchor) for use by PLT Web server
  ;  path is of form /doc/manual/page#anchor, or
  ;  /servlet/<rest-of-path>#anchor
  (define (finddoc-page-anchor manual index-key)
    (finddoc-page-help manual index-key #t))

  ;; returns either a string (failure) or
  ;; (list docdir index-key filename anchor title)
  (define finddoc-ht (make-hash-table))
  (define (finddoc-lookup manual index-key label)
    (let ([key (string->symbol manual)]
	  [docdir (find-doc-directory manual)])
      (let ([l (hash-table-get
		finddoc-ht
		key
		(lambda ()
		  (let ([f (build-path docdir "hdindex")])
                    (if (file-exists? f)
                        (let ([l (with-input-from-file f read)])
                          (hash-table-put! finddoc-ht key l)
                          l)
                        (error 'finddoc "manual index ~s not installed" manual)))))])
	(let ([m (assoc index-key l)])
	  (if m 
	      (cons docdir m)
	      (error 'finddoc "index key ~s not found in manual ~s" index-key manual))))))
  
  ; manual is doc collection subdirectory, e.g. "mred"
  (define (main-manual-page manual)
    (let* ([entry (assoc manual known-docs)]
	   [name (or (and entry (cdr entry))
                     manual)]
	   [href (string-append "/doc/" manual "/")])
      `(A ((HREF ,href)) ,name)))
  
  ; string string string -> xexpr
  ; man is manual name
  ; ndx is index into the manual
  ; txt is the link text
  ;; warning: if the index file isn't present, this page
  (define (manual-entry man ndx txt)
    (with-handlers ([not-break-exn?
                     (lambda (x)
                       `(font ((color "red"))
                              ,txt
                              " ["
                              ,(if (exn? x)
                                   (exn-message x)
                                   (format "~s" x))
                              "]"))])
      `(A ((HREF ,(finddoc-page man ndx))) ,txt)))
  
  (define (find-doc-names)
    (let* ([dirs (find-doc-directories)]
           [installed
            (map (lambda (dir)
                   (let-values ([(base name dir?) (split-path dir)])
                     name))
                 dirs)]
           [uninstalled
            (filter (lambda (x) (not (member (car x) installed)))
                    known-docs)])
      (append
       (map (lambda (short-name long-name) (cons short-name (get-doc-name long-name)))
            installed 
            dirs)
       uninstalled)))
  
  ;; find-doc-directories : -> (listof string[dir-path])
  ;; constructs a list of directories where documentation may reside.
  ;; it is the contents of all of the "doc" collections
  (define (find-doc-directories)
    (let loop ([paths (current-library-collection-paths)]
               [acc null])
      (cond
        [(null? paths) acc]
        [else (let* ([path (car paths)]
                     [doc-path (build-path path "doc")])
                (if (directory-exists? doc-path)
                    (let dloop ([doc-contents (directory-list doc-path)]
                                [acc acc])
                      (cond
                        [(null? doc-contents) (loop (cdr paths) acc)]
                        [else 
                         (let ([candidate (build-path doc-path (car doc-contents))])
                           (if (directory-exists? candidate)
                               (dloop (cdr doc-contents) (cons candidate acc))
                               (dloop (cdr doc-contents) acc)))]))
                    (loop (cdr paths) acc)))])))
  
  ;; find-doc-directory : string[doc-collection-name] -> (union #f string[dir-name])
  ;; finds the full path of the doc directory, if one exists
  (define (find-doc-directory doc)
    (let loop ([dirs (find-doc-directories)])
      (cond
        [(null? dirs) #f]
        [else (let ([dir (car dirs)])
                (let-values ([(base name dir?) (split-path dir)])
                  (if (equal? name doc)
                      dir
                      (loop (cdr dirs)))))])))
                            
  
  (define re:title (regexp "<[tT][iI][tT][lL][eE]>(.*)</[tT][iI][tT][lL][eE]>"))

  (define (find-manuals)
    (let* ([sys-type (system-type)]
	   [cvs-user? (cvs?)]
           [docs (let loop ([l (find-doc-directories)])
                   (cond
                     [(null? l) null]
                     [(get-index-file (car l))
                      (cons (car l) (loop (cdr l)))]
                     [else (loop (cdr l))]))]
	   [compare-docs (lambda (a b)
			   (let ([ap (standard-html-doc-position a)]
				 [bp (standard-html-doc-position b)])
			     (cond
			      [(= ap bp) (string<? a b)]
			      [else (< ap bp)])))]
           [docs (quicksort docs compare-docs)]
           [names (map get-doc-name docs)]
	   [names+paths (map cons names docs)]
	   [is-lang? (lambda (name+path)
		       (let ([name (car name+path)])
			 (ormap (lambda (s) (regexp-match s name))
				'(#rx"Language" #rx"MrEd"))))]
	   [lang-names+paths (filter is-lang? names+paths)]
	   [tool-names+paths (filter (lambda (x) (not (is-lang? x))) names+paths)]
	   [lang-names (map car lang-names+paths)]
	   [lang-doc-paths (map cdr lang-names+paths)]
	   [tool-names (map car tool-names+paths)]
	   [tool-doc-paths (map cdr tool-names+paths)]
	   [mk-link (lambda (doc-path name)
                      (let* ([manual-name (let-values ([(base manual-name dir?) (split-path doc-path)])
                                            manual-name)]
			     [index-file (get-index-file doc-path)])
                        (format "<LI> <A HREF=\"/doc/~a/~a\">~a</A>~a"
			        manual-name
                                index-file
                                name
                                (if (and cvs-user?
                                         (file-exists? (build-path doc-path index-file)))
                                    (string-append 
                                     "<BR>&nbsp;&nbsp;"
				     "<FONT SIZE=\"-1\">"
				     (if (is-known-doc? doc-path)
					 (string-append
					  (format 
					   "[<A mzscheme=\"((dynamic-require '(lib |refresh-manuals.ss| |help|) 'refresh-manuals) (list (cons |~a| |~a|)))\">~a</A>]"
					   manual-name
					   name
                                           (string-constant plt:hd:refresh))
					  "&nbsp;")
					 "")
				     (format (string-constant plt:hd:manual-installed-date)
                                             (date->string
                                              (seconds->date
                                               (file-or-directory-modify-seconds
                                                (build-path doc-path index-file)))))
				     "</FONT>")
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
	 "<html>"
         (xexpr->string 
	  `(HEAD
	    ,hd-css
	    ,@hd-links
	    (TITLE "PLT Manuals")))
	 "<body>"
	 (append 
	  
	  (list "<H1>Installed Manuals</H1>")
	  
	  (if cvs-user?
	      (list "<b>CVS:</b> <a mzscheme=\"((dynamic-require '(lib |refresh-manuals.ss| |help|) 'refresh-manuals))\">"
		    (string-constant plt:hd:refresh-all-manuals)
		    "</a>")
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
	  (let ([uninstalled (get-uninstalled docs)])
	    (cond
	     [(null? uninstalled)
	      (list "")]
	     [else
	      (list*
	       "<H3>Uninstalled Manuals</H3>"
	       "<UL>"
	       (append
		(map
		 (lambda (doc-pair)
		   (let* ([manual (car doc-pair)]
                          [name (cdr doc-pair)]
                          [manual-path (find-doc-directory manual)])
                     (format "<LI> Download and install <A mzscheme=\"((dynamic-require '(lib |refresh-manuals.ss| |help|) 'refresh-manuals) (list (cons |~a| |~a|)))\">~a</A>~a"
                             manual
                             name
                             name
                             (if (and manual-path
                                      (or (file-exists? (build-path manual-path "hdindex"))
                                          (file-exists? (build-path manual-path "keywords"))))
                                 " (index installed)"
                                 ""))))
		 uninstalled)
		(list "</UL>")))]))
	  (list "</body></html>"))))))
  
  (define cached-doc-names (make-hash-table 'equal))
  (define (get-doc-name doc-dir)
    (hash-table-get
     cached-doc-names
     doc-dir
     (lambda ()
       (let ([res (compute-doc-name doc-dir)])
         (hash-table-put! cached-doc-names doc-dir res)
         res))))
  
  (define (compute-doc-name doc-dir)
    (let ([unknown-title (let-values ([(base name dir?) (split-path doc-dir)]) name)])
      (or (get-known-doc-name doc-dir)
          (let ([main-file (get-index-file doc-dir)])
            (if main-file
                (with-input-from-file (build-path doc-dir main-file)
                  (lambda ()
                    (let loop ()
                      (let ([r (read-line)])
                        (cond
                          [(eof-object? r) unknown-title]
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
                          [else (loop)])))))
                unknown-title)))))
  
  ;; is-known-doc? : string[path] -> boolean
  (define (is-known-doc? doc-path)
    (let-values ([(base name dir?) (split-path doc-path)])
      (if (assoc name known-docs)
          #t
          #f)))
  
  ;; get-known-doc-name : string[full-path] -> (union string #f)
  (define (get-known-doc-name doc-path)
    (let-values ([(base name dir?) (split-path doc-path)])
      (let ([ass (assoc name known-docs)])
        (if ass
            (cdr ass)
            #f))))

  ;; get-uninstalled : (listof string[full-path]) -> (listof (cons string[full-path] string[docs-name]))
  (define (get-uninstalled docs)
    (let ([ht (make-hash-table)])
      (for-each (lambda (known-doc)
                  (hash-table-put! ht 
                                   (string->symbol (car known-doc))
                                   (cdr known-doc)))
                known-docs)
      (for-each (lambda (doc)
                  (let-values ([(base name dir?) (split-path doc)])
                    (hash-table-remove! ht (string->symbol name))))
                docs)
      (hash-table-map ht (lambda (k v) (cons (symbol->string k) v)))))
  
  ;; get-index-file : string[directory] -> (union #f string)
  ;; returns the name of the main file, if one can be found
  (define (get-index-file doc-dir)
    (cond
      [(file-exists? (build-path doc-dir "index.htm"))
       "index.htm"]
      [(file-exists? (build-path doc-dir "index.html"))
       "index.html"]
      [(tex2page-detected doc-dir)
       =>
       (lambda (x) x)]
      [else #f]))
  
  ;; tex2page-detected : string -> (union #f string)
  (define (tex2page-detected dir)
    (let loop ([contents (directory-list dir)])
      (cond
        [(null? contents) #f]
        [else (let* ([file (car contents)]
                     [m (regexp-match #rx"(.*)-Z-H-1.html" file)])
                (or (and m
                         (file-exists? (build-path dir file))
                         (let ([index-file (string-append (cadr m) ".html")])
                           (if (file-exists? (build-path dir index-file))
                               index-file
                               #f)))
                    (loop (cdr contents))))]))))
