(module manuals mzscheme
  (require (lib "list.ss")
           "private/colldocs.ss"
           "private/docpos.ss")

  (provide find-manuals)
 
  (define (find-manuals)
    (let* ([doc-collection-path (with-handlers ([void (lambda (x) #f)])
                                  (collection-path "doc"))]
           [docs (let loop ([l (if doc-collection-path
                                   (directory-list doc-collection-path)
                                   null)])
                   (cond
                     [(null? l) null]
                     [(file-exists? (build-path doc-collection-path (car l) "index.htm"))
                      (cons (car l) (loop (cdr l)))]
                     [else (loop (cdr l))]))]
           [docs (quicksort docs (lambda (a b)
                                   (let ([ap (standard-html-doc-position a)]
                                         [bp (standard-html-doc-position b)])
                                     (cond
                                       [(= ap bp) (string<? a b)]
                                       [else (< ap bp)]))))]
           [doc-paths (map (lambda (doc) (build-path doc-collection-path doc)) docs)]
           [names
            (map
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
                                 [(regexp-match "<[tT][iI][tT][lL][eE]>(.*)</[tT][iI][tT][lL][eE]>" r) => cadr]
                                 [else (loop)])))))))))
             doc-paths)])
      (let-values ([(collections-doc-files collection-names) (colldocs)])
        (apply
         string-append
         "<html><head>"
         "<TITLE>Installed Manuals</TITLE>"
         "</head>"
         "<body>"
         (let ([up-file (build-path (collection-path "doc") "help" "scheme" "doc.html")]
               [up-icon (build-path (collection-path "doc") "icons" "up.gif")])
           (format "<a href=\"file:~a\"><img src=\"file:~a\"></a> <a href=\"file:~a\">Up</a>"
                   up-file
                   up-icon
                   up-file))
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
           (lambda (collection-doc-file name)
             (format "<LI> <A HREF=\"file:~a\">~a collection</A>"
                     (apply build-path collection-doc-file)
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
                     "</font>")]
              [else
               (list*
                "<H3>Uninstalled Manuals</H3>"
                "<UL>"
                (append
                 (map
                  (lambda (doc-pair)
                    (format "<LI> <A HREF=\"file:~a\">~a</A>~a"
                            (build-path doc-collection-path (car doc-pair) "index.htm")
                            (cdr doc-pair)
                            (if (file-exists? (build-path doc-collection-path (car doc-pair) "hdindex"))
                                " (index installed)"
                                "")))
                  uninstalled)
                 (list "</UL></body></html>")))]))))))))
