(module colldocs mzscheme
  (require (lib "list.ss")
           (lib "getinfo.ss" "setup"))
  
  (provide colldocs)
  
  ; Gets a list of collections that contain a doc.txt file
  ; return two parallel lists:
  ; (values (listof (list directory filename)) (listof docname))
  ; the first has the locations of the docs and the second is their names.
  (define (colldocs)
    (let loop ([collection-paths (current-library-collection-paths)]
               [docs null]
               [names null])
      (cond
	[(null? collection-paths)
	 (let* ([collections-docs (map cons docs names)]
		[l (quicksort collections-docs
			      (lambda (a b) (string<=? (cdr a) (cdr b))))])
	   (values (map car l) (map cdr l)))]
	[else (let ([path (car collection-paths)])
		(let cloop ([l (with-handlers ([not-break-exn? (lambda (x) null)])
                                 (directory-list path))]
			    [path path]
			    [collpath null]
			    [docs docs]
			    [names names])
                  (cond
                    [(null? l) (if (null? collpath)
                                   (loop (cdr collection-paths) docs names)
                                   (values docs names))]
                    [(and (directory-exists? (build-path path (car l)))
                          (not (member (car l) names)))
                     (let* ([coll (car l)]
                            [colldir (build-path path coll)]
                            [lcollpath (append collpath (list coll))]
                            [doc-txt-file (list colldir "doc.txt")]
                            [this? (file-exists? (apply build-path doc-txt-file))])
                       (let-values ([(sub-docs sub-names)
                                     (with-handlers ([not-break-exn?
                                                      (lambda (x)
                                                        (values null null))])
                                       (let ([l ((get-info lcollpath) 
                                                 'doc-sub-collections 
                                                 (lambda () null))])
                                         (cloop l colldir lcollpath null null)))])
                         (let ([sub-names (map (lambda (s) (string-append coll " " s)) sub-names)])
                           (let-values ([(ldocs lnames)
                                         (if this?
                                             (values (cons doc-txt-file sub-docs)
                                                     (cons coll sub-names))
                                             (values sub-docs sub-names))])
                             (cloop (cdr l) path collpath (append ldocs docs) (append lnames names))))))]
                    [else (cloop (cdr l) path collpath docs names)])))]))))
