;; Gets a list of collections that contain a doc.txt file
;; return two parallel lists:
;; (values (listof (list directory filename)) (listof docname))
;; the first has the locations of the docs and the second is their names.

(lambda (quicksort)
  (invoke-unit
   (unit 
     (import quicksort)
     (export)

     (let loop ([collection-paths (current-library-collection-paths)]
		[docs null]
		[names null])
       (cond
	[(null? collection-paths)
	 (let* ([collections-docs (map cons docs names)]
		[general-docs
		 (let ([dir (collection-path "help" "txt")])
		   (map (lambda (file)
			  (cons (list dir file)
				(substring file 0 (- (string-length file) 4))))
			(let ([pred
			       (lambda (x)
				 (and (> (string-length x) 4)
				      (string=?
				       ".txt"
				       (substring x (- (string-length x) 4)
						  (string-length x)))))])
			(let loop ([files (directory-list dir)])
			  (cond
			   [(null? files) null]
			   [else (if (pred (car files))
				     (cons (car files) (loop (cdr files)))
				     (loop (cdr files)))])))))]

		[l (quicksort (append collections-docs general-docs)
			      (lambda (a b)
				(if (string-ci<? (car (car a)) (car (car b)))
				    #f
				    (string-ci<? (cadr (car a)) (cadr (car b))))))])
	   (values (map car l) (map cdr l)))]
	[else (let ([path (car collection-paths)])
		(let cloop ([l (with-handlers ([void (lambda (x) null)]) (directory-list path))]
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
				   (with-handlers ([void (lambda (x)
							   (values null null))])
				     (let ([l ((apply require-library/proc "info.ss" lcollpath)
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
		  [else (cloop (cdr l) path collpath docs names)])))])))
   quicksort))
