
; Gets a list of collections that contain a doc.txt file

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
	 (let ([l (quicksort (map cons docs names)
			     (lambda (a b)
			       (string-ci<? (car a) (car b))))])
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
			  [this? (file-exists? (build-path colldir "doc.txt"))])
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
					   (values (cons colldir sub-docs)
						   (cons coll sub-names))
					   (values sub-docs sub-names))])
			   (cloop (cdr l) path collpath (append ldocs docs) (append lnames names))))))]
		  [else (cloop (cdr l) path collpath docs names)])))])))
   quicksort))
