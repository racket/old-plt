(require-library "xml.ss" "xml")
(require-library "function.ss")

(define (all-collections)
  (let ([colls (make-hash-table)])
    (for-each
     (lambda (collection-path-dir)
       (when (directory-exists? collection-path-dir)
	 (for-each
	  (lambda (collection)
	    (when (and (directory-exists? (build-path collection-path-dir collection))
		       (not (string=? collection "CVS")))
	      (hash-table-put! colls (string->symbol collection) #t)))
	  (directory-list collection-path-dir))))
     (current-library-collection-paths))
    (quicksort (hash-table-map colls (lambda (x v) (symbol->string x)))
	       string<=?)))
  
(define-struct comp (name xml))

(define (get-blurb collection)
  (let/ec k
    (let ([info-file (build-path (collection-path collection) "info.ss")])
      (unless (file-exists? info-file)
	(k #f))
      (let* ([proc (with-handlers ([(lambda (x) #t)
				    (lambda (x)
				      (k #f))])
		     (load info-file))]
	     [blurb (with-handlers ([(lambda (x) #t)
				     (lambda (x)
				       (k
					(make-comp
					 collection
					 (format "error during 'blurb: ~a"
						 (if (exn? x)
						     (exn-message x)
						     x)))))])
		      (proc 'blurb (lambda () (k #f))))]
	     [name (with-handlers ([(lambda (x) #t)
				    (lambda (x)
				      (k
				       (make-comp
					collection
					(format "error during 'name: ~a"
						(if (exn? x)
						    (exn-message x)
						    x)))))])
		     (proc 'name (lambda () (k #f))))])
	(make-comp
	 name
	 `(li ()
	      (font ((color "forest green")) (b () ,name))
	      (br ())
	      ,@blurb))))))

(define (comp<=? ca cb) (string<=? (comp-name ca) (comp-name cb)))

(define (build-string comp)
  (let ([blurb (comp-xml comp)]
	[p (open-output-string)])
    (write-xml/content
     (xexpr->xml
      blurb)
     p)
    (newline p)
    (newline p)
    (get-output-string p)))

(define comps
  (quicksort
   (filter (lambda (x) x)
	   (map get-blurb (all-collections)))
   comp<=?))

(apply string-append
       (append
	(list (format "<ul>~n"))
	(map build-string comps)
	(list (format "</ul>~n"))))
