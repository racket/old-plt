(define rrequire-cache (make-hash-table))

(define rrequire-filenames null)

(current-load
 (let ([old (current-load)])
   (lambda (filename)
     (printf "> filename: ~a ~a~n" filename (current-require-relative-collection))
     (begin0 (old filename)
	     (printf "< filename: ~a~n" filename)))))


(define rrequire-library/filename/proc
  (lambda (x calculate-filename get-collection)
    (let* ([filename (apply calculate-filename x)]
	   [fn-symbol (string->symbol filename)]
	   [cache (hash-table-get rrequire-cache fn-symbol (lambda () #f))]
	   [_ (printf "1 ~a~n" cache)]
	   [no-reload?
	    (and cache
		 (let ([files (vector-ref cache 0)])
		   (foldl (lambda (file sofar)
			    (and sofar
				 (let ([filename (vector-ref file 0)]
				       [seconds (vector-ref file 1)])
				   '(printf "seconds: ~a fm: ~a~n"
					    seconds (file-modify-seconds filename))
				   (> seconds (file-modify-seconds filename)))))
			  #t
			  files)))])
      (if no-reload?
	  (begin (printf "CACHE filename: ~a value: ~a~n" filename (vector-ref cache 1))
		 (vector-ref cache 1))
	  (let ([files (list (vector (normalize-path filename)
				     (file-modify-seconds filename))])
	    (parameterize ([current-load
			    (let ([ol (current-load)])
			      (lambda (filename)
				(set! files (cons (vector (normalize-path filename)
							  (file-modify-seconds filename))
						  files))
				(ol filename)))]
			   [current-require-relative-collection
			    (apply get-collection x)])
	      (let ([value (load filename)])
		(printf "LOAD  filename: ~a value: ~a loaded: ~a~n" filename value files)
		(hash-table-put! rrequire-cache fn-symbol (vector files value))
		value)))))))

(define rrequire-library/filename
  (lambda (calculate-filename get-collection)
    (lambda x
      `(rrequire-library/filename/proc ',x ,calculate-filename ,get-collection))))

(define-macro require-library-unit/sig
  (rrequire-library/filename
   '(lambda (filename . collections)
      (build-path (if (null? collections)
		      (collection-path "mzlib")
		      (apply collection-path collections))
		  filename))
   '(lambda (filename . collections)
      (if (null? collections)
	  (list "mzlib")
	  collections))))
