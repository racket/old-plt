(require-library "file.ss")

(define rrequire-cache (make-hash-table))

(define rrequire-filenames null)

(define allowed? #f)
(define last-time null)

(current-load
 (let ([old (current-load)])
   (lambda (filename)
     (if allowed?
	 (set! allowed? #f)
	 (printf "disallowed: ~a~n stack: ~a~n~n" filename last-time))
     (dynamic-wind
      (lambda ()
	(set! last-time (cons filename last-time)))
      (lambda ()
	(old filename))
      (lambda ()
	(set! last-time (cdr last-time)))))))

(define rrequire-library/filename/proc
  (lambda (x calculate-filename)
    (set! allowed? #t)
    (let-values ([(filename require-relative-collection)
		  (apply calculate-filename x)])
      (let* ([fn-symbol (string->symbol filename)]
	     [cache (hash-table-get rrequire-cache fn-symbol (lambda () #f))]
	     [no-reload?
	      (and cache
		   (let ([files (vector-ref cache 0)])
		     (foldl (lambda (file sofar)
			      (and sofar
				   (let ([filename (vector-ref file 0)]
					 [seconds (vector-ref file 1)])
				     '(printf "    testing cace: ~a current: ~a ~a~n"
					      seconds (file-modify-seconds filename)
					      (>= seconds (file-modify-seconds filename)))
				     (>= seconds (file-modify-seconds filename)))))
			    #t
			    files)))])
	(if no-reload?
	    (begin (printf "CACHE filename: ~a value: ~a~n" filename '(vector-ref cache 1))
		   (vector-ref cache 1))
	    (let ([files (list (vector (normalize-path filename)
				       (file-modify-seconds filename)))])
	      (parameterize ([current-load
			      (let ([ol (current-load)])
				(lambda (filename)
				  (set! files (cons (vector (normalize-path filename)
							    (file-modify-seconds filename))
						    files))
				  (ol filename)))]
			     [current-require-relative-collection require-relative-collection])
		(let ([value (load filename)])
		  (printf "LOAD  filename: ~a value: ~a~n" filename value)
		  (hash-table-put! rrequire-cache fn-symbol (vector files value))
		  value))))))))

(define rrequire-library/filename
  (lambda (calculate-filename)
    (lambda x
      `((global-defined-value 'rrequire-library/filename/proc) ',x ,calculate-filename))))

(define rrequire-library/proc
  (rrequire-library/filename
   '(lambda (filename . collections)
      (values (build-path (if (null? collections)
			      (collection-path "mzlib")
			      (apply collection-path collections))
			  filename)
	      (if (null? collections)
		  (list "mzlib")
		  collections)))))

(define rrequire-relative-library/proc
  (rrequire-library/filename
   '(lambda (filename . collections)
      (unless (current-require-relative-collection)
	(error 'rrequire-relative-library/proc "current-require-relative-collection is #f"))
      (let ([collection (append (current-require-relative-collection) collections)])
	(values (build-path (apply collection-path collection)
			    filename)
		collection)))))

(define rrequire/proc
  (rrequire-library/filename
   '(lambda (filename)
      (values filename #f))))

(define-macro require-library-unit/sig rrequire-library/proc)
(define-macro require-library rrequire-library/proc)
(define-macro require-relative-library rrequire-relative-library/proc)
(define-macro require-relative-library-unit/sig rrequire-relative-library/proc)
(define-macro require-unit/sig rrequire/proc)
(define-macro require rrequire/proc)
