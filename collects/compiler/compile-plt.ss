
(parameterize ([require-library-use-compiled #f])
  (require-library "cmdline.ss"))

(define verbose (make-parameter #f))
(define compiler-verbose (make-parameter #f))
(define search-collections (make-parameter #t))

(define flags
  (parse-command-line
   program
   argv
   `((once-each
      [("--clean")
       ,(lambda (flag) 'clean)
       ("Delete existing compiled files")]
      [("-n" "--no-zo")
       ,(lambda (flag) 'no-zo)
       ("Do not produce .zo files.")]
      [("-s" "--so")
       ,(lambda (flag) 'so)
       ("Produce .so files.")]
      [("-r" "--so-verbose")
       ,(lambda (flag)
	  (verbose #t)
	  (compiler-verbose #t))
       ("See make and compiler verbose messages")]
      [("-v" "--verbose")
       ,(lambda (flag)
	  (verbose #t))
       ("See make and compiler usual messages")])
     (multi
      [("-l" "--collection")
       ,(lambda (flag collection)
	  (search-collections #f)
	  (list collection))
       ("Compile <collection>, and ignore unspecified collections"
	"collection")]))
   (lambda (flag-accum) flag-accum)
   '()))

(define clean? (member 'clean flags))
(define zo? (not (member 'no-zo flags)))
(define so? (member 'so flags))

(define collections-to-compile
  (if (search-collections)
      (let ([ht (make-hash-table)]
	    [collection-can-compile?
	     (lambda (collection)
	       (let/ec k
		 (with-handlers ([(lambda (x) #t)
				  (lambda (x) #f)])
		   (let ([info (load (build-path (collection-path collection)
						 "info.ss"))])
		     (for-each 
		      (lambda (sym) (info sym (lambda () (k #f))))
		      '(name compile-prefix compile-omit-files))
		     #t))))])
	(let loop ([collection-paths (current-library-collection-paths)])
	  (cond
	    [(null? collection-paths) 
	     (hash-table-map ht (lambda (k v) (list k)))]
	    [else (let ([cp (car collection-paths)])
		    (let loop ([collections (if (directory-exists? cp)
						(directory-list cp)
						null)])
		      (cond
			[(null? collections) (void)]
			[else (let ([collection (car collections)])
				(when (collection-can-compile? collection)
				  (hash-table-put! 
				   ht
				   collection
				   #t)))
			      (loop (cdr collections))])))
		  (loop (cdr collection-paths))])))
      (let loop ([in flags]
			[out null])
	       (cond
		 [(null? in) out]
		 [else (let ([x (car in)])
			 (if (list? x)
			     (loop (cdr in) (cons x out))
			     (loop (cdr in) out)))]))))

(define control-io-apply
  (let ([op (make-output-port void void)])
    (lambda (f args)
      (if (verbose)
	  (apply f args)
	  (parameterize ([current-output-port op])
	    (apply f args))))))

(define (delete-files-in-directory path)
  (cond
    [(directory-exists? path)
     (for-each (lambda (e) (delete-files-in-directory (build-path path e)))
	       (directory-list path))]
    [(file-exists? path)
     (unless (delete-file path)
       (error 'delete-files-in-directory
	      "unable to delete file: ~a" path))]
    [else (error 'delete-files-in-directory
		 "encountered ~a, neither a file nor a directory"
		 path)]))

(when clean?
  (for-each (lambda (collection-list)
	      (let* ([path (build-path (apply collection-path collection-list)
				       "compiled")])
		(when (directory-exists? path)
		  (printf "deleting files in ~a~n" path)
		  (delete-files-in-directory path))))
	    collections-to-compile))

(when (or zo? so?)
  (require-library "compile.ss" "compiler")
  (compiler:option:verbose (compiler-verbose)))

(when zo?  
  (for-each (lambda (collection-list)
	      (printf "zo compiling ~a~n" (apply collection-path collection-list))
	      (control-io-apply compile-collection-zos collection-list))
	    collections-to-compile))

(when so?
  (for-each (lambda (collection-list)
	      (printf "so compiling ~a~n" (apply collection-path collection-list))
	      (control-io-apply compile-collection-extension collection-list))
	    collections-to-compile))

