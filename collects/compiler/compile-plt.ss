
(parameterize ([require-library-use-compiled #f])
  (require-library "cmdline.ss"))

(define verbose (make-parameter #f))
(define make-verbose (make-parameter #f))
(define compiler-verbose (make-parameter #f))

(define clean? #f)
(define zo? #t)
(define so? #f)

(define specific-collections
  (parse-command-line
   program
   argv
   `((once-each
      [("--clean")
       ,(lambda (flag) (set! clean? #t))
       ("Delete existing compiled files")]
      [("-n" "--no-zo")
       ,(lambda (flag) (set! zo? #f))
       ("Do not produce .zo files.")]
      [("-s" "--so")
       ,(lambda (flag) (set! so? #t))
       ("Produce .so files.")]
      [("-v" "--verbose")
       ,(lambda (flag)
	  (verbose #t))
       ("See names of compiled files")]
      [("-m" "--make-verbose")
       ,(lambda (flag)
	  (make-verbose #t))
       ("See make and compiler usual messages")]
      [("-r" "--compile-verbose")
       ,(lambda (flag)
	  (make-verbose #t)
	  (compiler-verbose #t))
       ("See make and compiler verbose messages")])
     (multi
      [("-l" "--collection")
       ,(lambda (flag collection)
	  (list collection))
       ("Compile <collection> and ignore unspecified collections"
	"collection")]))
   (lambda (specifics) specifics)
   '()))


(define-struct cc (collection path name info))

(define collection->cc
  (lambda (collection-p)
    (let/ec k
      (with-handlers ([(lambda (x) #t)
		       (lambda (x) #f)])
	 (let ([info (parameterize ([require-library-use-compiled #f])
			(apply require-library "info.ss" collection-p))])
	   (make-cc
	    collection-p
	    (apply collection-path collection-p)
	    (andmap 
	     (lambda (sym) (or (info sym (lambda () (k #f))) #t))
	     '(compile-prefix name))
	    info))))))

(define (cannot-compile c)
  (error 'compile-plt "don't know how to compile collection: ~a" 
	 (if (= (length c) 1)
	     (car c)
	     c)))

(define collections-to-compile
  (if (null? specific-collections)
      (let ([ht (make-hash-table)])
	(let loop ([collection-paths (current-library-collection-paths)])
	  (cond
	   [(null? collection-paths) 
	    (hash-table-map ht (lambda (k v) v))]
	   [else (let ([cp (car collection-paths)])
		   (let loop ([collections (if (directory-exists? cp)
					       (directory-list cp)
					       null)])
		     (cond
		      [(null? collections) (void)]
		      [else (let* ([collection (car collections)]
				   [coll-sym (string->symbol collection)])
			      (hash-table-get
			       ht
			       coll-sym
			       (lambda ()
				 (let ([cc (collection->cc (list collection))])
				   (when cc
				     (hash-table-put! 
				      ht
				      coll-sym
				      cc))))))
			    (loop (cdr collections))])))
		 (loop (cdr collection-paths))])))
      (map
       (lambda (c)
	 (or (collection->cc c)
	     (cannot-compile c)))
       specific-collections)))

(define control-io-apply
  (lambda (print-doing f args)
    (if (make-verbose)
	(begin
	  (apply f args)
	  #t)
	(let* ([oop (current-output-port)]
	       [printed? #f]
	       [on? #f]
	       [op (make-output-port 
		    (lambda (s)
		      (let loop ([s s])
			(if on?
			    (let ([m (regexp-match-positions (string #\newline) s)])
			      (if m
				  (begin
				    (set! on? #f)
				    (when (verbose)
				      (display (substring s 0 (add1 (caar m))) oop)
				      (flush-output oop))
				    (loop (substring s (add1 (caar m)) (string-length s))))
				  (when (verbose)
				    (display s oop)
				    (flush-output oop))))
			    (let ([m (regexp-match-positions "making" s)])
			      (when m
			        (unless printed?
				   (set! printed? #t)
				   (print-doing oop))
				(set! on? #t)
				(when (verbose)
				  (display "  " oop)) ; indentation 
				(loop (substring s (caar m) (string-length s))))))))
		    void)])
	  (parameterize ([current-output-port op])
	    (apply f args)
	    printed?)))))

; Close over sub-collections
(define collections-to-compile
  (let loop ([l collections-to-compile])
    (if (null? l)
	null
	(let* ([cc (car l)]
	       [info (cc-info cc)])
	  (append
	   (list cc)
	   (map
	    (lambda (sub)
	      (let ([subcol (append (cc-collection cc) sub)])
		(or
		 (collection->cc subcol)
		 (cannot-compile subcol))))
	    (info 'compile-subcollections (lambda () null)))
	   (loop (cdr l)))))))

(define (delete-files-in-directory path printout)
  (let ([printed? #f])
    (let loop ([path path])
      (cond
       [(directory-exists? path)
	(for-each (lambda (e) (loop (build-path path e)))
		  (directory-list path))]
       [(file-exists? path)
	(unless printed? 
		(set! printed? #t)
		(printout))
	(unless (delete-file path)
		(error 'delete-files-in-directory
		       "unable to delete file: ~a" path))]
       [else (error 'delete-files-in-directory
		    "encountered ~a, neither a file nor a directory"
		    path)]))))

(define (clean-collection cc)
  (let* ([path (build-path (cc-path cc) "compiled")])
    (when (directory-exists? path)
	  (delete-files-in-directory 
	   path
	   (lambda ()
	     (printf "Deleting files for ~a in ~a~n" (cc-name cc) path))))))

(when clean?
  (for-each clean-collection
	    collections-to-compile))

(when (or zo? so?)
  (require-library "compile.ss" "compiler")
  (compiler:option:verbose (compiler-verbose))
  (compiler:option:compile-subcollections #f))

(when zo?
  (for-each (lambda (cc)
	      (unless
	       (control-io-apply 
		(lambda (p) (fprintf p "Making .zos for ~a at ~a~n" (cc-name cc) (cc-path cc)))
		compile-collection-zos 
		(cc-collection cc))
	       (printf "No need to make .zos for ~a at ~a~n"  (cc-name cc) (cc-path cc))))
	    collections-to-compile))

(when so?
  (for-each (lambda (cc)
	      (control-io-apply 
	       (lambda (p) (fprintf p "Checking extensions for ~a at ~a~n" (cc-name cc) (cc-path cc)))
	       compile-collection-extension 
	       (cc-collection cc)))
	    collections-to-compile))
