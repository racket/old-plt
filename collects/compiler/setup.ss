
(parameterize ([require-library-use-compiled #f])
  (require-library "cmdline.ss"))

(define verbose (make-parameter #f))
(define make-verbose (make-parameter #f))
(define compiler-verbose (make-parameter #f))
(define clean (make-parameter #f))
(define make-zo (make-parameter #t))
(define make-so (make-parameter #f))
(define make-launchers (make-parameter #t))
(define call-install (make-parameter #t))

(define-values (specific-collections archives)
  (command-line
   "setup-plt"
   argv
   (once-each
    [("-c" "--clean") "Delete existing compiled files"
		      (clean #t)]
    [("-n" "--no-zo") "Do not produce .zo files"
		      (make-zo #f)]
    [("-x" "--no-launcher") "Do not produce launcher programs"
			    (make-launchers #f)]
    [("-i" "--no-install") "Do not call collection-specific installers"
			   (call-install #f)]
    [("-e" "--extension") "Produce native code extensions"
			  (make-so #t)]
    [("-v" "--verbose") "See names of compiled files and info printfs"
			(verbose #t)]
    [("-m" "--make-verbose") "See make and compiler usual messages"
			     (make-verbose #t)]
    [("-r" "--compile-verbose") "See make and compiler verbose messages"
				(make-verbose #t)
				(compiler-verbose #t)]
    [("-l") =>
	    (lambda (flag . collections)
	      (map list collections))
	    '("Setup specific <collection>s only" "collection")])
   (=>
    (lambda (collections . archives) 
      (values (if (null? collections)
		  null
		  (car collections))
	      archives))
    '("archive")
    (lambda (s)
      (display s)
      (printf "If no <archive> or -l <collection> is specified, all collections are setup~n")
      (exit 0)))))

(define plthome
  (or (getenv "PLTHOME")
      (let ([dir (collection-path "mzlib")])
	(and dir
	     (let-values ([(base name dir?) (split-path dir)])
		    (and (string? base)
		      (let-values ([(base name dir?) (split-path base)])
			(and (string? base)
			     (complete-path? base)
			     base))))))))

(printf "PLT home directory is ~a~n" plthome)
(printf "Collection Paths are: ~a~n" (current-library-collection-paths))

(define (warning s x)
  (printf s
	  (if (exn? x)
	      (exn-message x)
	      x)))

(define (call-info info flag default test)
  (with-handlers ([void (lambda (x) 
			  (warning
			   (format "Warning: error getting ~a info: ~~a~n"
				   flag)
			   x)
			  default)])
     (let ([v (info flag (lambda () default))])
       (test v)
       v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               Archive Unpacking              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (port64->port p)
  (let* ([waiting 0]
	 [waiting-bits 0]
	 [at-eof? #f]
	 [push
	  (lambda (v)
	    (set! waiting (+ (arithmetic-shift waiting 6) v))
	    (set! waiting-bits (+ waiting-bits 6)))])
    (make-input-port
     (lambda ()
       (let loop ()
	 (if at-eof?
	     eof
	     (if (>= waiting-bits 8)
		 (begin0
		  (integer->char (arithmetic-shift waiting (- 8 waiting-bits)))
		  (set! waiting-bits (- waiting-bits 8))
		  (set! waiting (bitwise-and waiting (sub1 (arithmetic-shift 1 waiting-bits)))))
		 (let* ([c (read-char p)]
			[n (if (eof-object? c)
			       (#%char->integer #\=)
			       (char->integer c))])
		   (cond
		    [(<= (#%char->integer #\A) n (#%char->integer #\Z)) (push (- n (#%char->integer #\A)))]
		    [(<= (#%char->integer #\a) n (#%char->integer #\z)) (push (+ 26 (- n (#%char->integer #\a))))]
		    [(<= (#%char->integer #\0) n (#%char->integer #\9)) (push (+ 52 (- n (#%char->integer #\0))))]
		    [(= (#%char->integer #\+) n) (push 62)]
		    [(= (#%char->integer #\/) n) (push 63)]
		    [(= (#%char->integer #\=) n) (set! at-eof? #t)])
		   (loop))))))
     (lambda ()
       (or at-eof? (char-ready? p)))
     void)))

(define (port64gz->port p64gz)
  (require-library "inflate.ss")
  ; Inflate in a thread so the whole input isn't read at once
  (let*-values ([(pgz) (port64->port p64gz)]
		[(waiting?) #f]
		[(ready) (make-semaphore)]
		[(read-pipe write-pipe) (make-pipe)]
		[(out) (make-output-port
			(lambda (s)
			  (set! waiting? #t)
			  (semaphore-wait ready)
			  (set! waiting? #f)
			  (display s write-pipe))
			(lambda ()
			  (close-output-port write-pipe)))]
		[(get) (make-input-port
			(lambda ()
			  (if (char-ready? read-pipe)
			      (read-char read-pipe)
			      (begin
				(semaphore-post ready)
				(read-char read-pipe))))
			(lambda ()
			  (or (char-ready? read-pipe) waiting?))
			(lambda ()
			  (close-input-port read-pipe)))])
    (thread (lambda () 
	      (with-handlers ([void (lambda (x)
				      (warning "Warning: unpacking error: ~a~n" x))])
                (gunzip-through-ports pgz out))
	      (close-output-port out)))
    get))

(define (unmztar p filter)
  (require-library "file.ss" "dynext")
  (let loop ()
    (let ([kind (read p)])
      (unless (eof-object? kind)
       (case kind
	 [(dir) (let ([s (apply build-path (read p))])
		  (unless (relative-path? s)
		    (error "expected a directory name relative path string, got" s))
		  (when (filter 'dir s plthome)
		   (let ([d (build-path plthome s)])
		     (unless (directory-exists? d)
		       (when (verbose)
			 (printf "  making directory ~a~n" d))
		       (make-directory* d)))))]
	 [(file) (let ([s (apply build-path (read p))])
		   (unless (relative-path? s)
		    (error "expected a file name relative path string, got" s))
		   (let ([len (read p)])
		     (unless (and (number? len) (integer? len))
		       (error "expected a file name size, got" len))
		     (let* ([write? (filter 'file s plthome)]
			    [path (build-path plthome s)])
		       (let ([out (and write?
				       (not (file-exists? path))
				       (open-output-file path))])
			 (when (and write? (not out))
			   (printf "Warning: ~a already exists; skipping~n" path))
			 (when (and out (or #t (verbose)))
			   (printf "  unpacking ~a~n" path))
			 ; Find starting *
			 (let loop ()
			   (let ([c (read-char p)])
			     (cond
			      [(char=? c #\*) (void)] ; found it
			      [(char-whitespace? c) (loop)]
			      [(eof-object? c) (void)] ; signal the error below
			      [else (error 
				     (format
				      "unexpected character setting up ~a, looking for #\*"
				      path)
				     c)])))
			 ; Copy file data
			 (let loop ([n len])
			   (unless (zero? n)
			     (let ([c (read-char p)])
			       (when (eof-object? c)
				  (error (format 
					  "unexpected end-of-file while ~a ~a (at ~a of ~a)"
					   (if out "unpacking" "skipping")
					   path
					   (- len n -1) len)))
			       (when out
				  (write-char c out)))
			     (loop (sub1 n))))
			 (when out
			    (close-output-port out))))))]
	 [else (error "unknown file tag" kind)])
       (loop)))))

(define (unpack-archive archive)
  (with-handlers ([void
		   (lambda (x)
		     (warning (format "Warning: error unpacking ~a: ~~a~n"
				      archive)
			      x)
		     null)])
   (call-with-input-file
    archive
    (lambda (p64)
      (let* ([p (port64gz->port p64)])
	(unless (and (eq? #\P (read-char p))
		     (eq? #\L (read-char p))
		     (eq? #\T (read-char p)))
	   (error "not an unpackable distribution archive"))
	(let* ([n (make-namespace)]
	       [info (eval (read p) n)])
	  (unless (and (procedure? info)
		       (procedure-arity-includes? info 2))
	     (error "expected a procedure of arity 2, got" info))
	  (let ([name (call-info info 'name #f (lambda (n) 
						 (unless (string? n)
						  (if name
						      (error "couldn't find the package name")
						      (error "expected a string")))))]
		[unpacker (call-info info 'unpacker #f (lambda (n) 
							 (unless (eq? n 'mzscheme)
							   (error "unpacker isn't mzscheme:" n))))])
	    (unless (and name unpacker)
	     (error "bad name or unpacker"))
	    (printf "Unpacking ~a from ~a~n" name archive)
	    (let ([u (eval (read p) n)])
	      (unless (unit? u)
	        (error "expected a unit, got" u))
	      (let ([plthome plthome]
		    [unmztar (lambda (filter)
			       (unmztar p filter))])
		(invoke-unit u plthome unmztar))))))))))

(set! specific-collections
      (apply 
       append
       specific-collections
       (map unpack-archive archives)))

(unless (null? archives)
  (when (null? specific-collections)
     (exit 0))) ; done

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           Collection Compilation             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct cc (collection path name info))

(define collection->cc
  (lambda (collection-p)
    (with-handlers ([void (lambda (x) #f)])
      (let ([dir (apply collection-path collection-p)])
	(with-handlers ([(lambda (x)
			   (and (exn:i/o:filesystem:file? x)
				(string=? (exn:i/o:filesystem-pathname x)
					  (build-path dir "info.ss"))))
			 (lambda (x) #f)]
			[void
			 (lambda (x)
			   (warning "Warning: error loading info.ss: ~a~n" x)
			   #f)])
	   (let* ([info (parameterize ([require-library-use-compiled #f])
			  (apply require-library/proc "info.ss" collection-p))]
		  [name (call-info info 'name #f
				   (lambda (x)
				     (unless (string? x)
					     (error "result is not a string:" x))))])
	     (and
	      name
	      (call-info info 'compile-prefix #f void)
	      (make-cc
	       collection-p
	       (apply collection-path collection-p)
	       name
	       info))))))))

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
	    (lambda (subcol)
	      (or
	       (collection->cc subcol)
	       (cannot-compile subcol)))
	    (call-info info 'compile-subcollections null
		       (lambda (x)
			 (unless (and (list? x)
				      (andmap
				       (lambda (x)
					 (list? x)
					 (andmap
					  (lambda (x)
					    (and (string? x)
						 (relative-path? x)))
					  x))
				       x))
			     (error "result is not a list of relative path string lists:" x)))))
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

(when (clean)
  (for-each clean-collection collections-to-compile))

(when (or (make-zo) (make-so))
  (parameterize ([require-library-use-compiled #f])
    (require-library "compile.ss" "compiler"))
  (compiler:option:verbose (compiler-verbose))
  (compiler:option:compile-subcollections #f))

(define errors null)
(define (record-error cc desc go)
  (with-handlers ([void (lambda (x)
			  (fprintf (current-error-port) "~a~n" (exn-message x))
			  (set! errors (cons (cons cc desc) errors)))])
    (go)))

(define (make-it desc compile-collection)
  (for-each (lambda (cc)
	      (record-error
	       cc
	       (format "Making ~a" desc)
	       (lambda ()
		 (unless
		     (control-io-apply 
		      (lambda (p) (fprintf p "Making ~a for ~a at ~a~n" desc (cc-name cc) (cc-path cc)))
		      compile-collection 
		      (cc-collection cc))
		   (printf "No need to make ~a for ~a at ~a~n" desc (cc-name cc) (cc-path cc))))))
	    collections-to-compile))

(when (make-zo)
 (make-it ".zos" compile-collection-zos))

(when (make-so)
 (make-it "extension" compile-collection-extension))

(when (make-launchers)
  (define (name-list l)
    (unless (and (list? l)
		 (andmap (lambda (x)
			   (and (string? x)
				(relative-path? x)))
			 l))
       (error "result is not a list of relative path strings:" l)))
  (require-library "launcher.ss" "launcher")
  (for-each (lambda (cc)
	      (record-error
	       cc
	       "Launcher Setup"
	       (lambda ()
		 (when (= 1 (length (cc-collection cc)))
		   (let ([info (cc-info cc)])
		     (map
		      (lambda (kind
			       mzscheme-launcher-libraries
			       mzscheme-launcher-names
			       mzscheme-program-launcher-path
			       install-mzscheme-program-launcher)
			(let ([mzlls (call-info info mzscheme-launcher-libraries null
						name-list)]
			      [mzlns (call-info info mzscheme-launcher-names null
						name-list)])
			  (if (= (length mzlls) (length mzlns))
			      (map
			       (lambda (mzll mzln)
				 (let ([p (mzscheme-program-launcher-path mzln)])
				   (unless (file-exists? p)
				     (printf "Installing ~a launcher ~a~n" kind p)
				     (install-mzscheme-program-launcher 
				      mzll
				      (car (cc-collection cc))
				      mzln))))
			       mzlls mzlns)
			      (printf "Warning: ~a launcher library list ~s doesn't match name list ~s~n"
				      kind mzlls mzlns))))
		      '("MzScheme" "MrEd")
		      '(mzscheme-launcher-libraries mred-launcher-libraries)
		      '(mzscheme-launcher-names mred-launcher-names)
		      (list mzscheme-program-launcher-path mred-program-launcher-path)
		      (list install-mzscheme-program-launcher install-mred-program-launcher)))))))
	    collections-to-compile))

(when (call-install)
  (let ()
    (for-each (lambda (cc)
		(record-error
		 cc
		 "General Install"
		 (lambda ()
		   (let ([t (call-info (cc-info cc) 'install-collection void
				       (lambda (p)
					 (unless (and (procedure? p)
						      (procedure-arity-includes? p 1))
					   (error "result is not a procedure of arity 1"))))])
		     (with-handlers ([void (lambda (x)
					     (warning "Warning: error running installer: ~a~n"
						      x))])
		       (t plthome))))))
	      collections-to-compile)))

(printf "Done setting up~n")

(unless (null? errors)
  (for-each
   (lambda (e)
     (let ([cc (car e)]
	   [desc (cdr e)])
       (fprintf (current-error-port) " Error during ~a for ~a (~a)~n"
		desc (cc-name cc) (cc-path cc))))
   errors)
  (exit -1))
