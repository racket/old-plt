
; Expects parameters to be set before invocation.
; Calls `exit' when done.

(module setup-unit mzscheme
  (require (lib "unitsig.ss")
	   (lib "unit.ss")
	   (lib "file.ss")
	   (lib "list.ss")

	   "option-sig.ss"
	   (lib "sig.ss" "compiler")
	   (lib "launcher-sig.ss" "launcher")

	   "unpack.ss")

  (require "getinfo.ss")

  (provide setup@)

  (define setup@
    (unit/sig ()
      (import setup-option^
	      compiler^
	      (compiler:option : compiler:option^)
	      launcher^)
      
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

      (define setup-fprintf
	(lambda (p s . args)
	  (apply fprintf p (string-append "setup-plt: " s "~n") args)))

      (define setup-printf
	(lambda (s . args)
	  (apply setup-fprintf (current-output-port) s args)))

      (setup-printf "Setup version is ~a" (version))
      (setup-printf "PLT home directory is ~a" plthome)
      (setup-printf "Collection Paths are: ~a" (current-library-collection-paths))

      (exit-handler
       (let ([oh (exit-handler)])
	 (lambda (num)
	   (let ([error-log (build-path (collection-path "setup") "errors")])
	     (if (zero? num)
		 (when (file-exists? error-log)
		   (delete-file error-log))
		 (call-with-output-file error-log
		   (lambda (port)
		     (show-errors port))
		   'truncate))
	     (oh num)))))

      (define (warning s x)
	(setup-printf s
		      (if (exn? x)
			  (exn-message x)
			  x)))

      (define (call-info info flag mk-default test)
	(if info
	    (let ([v (info flag mk-default)])
	      (test v)
	      v)
	    (mk-default)))

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;               Archive Unpacking               ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define x-specific-collections
	(apply 
	 append
	 (specific-collections)
	 (map (lambda (x) (unpack 
			   x 
			   plthome 
			   (lambda (s) (setup-printf "~a" s)) 
			   (current-target-directory-getter))) 
	      (archives))))
      
      (define (done)
	(setup-printf "Done setting up"))

      (unless (null? (archives))
	(when (null? x-specific-collections)
	  (done)
	  (exit 0))) ; done

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;           Collection Compilation              ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define-struct cc (collection path name info))

      (define collection->cc
	(lambda (collection-p)
	  (let* ([info (with-handlers ([not-break-exn? 
					(lambda (exn) 
					  (setup-printf 
					   "Warning: ~a"
					   (if (exn? exn)
					       (exn-message exn)
					       exn))
					  #f)])
			 (get-info collection-p))]
		 [name (call-info info 'name (lambda () #f)
				  (lambda (x)
				    (unless (string? x)
				      (error "result is not a string:" x))))])
	    (and
	     name
	     (make-cc
	      collection-p
	      (apply collection-path collection-p)
	      name
	      info)))))

      (define (cannot-compile c)
	(error 'setup-plt "don't know how to compile collection: ~a" 
	       (if (= (length c) 1)
		   (car c)
		   c)))

      (define collections-to-compile
	(quicksort
	 (if (null? x-specific-collections)
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
	      x-specific-collections))
	 (lambda (a b) (string-ci<? (cc-name a) (cc-name b)))))

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
				  (let ([m (or (regexp-match-positions "making" s)
					       (regexp-match-positions "compiling" s))])
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

      ;; Close over sub-collections
      (set! collections-to-compile
	    (let loop ([l collections-to-compile])
	      (if (null? l)
		  null
		  (let* ([cc (car l)]
			 [info (cc-info cc)])
		    (append
		     (map
		      (lambda (subcol)
			(or
			 (collection->cc subcol)
			 (cannot-compile subcol)))
		      (call-info info 'compile-subcollections
				 ;; Default: subdirs with info.ss files
				 (lambda ()
				   (map (lambda (x) (append (cc-collection cc) (list x)))
					(filter
					 (lambda (p)
					   (let ([d (build-path (cc-path cc) p)])
					     (and (directory-exists? d)
						  (file-exists?
						   (build-path d "info.ss")))))
					 (directory-list (cc-path cc)))))
				 ;; Result checker:
				 (lambda (x)
				   (unless (and (list? x)
						(andmap
						 (lambda (x)
						   (and (list? x)
							(andmap
							 (lambda (x)
							   (and (string? x)
								(relative-path? x)))
							 x)))
						 x))
				     (error "result is not a list of relative path string lists:" x)))))
		     (list cc)
		     (loop (cdr l)))))))

      (define (delete-files-in-directory path printout)
	(for-each
	 (lambda (end-path)
	   (let ([path (build-path path end-path)])
	     (cond
	      [(directory-exists? path)
	       (void)]
	      [(file-exists? path)
	       (printout)
	       (unless (delete-file path)
		 (error 'delete-files-in-directory
			"unable to delete file: ~a" path))]
	      [else (error 'delete-files-in-directory
			   "encountered ~a, neither a file nor a directory"
			   path)])))
	 (directory-list path)))

      (define (clean-collection cc)
	(let* ([info (cc-info cc)]
	       [default (box 'default)]
	       [paths (call-info
		       info
		       'clean
		       (lambda ()
			 (list "compiled" (build-path "compiled" "native" (system-library-subpath))))
		       (lambda (x)
			 (unless (or (eq? x default)
				     (and (list? x)
					  (andmap string? x)))
			   (error 'setup-plt "expected a list of strings for 'clean, got: ~s"
				  x))))]
	       [printed? #f]
	       [print-message
		(lambda ()
		  (unless printed?
		    (set! printed? #t)
		    (setup-printf "Deleting files for ~a at ~a" (cc-name cc) (cc-path cc))))])
	  (for-each (lambda (path)
		      (let ([full-path (build-path (cc-path cc) path)])
			(cond
			 [(directory-exists? full-path)
			  (delete-files-in-directory
			   full-path
			   print-message)]
			 [(file-exists? full-path)
			  (delete-file full-path)
			  (print-message)]
			 [else (void)])))
		    paths)))

      (when (clean)
	(for-each clean-collection collections-to-compile))

      (when (or (make-zo) (make-so))
	(compiler:option:verbose (compiler-verbose))
	(compiler:option:compile-subcollections #f))

      (define errors null)
      (define (record-error cc desc go)
	(with-handlers ([not-break-exn?
			 (lambda (x)
			   (if (exn? x)
			       (begin
				 (fprintf (current-error-port) "~a~n" (exn-message x)))
			       (fprintf (current-error-port) "~s~n" x))
			   (set! errors (cons (list cc desc x) errors)))])
	  (go)))
      (define (show-errors port)
	(for-each
	 (lambda (e)
	   (let ([cc (car e)]
		 [desc (cadr e)]
		 [x (caddr e)])
	     (setup-fprintf port
			    "Error during ~a for ~a (~a)"
			    desc (cc-name cc) (cc-path cc))
	     (if (exn? x)
		 (setup-fprintf port "  ~a" (exn-message x))
		 (setup-fprintf port "  ~s" x))))
	 errors))

      (use-compiled-file-kinds 'all)

      (define (make-it desc compile-collection)
	(for-each (lambda (cc)
		    (record-error
		     cc
		     (format "Making ~a" desc)
		     (lambda ()
		       (unless (control-io-apply 
				(lambda (p) 
				  (setup-fprintf p "Making ~a for ~a at ~a" 
						 desc (cc-name cc) (cc-path cc)))
				compile-collection
				(cc-collection cc))
			 (setup-printf "No need to make ~a for ~a at ~a" 
				       desc (cc-name cc) (cc-path cc)))
		       (collect-garbage))))
		  collections-to-compile))

      (when (make-zo) (make-it ".zos" compile-collection-zos))
      (when (make-so) (make-it "extension" compile-collection-extension))

      (when (make-launchers)
	(let ([name-list 
	       (lambda (l)
		 (unless (and (list? l)
			      (andmap (lambda (x)
					(and (string? x)
					     (relative-path? x)))
				      l))
		   (error "result is not a list of relative path strings:" l)))])
	  (for-each
	   (lambda (cc)
	     (record-error
	      cc
	      "Launcher Setup"
	      (lambda ()
		(let* ([info (cc-info cc)]
		       [make-launcher
			(lambda (kind
				 launcher-libraries
				 launcher-names
				 program-launcher-path
				 make-launcher)
			  (let ([mzlls (call-info info launcher-libraries (lambda () null)
						  name-list)]
				[mzlns (call-info info launcher-names (lambda () null)
						  name-list)])
			    (if (= (length mzlls) (length mzlns))
				(for-each
				 (lambda (mzll mzln)
				   (let ([p (program-launcher-path mzln)])
				     (unless (file-exists? p)
				       (setup-printf "Installing ~a launcher ~a" kind p)
				       (make-launcher 
					(list
					 "-qmve-"
					 (format
					  "~s"
					  `(require (lib ,mzll ,@(cc-collection cc)))))
					p))))
				 mzlls mzlns)
				(setup-printf 
				 "Warning: ~a launcher library list ~s doesn't match name list ~s"
				 kind mzlls mzlns))))])
		  (make-launcher
		   "MrEd"
		   'mred-launcher-libraries
		   'mred-launcher-names
		   mred-program-launcher-path
		   make-mred-launcher)
		  (make-launcher
		   "MzScheme"
		   'mzscheme-launcher-libraries
		   'mzscheme-launcher-names
		   mzscheme-program-launcher-path
		   make-mzscheme-launcher)))))
	   collections-to-compile)))

      (when (call-install)
	(for-each (lambda (cc)
		    (let/ec k
		      (record-error
		       cc
		       "General Install"
		       (lambda ()
			 (let ([fn (call-info (cc-info cc)
					      'install-collection
					      (lambda () (k #f))
					      (lambda (v)
						(unless (and (string? v)
							     (relative-path? v))
						  (error "result is not a relative path string: " v))
						(let ([p (build-path (cc-path cc) v)])
						  (unless (file-exists? p)
						    (error "installer file does not exist: " p)))))])
			   (let ([installer 
				  (with-handlers ([not-break-exn?
						   (lambda (exn)
						     (error 'setup-plt
							    "error loading installer: ~a"
							    (if (exn? exn)
								(exn-message exn)
								exn)))])
				    (dynamic-require `(lib ,fn ,@(cc-collection cc)) 'installer))])
			     (setup-printf "Installing ~a" (cc-name cc))
			     (installer plthome)))))))
		  collections-to-compile))

      (done)

      (unless (null? errors)
	(setup-printf "")
	(show-errors (current-error-port))
	(when (pause-on-errors)
	  (fprintf (current-error-port)
		   "INSTALLATION FAILED.~nPress Enter to continue...~n")
	  (read-line))
	(exit 1))

      (exit 0))))
