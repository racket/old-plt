
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

	   "unpack.ss"
	   "getinfo.ss"
	   "plthome.ss")

  (provide setup@)

  (define setup@
    (unit/sig ()
      (import setup-option^
	      compiler^
	      (compiler:option : compiler:option^)
	      launcher^)

      (define setup-fprintf
	(lambda (p s . args)
	  (apply fprintf p (string-append "setup-plt: " s "~n") args)))

      (define setup-printf
	(lambda (s . args)
	  (apply setup-fprintf (current-output-port) s args)))

      (setup-printf "Setup version is ~a" (version))
      (setup-printf "PLT home directory is ~a" plthome)
      (setup-printf "Collection paths are ~a" (if (null? (current-library-collection-paths))
						  "empty!"
						  ""))
      (for-each (lambda (p)
		  (setup-printf "  ~a" p))
		(current-library-collection-paths))

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
			   (current-target-directory-getter)
			   (force-unpacks)
			   (current-target-plt-directory-getter)))
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
				    (when x
				      (unless (string? x)
					(error 
					 (format 
					  "'name' result from collection ~s is not a string:"
					  collection-p)
					 x)))))])
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
		  [else (let* ([cp (car collection-paths)]
			       [cp-contents
				(if (directory-exists? cp)
				    (directory-list cp)
				    null)])
			  (let loop ([collections (filter
						   (lambda (x)
						     (directory-exists?
						      (build-path cp x)))
						   cp-contents)])
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

      (define re:making (regexp "making (.*) because "))
      (define re:compiling (regexp "compiling: (.*)"))

      (define control-io-apply
	(lambda (print-doing f args)
	  (if (make-verbose)
	      (begin
		(apply f args)
		#t)
	      (let* ([oop (current-output-port)]
		     [printed? #f]
		     [on? #f]
		     [dir-table (make-hash-table 'equal)]
		     [line-accum ""]
		     [op (make-custom-output-port 
			  #f
			  (lambda (s start end flush?)
			    (let loop ([s (substring s start end)])
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
				  (let ([s (string-append line-accum s)])
				    (let ([m (or (regexp-match-positions re:making s)
						 (regexp-match-positions re:compiling s))])
				      (unless m
					(set! line-accum s)
					(let ([m (regexp-match-positions #rx".*[\r\n]" line-accum)])
					  (when m
					    (set! line-accum (substring line-accum (cdar m))))))
				      (when m
					(unless printed?
					  (set! printed? #t)
					  (print-doing oop))
					(set! on? #t)
					(unless (verbose)
					  (let ([path (path-only (substring s (caadr m) (cdadr m)))])
					    (unless (hash-table-get dir-table path (lambda () #f))
					      (hash-table-put! dir-table path #t)
					      (print-doing oop path))))
					(when (verbose)
					  (display "  " oop)) ; indentation 
					(loop (substring s (caar m) (string-length s))))))))
			    (- end start))
			  void
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

      (define (delete-file/record-dependency path dependencies)
	(when (regexp-match-positions #rx"[.]dep$" path)
	  (let ([deps (with-handlers ([not-break-exn? (lambda (x) null)])
			(with-input-from-file path read))])
	    (when (and (pair? deps) (list? deps))
              (for-each (lambda (s)
			  (when (string? s)
			    (hash-table-put! dependencies s #t)))
			(map un-plthome-ify (cdr deps))))))
	(delete-file path))

      (define (delete-files-in-directory path printout dependencies)
	(for-each
	 (lambda (end-path)
	   (let ([path (build-path path end-path)])
	     (cond
	      [(directory-exists? path)
	       (void)]
	      [(file-exists? path)
	       (printout)
	       (delete-file/record-dependency path dependencies)]
	      [else (error 'delete-files-in-directory
			   "encountered ~a, neither a file nor a directory"
			   path)])))
	 (directory-list path)))

      (define (clean-collection cc dependencies)
	(let* ([info (cc-info cc)]
	       [default (box 'default)]
	       [paths (call-info
		       info
		       'clean
		       (lambda ()
			 (list "compiled" 
			       (build-path "compiled" "native")
			       (build-path "compiled" "native" (system-library-subpath))))
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
			   print-message
			   dependencies)]
			 [(file-exists? full-path)
			  (delete-file/record-dependency full-path dependencies)
			  (print-message)]
			 [else (void)])))
		    paths)))

      (define re:suffix (regexp "[.].?.?.?$"))

      (when (clean)
	(let ([dependencies (make-hash-table 'equal)])
	  ;; Main deletion:
	  (for-each (lambda (cc)
		      (clean-collection cc dependencies))
		    collections-to-compile)
	  ;; Unless specific collections were named, also
	  ;;  delete .zos for referenced modules:
	  (when (null? x-specific-collections)
	    (setup-printf "Checking dependencies")
	    (let loop ([old-dependencies dependencies])
	      (let ([dependencies (make-hash-table 'equal)]
		    [did-something? #f])
		(hash-table-for-each
		 old-dependencies
		 (lambda (file _)
		   (let-values ([(dir name dir?) (split-path file)])
		     (let ([base-name (regexp-replace re:suffix name "")])
		       (let ([zo (build-path dir "compiled" (format "~a.zo" base-name))]
			     [dep (build-path dir "compiled" (format "~a.dep" base-name))])
			 (when (and (file-exists? dep)
				    (file-exists? zo))
			   (set! did-something? #t)
			   (setup-printf "  deleting ~a" zo)
			   (delete-file/record-dependency zo dependencies)
			   (delete-file/record-dependency dep dependencies)))))))
		(when did-something?
		  (loop dependencies)))))))


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


      (define (do-install-part part)
        (when (or (call-install) (eq? part 'post))
          (for-each
           (lambda (cc)
             (let/ec k
               (record-error
                cc
                (case part
                  [(pre)     "Early Install"]
                  [(general) "General Install"]
                  [(post)    "Post Install"])
                (lambda ()
                  (let ([fn (call-info
                             (cc-info cc)
                             (case part
                               [(pre)     'pre-install-collection]
                               [(general) 'install-collection]
                               [(post)    'post-install-collection])
                             (lambda () (k #f))
                             (lambda (v)
                               (unless (and (string? v)
                                            (relative-path? v))
                                 (error "result is not a relative path string: " v))
                               (let ([p (build-path (cc-path cc) v)])
                                 (unless (file-exists? p)
                                   (error "installer file does not exist: " p)))))])
                    (let ([installer
                           (with-handlers
                               ([not-break-exn?
                                 (lambda (exn)
                                   (error 'setup-plt
                                          "error loading installer: ~a"
                                          (if (exn? exn) (exn-message exn) exn)))])
                             (dynamic-require `(lib ,fn ,@(cc-collection cc)) 
                                              (case part
                                                [(pre)     'pre-installer]
                                                [(general) 'installer]
                                                [(post)    'post-installer])))])
                      (setup-printf "~aInstalling ~a"
                                    (case part [(pre) "Pre-"] [(post) "Post-"] [else ""])
                                    (cc-name cc))
                      (installer plthome)))))))
           collections-to-compile)))

      (do-install-part 'pre)

      (define (make-it desc compile-collection)
	;; Create a fresh namespace to avoid polluting the compilation
	;;  with modules that are already loaded
	(for-each (lambda (cc)
		    (record-error
		     cc
		     (format "Compiling ~a" desc)
		     (lambda ()
		       (unless (control-io-apply 
				(case-lambda 
				 [(p) 
				  ;; Main "doing something" message
				  (setup-fprintf p "Compiling ~a used by ~a" 
						 desc (cc-name cc))]
				 [(p where)
				  ;; Doing something specifically in "where"
				  (setup-fprintf p "  in ~a" 
						 (path->complete-path
						  where
						  (cc-path cc)))])
				compile-collection
				(cc-collection cc))
			 (setup-printf "No more ~a to compile for ~a" 
				       desc (cc-name cc)))
		       (collect-garbage))))
		  collections-to-compile))

      (when (make-zo) (make-it ".zos" compile-collection-zos))
      (when (make-so) (make-it "extensions" compile-collection-extension))

      (when (make-launchers)
	(let ([name-list
	       (lambda (l)
		 (unless (and (list? l) (andmap (lambda (x) (and (string? x) (relative-path? x))) l))
		   (error "result is not a list of relative path strings:" l)))]
              [flags-list
               (lambda (l)
                 (unless (and (list? l) (andmap (lambda (fs) (andmap string? fs)) l))
                   (error "result is not a list of strings:" l)))]
              [or-f (lambda (f) (lambda (x) (when x (f x))))])
	  (for-each
	   (lambda (cc)
	     (record-error
	      cc
	      "Launcher Setup"
	      (lambda ()
		(let* ([info (cc-info cc)]
		       [make-launcher
			(lambda (kind
				 launcher-names
				 launcher-libraries
				 launcher-flags
				 program-launcher-path
				 make-launcher
				 up-to-date?)
			  (let ([mzlns (call-info info launcher-names (lambda () null) name-list)]
				[mzlls (call-info info launcher-libraries (lambda () #f) (or-f name-list))]
				[mzlfs (call-info info launcher-flags (lambda () #f) (or-f flags-list))])
                            (cond
                             [(null? mzlns) (void)]
                             [(not (or mzlls mzlfs))
                              (unless (null? mzlns)
                                (setup-printf
				 "Warning: ~a launcher name list ~s has no matching library/flags lists"
				 kind mzlns))]
                             [(and (or (not mzlls) (= (length mzlns) (length mzlls)))
                                   (or (not mzlfs) (= (length mzlns) (length mzlfs))))
                              (for-each
                               (lambda (mzln mzll mzlf)
                                 (let ([p (program-launcher-path mzln)]
                                       [aux (cons `(exe-name . ,mzln)
                                                  (build-aux-from-path
                                                   (build-path (apply collection-path (cc-collection cc))
                                                               (regexp-replace "[.]..?.?$"
                                                                               (or mzll mzln) ""))))])
                                   (unless (up-to-date? p aux)
                                     (setup-printf "Installing ~a~a launcher ~a"
                                                   kind (if (eq? (current-launcher-variant) 'normal)
                                                          ""
                                                          (current-launcher-variant))
                                                   p)
                                     (make-launcher
                                      (or mzlf
                                          (if (= 1 (length (cc-collection cc)))
                                            ;; Common case (simpler parsing for Windows to
                                            ;; avoid cygwin bug):
                                            (list "-qmvL-" mzll (car (cc-collection cc)))
                                            (list "-qmve-"
                                                  (format "~s" `(require (lib ,mzll ,@(cc-collection cc)))))))
                                      p
                                      aux))))
                               mzlns
                               (or mzlls (map (lambda (_) #f) mzlns))
                               (or mzlfs (map (lambda (_) #f) mzlns)))]
                             [else
                              (let ([fault (if (or (not mzlls) (= (length mzlns) (length mzlls))) 'f 'l)])
                                (setup-printf
                                 "Warning: ~a launcher name list ~s doesn't match ~a list; ~s"
                                 kind mzlns
                                 (if (eq? 'l fault) "library" "flags")
                                 (if (eq? fault 'l) mzlls mzlfs)))])))])
		  (for-each
		   (lambda (variant)
		     (parameterize ([current-launcher-variant variant])
		       (make-launcher
			"MrEd"
			'mred-launcher-names
			'mred-launcher-libraries
			'mred-launcher-flags
			mred-program-launcher-path
			make-mred-launcher
			mred-launcher-up-to-date?)))
		   (available-mred-variants))
		  (for-each
		   (lambda (variant)
		     (parameterize ([current-launcher-variant variant])
		       (make-launcher
			"MzScheme"
			'mzscheme-launcher-names
			'mzscheme-launcher-libraries
			'mzscheme-launcher-flags
			mzscheme-program-launcher-path
			make-mzscheme-launcher
			mzscheme-launcher-up-to-date?)))
		   (available-mzscheme-variants))))))
	   collections-to-compile)))

      (do-install-part 'general)
      (do-install-part 'post)

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
