  (unit/sig mzlib:compile^
   (import)

   (define identity (lambda (x n) x))

   (define-values (make-reference-unit make-reference) (require-library "referf.ss"))

   ; top-level begin-elaboration-time => begin-expansion-time
   ; nested begin-elaboration-time => begin
   ; reference-XXX => usual expansion w/o string check
   (define -reference-library-unit/sig (make-reference-unit #f #t #t 'reference-library-unit/sig))
   (define -reference-library-unit (make-reference-unit #f #t #f 'reference-library-unit))
   (define -reference-unit/sig (make-reference-unit #f #f #t 'reference-unit/sig))
   (define -reference-unit (make-reference-unit #f #f #f 'reference-unit))
   (define -reference (make-reference #f #f))
   (define -reference-library (make-reference #f #t))
   (define -begin-elaboration-time
     (lambda body
       (let ([expr `(eval (eval (quote (begin ,@body))))])
	 (if (local-expansion-top-level?)
	     `(begin-expansion-time ,expr)
	     expr))))

   (define make-compile-namespace
     (lambda (flags preserve-elab? preserve-constr?)
       (let ([n (apply make-namespace (list* 'no-constants flags))]
	     [gvs (make-global-value-list)])
	 (parameterize ([current-namespace n])
	   (for-each
	    (lambda (gvp)
	      (unless (defined? (car gvp))
		      (eval `(define ,(car gvp) (quote ,(cdr gvp))))))
	    gvs)
	   (when (or preserve-elab? preserve-constr?)
		 (eval `(begin
			  (require-library "refer.ss")
			  (define-macro reference ,-reference)
			  (define-macro reference-unit/sig ,-reference-unit/sig)
			  (define-macro reference-unit ,-reference-unit)
			  (define-macro reference-library-unit/sig ,-reference-library-unit/sig)
			  (define-macro reference-library-unit ,-reference-library-unit)
			  (define-macro reference-library ,-reference-library)
			  ,@(let ([e (if preserve-elab?
					 `((define-macro begin-elaboration-time ,-begin-elaboration-time))
					 null)]
				  [c (if preserve-constr?
					 `((define-macro begin-construction-time ,-begin-elaboration-time))
					 null)])
			      (append e c))))))
			      
	 n)))

   (define compile-file
     (case-lambda 
      [(srcs dest) (compile-file srcs dest null identity)]
      [(srcs dest flags) (compile-file srcs dest flags identity)]
      [(srcs dest flags preprocessor)
       (unless (or (string? srcs) 
		   (input-port? srcs)
		   (and (list? srcs) (andmap (lambda (x) (or (string? x)
							     (input-port? x)))
					     srcs)))
	       (raise-type-error 'compile-file 
				 "string, input-port, or list of strings or input-ports" 
				 srcs))
       (unless (or (string? dest) (output-port? dest))
	       (raise-type-error 'compile-file "string or output-port" dest))
       (unless (and (list flags) 
		    (andmap (lambda (s) 
			      (member s '(ignore-macro-definitions
					  expand-load 
					  use-current-namespace
					  ignore-require-library
					  expand-require-library
					  no-warnings
					  only-expand
					  preserve-elaborations
					  preserve-constructions)))
			    flags))
	       (raise-type-error 'compile-file "list of flag symbols" flags))
       (unless (and (procedure? preprocessor)
		    (procedure-arity-includes? preprocessor 2))
	       (raise-type-error 'compile-file "procedure (arity 2)" preprocessor))
       (let ([do-macros? (not (member 'ignore-macro-definitions flags))]
	     [expand-load? (member 'expand-load flags)]
	     [expand-rl? (member 'expand-require-library flags)]
	     [ignore-rl? (member 'ignore-require-library flags)]
	     [expand-only? (member 'only-expand flags)]
	     [namespace (if (member 'use-current-namespace flags)
			    (current-namespace)
			    (make-compile-namespace
			     (if (built-in-name 'wx:frame%) ; HACK!!!
				 '(wx)
				 null)
			     (member 'preserve-elaborations flags)
			     (member 'preserve-constructions flags)))]
	     [required (make-hash-table)]
	     [warning
	      (lambda (s)
		(unless (member 'no-warnings flags)
			(fprintf (current-error-port)
				 "compile-file warning: ~a~n"
				 s)))])
	 (let ([out (if (output-port? dest)
			dest
			(open-output-file dest 'truncate))])
	   (dynamic-wind
	    void
	    (lambda ()
	      (write
	       `(#%if (#%not (#%string=? (#%version) ,(version)))
		    (#%error (#%quote ,(if (string? srcs)
				       (string->symbol srcs)
				       'compiled-file))
			   ,(string-append
			     "compiled for MzScheme version "
			     (version)
			     ", not ~a")
			   (#%version)))
	       out)
	      (newline out)
	      (let src-loop ([srcs srcs])
		(unless (null? srcs)
		  (let*-values ([(src next-srcs) 
				 (if (list? srcs)
				     (values (car srcs) (cdr srcs))
				     (values srcs null))]
				[(in) (if (input-port? src)
					  src
					  (open-input-file src))])
		    (dynamic-wind
		     void
		     (lambda ()
		       (let loop ([in in])
			 (let ([s (read in)])
			   (if (not (eof-object? s))
			       (let* ([s (let ([p (preprocessor s namespace)])
					   (parameterize ([current-namespace namespace])
							 (expand-defmacro p)))]
				      [do-defmacro
				       (lambda (s)
					 (let ([m (if (pair? (cdr s))
						      (cadr s)
						      #f)])
					   (if (symbol? m)
					       (parameterize ([current-namespace namespace])
							     (eval s)
							     #f)
					       (begin
					     (warning 
					      (format
					       "define-macro expression is ill-formed: ~s"
					       s))
					     #f))))]
				      [do-load
				       (lambda (s cd? rel?)
					 (let ([name (if (pair? (cdr s))
							 (cadr s)
							 #f)])
					   (if (and (string? name)
						    (null? (cddr s)))
					       (let*-values ([(name) (if (and rel?
									    (relative-path? name)
									    (current-load-relative-directory))
								       (build-path (current-load-relative-directory) name)
								       name)]
							     [(base nameonly dir?) (split-path name)]
							     [(cd?) (and cd? 
									 (string? base))]
							     [(orig-dir) (and cd? 
									      (current-directory))])
							    (if cd? 
								(current-directory 
								 base))
							    (let ([in (open-input-file 
								       (if cd? 
									   nameonly 
									   name))])
							      (dynamic-wind
							       void
							       (lambda () 
								 (parameterize ([current-load-relative-directory 
										 (if (string? base)
										     base
										     (current-load-relative-directory))])
								    (loop in))
								 #t)
							       (lambda ()
								 (close-input-port in)
								 (if cd? 
								     (current-directory 
								      orig-dir)))))
							    #t)
					       (begin
						 (warning 
						  (format
						   "load expression is ill-formed or ~a: ~s"
						   "contains an expression for file name"
						   s))
						 #f))))]
				      [find-library
				       (lambda (collection name)
					 (let ([all-paths (current-library-collection-paths)])
					   (let loop ([paths all-paths])
					     (if (null? paths)
						 (error 'compile-file "require-library: collection not found: ~s (in any of ~s)" 
							collection all-paths)
						 (let ([dir (build-path (car paths) collection)])
						   (if (directory-exists? dir)
						       (build-path dir name)
						       (loop (cdr paths))))))))]
				      [do-rl
				       (lambda (s)
					 (if (and (pair? (cdr s))
						  (string? (cadr s))
						  (or (null? (cddr s))
						      (and (pair? (cddr s))
							   (string? (caddr s))
							   (null? (cdddr s)))))
					     (if expand-rl?
						 (let* ([name (cadr s)]
							[collection (if (null? (cddr s))
									"standard"
									(caddr s))]
							[key (string->symbol (string-append collection (string #\null) name))])
						   (if (hash-table-get required key (lambda () #f))
						       #t
						       (let ([fullname (find-library collection name)])
							 (hash-table-put! required key #t)
							 (do-load s #f #t)
							 #t)))
						 (parameterize ([current-namespace namespace])
							       (eval `(require-library ,(cadr s)))
							       #f))
					     (begin
					       (warning 
						(format
						 "require-library expression is ill-formed or ~a: ~s"
						 "contains an expression for library/collection name"
						 s))
					       #f)))]
				      [v-c (if expand-only?
					       s
					       (parameterize ([current-namespace namespace])
							     (compile s)))]
				      [v (if (pair? s)
					     (let ([t (car s)])
					       (case t
						 [(define-macro 
						    define-id-macro 
						    define-expansion-time
						    #%define-macro
						    #%define-id-macro 
						    #%define-expansion-time)
						  (and do-macros? (do-defmacro s))]
						 [(begin-expansion-time
						   #%begin-expansion-time)
						  (when do-macros?
							(parameterize ([current-namespace namespace])
								      (eval s)))
						  #f]
						 [(load #%load)
						  (and expand-load? (do-load s #f #f))]
						 [(load/cd #%load/cd)
						  (and expand-load? (do-load s #t #f))]
						 [(load-relative #%load-relative)
						  (and expand-load? (do-load s #f #t))]
						 [(require-library #%require-library)
						  (and (not ignore-rl?) (do-rl s))]
						 [else #f]))
					     #f)])
				 (if v
				     (void)
				     (write v-c out))
				 (newline out)
				 (loop in))))))
		     (lambda ()
		       (if (input-port? src)
			   (void)
			   (close-input-port in))))
		    (src-loop next-srcs)))))
	    (lambda ()
	      (if (output-port? dest)
		  (void)
		  (close-output-port out))))))])))