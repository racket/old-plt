(module reload mzscheme
  (require (lib "match.ss"))

  ;; assumes that the program has been loaded
  ;; in particular, it doesn't check for loops in
  ;; of imports, or syntactically well-formed modules.
  (provide reload)

  (define cache-name (gensym "reload-cache-name"))

  ;; reload : module-spec exact-positive-integer -> void
  ;; reloads the module or anything it depends on, if
  ;; it has changed.
  (define (reload spec timestamp)
    (reload/cache spec timestamp (get/create-cache))
    (dynamic-require spec #f)
    (void))

  ;; reload/cache : module-spec exact-positive-integer hash-table -> boolean
  ;; reloads the module or anything it depends on,
  ;; using `cache' to map from a filename to the files it depends on.
  (define (reload/cache spec timestamp cache)
    (let* ([module-name (if (symbol? spec)
			    spec
			    ((current-module-name-resolver) spec #f #f))]
	   [module-filename (module-name->filename module-name)])

      ;; since we don't know what to do with top-level defined modules, (without a filename)
      ;; we just assume that they never change (which isn't necessarily true)
      (and module-filename

           (parameterize ([current-directory (let-values ([(base name dir?) (split-path module-filename)])
                                               base)])
             (let ([depends
                    (fetch-from-cache
                     cache
                     module-name
                     (lambda () (calculate-imports module-name module-filename)))])
               (if (or (ormap (lambda (spec) (reload/cache spec timestamp cache))
                              depends)
                       ((file-or-directory-modify-seconds module-filename) . > . timestamp))
                   (begin
                     
                     (parameterize ([current-module-name-prefix
                                     (let-values ([(base _1 _2) (split-path module-filename)])
                                       (string->symbol (string-append "," base)))])
                       (load module-filename))
                     
                     #t)
                   #f))))))

  ;; get/create-cache : -> ht
  ;; returns the current cache table for this namespace
  (define (get/create-cache)
    (with-handlers ([not-break-exn?
		     (lambda (x)
		       (let ([ht (make-hash-table)])
			 (namespace-variable-binding cache-name ht)
			 ht))])
      (namespace-variable-binding cache-name)))

  ;; fetch-from-cache : ht sym (-> TST) -> TST
  ;; returns what ht maps sym to, or if ht doesn't bind sym,
  ;; binds sym to the result of thnk and returns that result.
  (define (fetch-from-cache ht sym thnk)
    (hash-table-get ht sym (lambda ()
			     (let ([v (thnk)])
			       (hash-table-put! ht sym v)
			       v))))

  ;; calculate-imports : symbol -> (listof module-spec)
  ;; determines the list of imports from a module name
  (define (calculate-imports module-name module-filename)
    (let ([short-name (truncate-filename module-filename)])
      (if short-name
	  (let ([module-sexp (find-named-module short-name module-filename)])
	    (if module-sexp
		(get-imports (expand module-sexp))
		null))
	  null)))

  ;; find-named-module : symbol string[filename] -> (union #f `(module ...))
  ;; opens `module-filename' and finds the module whose name is `module-name'
  (define (find-named-module module-name module-filename)
    (call-with-input-file module-filename
      (lambda (port)
	(let loop ()
	  (let ([sexp (read port)])
	    (match sexp
	      [(? eof-object?) #f]
	      [`(module ,this-module-name . ,body)
	       (if (eq? this-module-name module-name)
		   sexp
		   (loop))]
	      [else (loop)]))))))

  ;; trunate-filename : string -> (union #f symbol)
  ;; returns the module name that corresponds to `filename'
  (define re:filename "(.*)\\.((scm)|(ss))")
  (define (truncate-filename filename)
    (let-values ([(base name dir?) (split-path filename)])
      (let ([m (regexp-match re:filename name)])
	(and m
	     (string->symbol (cadr m))))))

  ;; module-name->filename : symbol -> (union #f string[normalized-path])
  ;; given a symbol naming a module, returns the corresponding
  ;; filename it was loaded from (checks for .scm, .ss, and just raw names)
  ;; or #f, if there isn't a filename recoverable.
  (define (module-name->filename module-name)
    (let ([str (symbol->string module-name)])
      (cond
	[(or (string=? str "")
	     (not (char=? #\, (string-ref str 0))))
	 #f]
	[else
	 (let* ([base (substring str 1 (string-length str))]
		[test
		 (lambda (ext)
		   (let ([f (string-append base ext)])
		     (and (file-exists? f)
			  f)))])
	   (or (test ".scm")
	       (test ".ss")
	       (test "")))])))

  ;; get-imports : syntax[expanded-module] -> (listof module-spec)
  ;; searches the body of the expanded module for `require' and
  ;; `require-for-syntax' and 
  (define (get-imports stx)
    (let ([sexp (syntax-object->datum stx)])
      (match sexp
	[`(module ,name mzscheme (#%module-begin ,@bodies))
         (let ([pre-specs
                (let loop ([bodies bodies])
                  (cond
                    [(null? bodies) null]
                    [else (match (car bodies)
                            [`(require ,@require-specs)
                             (cons require-specs (loop (cdr bodies)))]
                            [`(require-for-syntax ,@require-specs)
                             (cons require-specs (loop (cdr bodies)))]
                            [else (loop (cdr bodies))])]))])
           (map extract-module-name-from-require-spec (apply append pre-specs)))]
	[else
	 (error 'get-imports "~s" sexp)])))
  
  ;; extract-module-name-from-require-spec : require-spec -> module-name
  (define (extract-module-name-from-require-spec spec)
    (match spec
      [`(prefix ,identifier ,module-name) module-name]
      [`(all-except ,module-name ,@identifier) module-name]
      [`(rename ,module-name ,local-identifer ,exported-identifer) module-name]
      [module-name module-name])))
