(unit/sig zodiac:expander^
  (import
    zodiac:misc^ zodiac:sexp^
    zodiac:structures^
    (z : zodiac:reader-structs^)
    zodiac:scheme-core^
    zodiac:interface^)

  ; ----------------------------------------------------------------------

  (define-struct resolutions (user?))
  (define-struct (micro-resolution struct:resolutions) (rewriter))
  (define-struct (macro-resolution struct:resolutions) (rewriter))

  ; ----------------------------------------------------------------------

  (define-struct vocabulary-record (name this rest))

  (define get-vocabulary-name vocabulary-record-name)

  (define create-vocabulary
    (opt-lambda (name (root #f))
      (let ((h (make-hash-table)))
	(make-vocabulary-record name h root))))

  (define append-vocabulary
    (opt-lambda (new old (name #f))
      (let loop ((new new) (first? #t))
	(let ((name (if (and first? name) name
		      (vocabulary-record-name new))))
	  (make-vocabulary-record name
	    (vocabulary-record-this new)
	    (if (vocabulary-record-rest new)
	      (loop (vocabulary-record-rest new) #f)
	      old))))))

  (define add-micro/macro-form
    (lambda (constructor)
      (lambda (name/s vocab rewriter)
	(let ((v (vocabulary-record-this vocab))
	       (names (if (symbol? name/s) (list name/s) name/s))
	       (r (constructor rewriter)))
	  (map (lambda (n)
		 (hash-table-put! v n r))
	    names)))))

  (define vocab->list
    (lambda (vocab)
      (cons (vocabulary-record-name vocab)
	(hash-table-map cons (vocabulary-record-this vocab)))))

  (define add-micro-form
    (add-micro/macro-form (lambda (r)
			    (make-micro-resolution #f r))))

  (define add-system-macro-form
    (add-micro/macro-form (lambda (r)
			    (make-macro-resolution #f r))))

  (define add-user-macro-form
    (add-micro/macro-form (lambda (r)
			    (make-macro-resolution #t r))))

  (define add-macro-form add-system-macro-form)

  (define list-micro-kwd
    (string->uninterned-symbol "list-expander"))
  (define ilist-micro-kwd
    (string->uninterned-symbol "ilist-expander"))
  (define sym-micro-kwd
    (string->uninterned-symbol "symbol-expander"))
  (define lit-micro-kwd
    (string->uninterned-symbol "literal-expander"))

  (define add-list/sym/lit-micro
    (lambda (kwd)
      (lambda (vocab rewriter)
	(hash-table-put! (vocabulary-record-this vocab)
	  kwd
	  (make-micro-resolution #f rewriter)))))

  (define add-list-micro (add-list/sym/lit-micro list-micro-kwd))
  (define add-ilist-micro (add-list/sym/lit-micro ilist-micro-kwd))
  (define add-sym-micro (add-list/sym/lit-micro sym-micro-kwd))
  (define add-lit-micro (add-list/sym/lit-micro lit-micro-kwd))

  (define get-list/sym/lit-micro
    (lambda (kwd)
      (lambda (vocab)
	(let loop ((vocab vocab))
	  (hash-table-get (vocabulary-record-this vocab)
	    kwd
	    (lambda ()
	      (let ((v (vocabulary-record-rest vocab)))
		(if v
		  (loop v)
		  #f))))))))

  (define get-list-micro (get-list/sym/lit-micro list-micro-kwd))
  (define get-ilist-micro (get-list/sym/lit-micro ilist-micro-kwd))
  (define get-sym-micro (get-list/sym/lit-micro sym-micro-kwd))
  (define get-lit-micro (get-list/sym/lit-micro lit-micro-kwd))

  ; ----------------------------------------------------------------------

  (define expand-expr
    (lambda (expr env attributes vocab)
      ; (printf "Expanding~n") (pretty-print (sexp->raw expr))
      ; (printf "Expanding~n") (pretty-print expr) (newline))
      ;	(printf "Expanding~n") (display expr)
      ; (printf "in ~s~n" (get-vocabulary-name vocab))
      ;	(printf "in vocabulary~n") (print-env vocab)
      ;	(printf "in attributes~n") (hash-table-map attributes cons)
      ;	(printf "in~n") (print-env env) (newline)
      (cond
	((z:symbol? expr)
	  (let ((sym-expander (get-sym-micro vocab)))
	    (cond
	      ((micro-resolution? sym-expander)
		((micro-resolution-rewriter sym-expander)
		  expr env attributes vocab))
	      (sym-expander
		(internal-error expr "Invalid sym expander ~s" sym-expander))
	      (else
		(static-error expr "Invalid symbol syntax")))))
	((or (z:scalar? expr)		; "literals" = scalars - symbols
	   (z:vector? expr))
	  (let ((lit-expander (get-lit-micro vocab)))
	    (cond
	      ((micro-resolution? lit-expander)
		((micro-resolution-rewriter lit-expander)
		  expr env attributes vocab))
	      (lit-expander
		(internal-error expr
		  "Invalid lit expander ~s" lit-expander))
	      (else
		(static-error expr "Invalid scalar/vector syntax")))))
	((z:list? expr)
	  (let ((invoke-list-expander
		  (lambda ()
		    (let ((list-expander (get-list-micro vocab)))
		      (cond
			((micro-resolution? list-expander)
			  ((micro-resolution-rewriter list-expander)
			    expr env attributes vocab))
			(list-expander
			  (internal-error expr
			    "Invalid list expander ~s" list-expander))
			(else
			  (static-error expr "Invalid list syntax"))))))
		 (contents (expose-list expr)))
	    (if (null? contents)
	      (invoke-list-expander)
	      (let ((app-pos (car contents)))
		(if (z:symbol? app-pos)
		  (let ((r (resolve app-pos env vocab)))
		    (cond
		      ((macro-resolution? r)
			(with-handlers (((lambda (e)
					   (and (exn? e)
					     (not (exn:user? e))))
					  (lambda (exn)
					    (internal-error expr
					      "Macro expansion error: ~a"
					      exn))))
			  (let* ((rewriter (macro-resolution-rewriter r))
				  (m (new-mark))
				  (marker (mark-expression m))
				  (rewritten (rewriter expr env))
				  (structurized (structurize-syntax
						  rewritten expr (list m)))
				  (expanded (expand-expr structurized env
					      attributes vocab)))
			    (set-macro-origin expanded app-pos))))
		      ((micro-resolution? r)
			((micro-resolution-rewriter r)
			  expr env attributes vocab))
		      (else
			(invoke-list-expander))))
		  (invoke-list-expander))))))
	((z:improper-list? expr)
	  (let ((ilist-expander (get-ilist-micro vocab)))
	    (cond
	      ((micro-resolution? ilist-expander)
		((micro-resolution-rewriter ilist-expander)
		  expr env attributes vocab))
	      (ilist-expander
		(internal-error expr
		  "Invalid ilist expander ~s" ilist-expander))
	      (else
		(static-error expr "Invalid improper-list syntax")))))
	(else
	  (static-error expr "Invalid body")))))

  (define zodiac-user-parameterization (current-parameterization))

  (define expand
    (opt-lambda (expr attr vocab (params (current-parameterization)))
      (set! zodiac-user-parameterization params)
      (expand-expr expr (make-new-environment) attr vocab)))

  (define expand-program
    (opt-lambda (exprs attr vocab (params (current-parameterization)))
      (set! zodiac-user-parameterization params)
      (put-attribute attr 'top-levels (make-hash-table))
      (map (lambda (expr)
	     (expand-expr expr (make-new-environment) attr vocab))
	exprs)))

  ; ----------------------------------------------------------------------

  (define make-attributes make-hash-table)
  (define put-attribute
    (lambda (table key value)
      (hash-table-put! table key value)
      table))
  (define get-attribute
    (opt-lambda (table key (failure-thunk (lambda () #f)))
      (hash-table-get table key failure-thunk)))

  ; ----------------------------------------------------------------------

  (define introduce-identifier
    (lambda (new-name old-id)
      (z:make-symbol (zodiac-origin old-id)
	(zodiac-start old-id) (zodiac-finish old-id)
	new-name new-name (z:symbol-marks old-id))))

  (define introduce-fresh-identifier
    (lambda (new-name source)
      (z:make-symbol (zodiac-origin source)
	(zodiac-start source) (zodiac-finish source)
	new-name new-name '())))

  (define introduce-bound-id
    (lambda (binding-gen name-gen old-id old-id-marks)
      (let* ((base-name (binding-var old-id))
	      (real-base-name (binding-orig-name old-id))
	      (new-base-name (name-gen real-base-name))
	      (new-name (symbol-append base-name "-init")))
	(let ((s (z:make-symbol (zodiac-origin old-id)
		   (zodiac-start old-id) (zodiac-finish old-id)
		   new-base-name new-base-name old-id-marks)))
	  ((create-binding+marks binding-gen
	     (lambda (_) new-name))
	    s)))))

  ; ----------------------------------------------------------------------

  (define-struct (top-level-resolution struct:resolutions) ())

  ; ----------------------------------------------------------------------

  (define make-new-environment make-hash-table)

  (define resolve
    (lambda (id env vocab)
      (let ((name (z:read-object id)) (marks (z:symbol-marks id)))
	(or (resolve-in-env name marks env)
	  (resolve-in-global name vocab)))))

  (define resolve-in-env
    (lambda (name marks env)
      (let ((v (hash-table-get env name (lambda () #f)))) ; name-eq?
	(and v
	  (let ((w (assoc marks v)))	; marks-equal?
	    (and w (cdr w)))))))

  (define resolve-in-global
    (let ((top-level-resolution (make-top-level-resolution #f))) ; name-eq?
      (lambda (name vocab)
	(let loop ((vocab vocab))
	  (hash-table-get (vocabulary-record-this vocab)
	    name
	    (lambda ()
	      (let ((v (vocabulary-record-rest vocab)))
		(if v
		  (loop v)
		  top-level-resolution))))))))

  (define print-env
    (lambda (env)
      (hash-table-map env (lambda (key value)
			    (printf "~s ->~n" key)
			    (pretty-print value)))))

  ; ----------------------------------------------------------------------

  (define extend-env
    (lambda (new-vars+marks env)
      (for-each
	(lambda (var+marks)
	  (let ((new-var (car var+marks)))
	    (let ((real-name (binding-orig-name new-var)))
	      (hash-table-put! env real-name
		(cons (cons (cdr var+marks) new-var)
		  (hash-table-get env real-name (lambda () '())))))))
	new-vars+marks)))

  (define retract-env
    (lambda (vars env)
      (let ((names (map binding-orig-name vars)))
	(for-each (lambda (name)
		    (hash-table-put! env name
		      (cdr (hash-table-get env name
			     (lambda ()
			       '(internal-error:dummy-for-sake-of-cdr!))))))
	  names))))

  )
