(require-library "unitsig.ss")

(define zodiac:expander@
  (unit/sig zodiac:expander^
    (import
      zodiac:misc^ zodiac:sexp^
      zodiac:structures^
      (z : zodiac:reader-structs^)
      zodiac:scheme-core^
      zodiac:interface^)

    ; ----------------------------------------------------------------------

    (define-struct resolutions ())
    (define-struct (micro-resolution struct:resolutions) (rewriter))
    (define-struct (macro-resolution struct:resolutions) (rewriter))

    ; ----------------------------------------------------------------------

    (define make-vocabulary make-hash-table)

    (define copy-vocabulary
      (lambda (vocab)
	(let ((new-vocab (make-vocabulary)))
	  (hash-table-for-each vocab
	    (lambda (key value)
	      (hash-table-put! new-vocab key value)))
	  new-vocab)))

    (define merge-vocabulary
      (lambda (vocab delta)
	(hash-table-for-each delta
	  (lambda (key value)
	    (hash-table-put! vocab key value)))
	vocab))

    (define add-micro/macro-form
      (lambda (constructor)
	(lambda (name vocab rewriter)
	  (hash-table-put! vocab name
	    (constructor rewriter)))))

    (define add-micro-form
      (add-micro/macro-form make-micro-resolution))

    (define add-macro-form
      (add-micro/macro-form make-macro-resolution))

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
	  (hash-table-put! vocab kwd
	    (make-micro-resolution rewriter)))))

    (define add-list-micro (add-list/sym/lit-micro list-micro-kwd))
    (define add-ilist-micro (add-list/sym/lit-micro ilist-micro-kwd))
    (define add-sym-micro (add-list/sym/lit-micro sym-micro-kwd))
    (define add-lit-micro (add-list/sym/lit-micro lit-micro-kwd))

    (define get-list/sym/lit-micro
      (lambda (kwd)
	(lambda (vocab)
	  (hash-table-get vocab kwd
	    (lambda () #f)))))

    (define get-list-micro (get-list/sym/lit-micro list-micro-kwd))
    (define get-ilist-micro (get-list/sym/lit-micro ilist-micro-kwd))
    (define get-sym-micro (get-list/sym/lit-micro sym-micro-kwd))
    (define get-lit-micro (get-list/sym/lit-micro lit-micro-kwd))

    ; ----------------------------------------------------------------------

    (define expand-expr
      (lambda (expr env attributes vocab)
;	(printf "Expanding~n") (pretty-print (sexp->raw expr))
;	(printf "Expanding~n") (pretty-print expr)
;	(printf "in vocabulary~n") (print-env vocab)
;	(printf "Expanding~n") (pretty-print expr)
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
		  (static-error expr "Invalid syntax")))))
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
		  (static-error expr "Invalid syntax")))))
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
			    (static-error expr "Invalid syntax"))))))
		   (contents (expose-list expr)))
	      (if (null? contents)
		(invoke-list-expander)
		(let ((app-pos (car contents)))
		  (if (z:symbol? app-pos)
		    (let ((r (resolve app-pos env vocab)))
		      (cond
			((macro-resolution? r)
			  (let* ((rewriter (macro-resolution-rewriter r))
				  (m (new-mark))
				  (marker (mark-expression m))
				  (rewritten (rewriter expr env))
				  (structurized (structurize-syntax
						  rewritten expr (list m))))
			    (expand-expr structurized env attributes vocab)))
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
		  (static-error expr "Invalid syntax")))))
	  (else
	    (static-error expr "Invalid body")))))

    (define expand
      (lambda (expr vocab)
	(expand-expr expr (make-new-environment) (make-attributes) vocab)))

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
      (let ((top-level-resolution (make-top-level-resolution)))	; name-eq?
	(lambda (name vocab)
	  (hash-table-get vocab name
	    (lambda () top-level-resolution)))))

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
  )
