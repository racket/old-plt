(unit/sig zodiac:scheme-units^
  (import zodiac:misc^ (z : zodiac:structures^)
    (z : zodiac:scanner-parameters^)
    (z : zodiac:reader-structs^)
    (z : zodiac:reader-code^)
    zodiac:sexp^ (pat : zodiac:pattern^) zodiac:scheme-core^
    zodiac:scheme-main^ zodiac:back-protocol^
    zodiac:expander^ zodiac:interface^)

  (define-struct (unit-form struct:parsed)
    (imports exports clauses))

  (define-struct (compound-unit-form struct:parsed)
    (imports links exports))

  (define-struct (invoke-unit-form struct:parsed)
    (unit variables))

  (define-struct (invoke-open-unit-form struct:parsed)
    (unit name-specifier variables))

  (define create-unit-form
    (lambda (imports exports clauses source)
      (make-unit-form (z:zodiac-origin source)
	(z:zodiac-start source) (z:zodiac-finish source)
	(make-empty-back-box)
	imports exports clauses)))

  (define create-compound-unit-form
    (lambda (imports links exports source)
      (make-compound-unit-form (z:zodiac-origin source)
	(z:zodiac-start source) (z:zodiac-finish source)
	(make-empty-back-box)
	imports links exports)))

  (define create-invoke-unit-form
    (lambda (unit variables source)
      (make-invoke-unit-form (z:zodiac-origin source)
	(z:zodiac-start source) (z:zodiac-finish source)
	(make-empty-back-box)
	unit variables)))

  (define create-invoke-open-unit-form
    (lambda (unit name-specifier variables source)
      (make-invoke-open-unit-form (z:zodiac-origin source)
	(z:zodiac-start source) (z:zodiac-finish source)
	(make-empty-back-box)
	unit name-specifier variables)))

  ; --------------------------------------------------------------------

  (define c-unit-link-import/body-vocab-attr 'c-unit-link-import/body-vocab)

  (define put-c-unit-vocab-attribute
    (lambda (attributes vocab)
      (put-attribute attributes c-unit-link-import/body-vocab-attr
	(cons vocab
	  (get-attribute attributes c-unit-link-import/body-vocab-attr
	    (lambda () null))))))

  (define get-c-unit-vocab-attribute
    (lambda (attributes)
      (car
	(get-attribute attributes c-unit-link-import/body-vocab-attr))))

  (define remove-c-unit-vocab-attribute
    (lambda (attributes)
      (put-attribute attributes c-unit-link-import/body-vocab-attr
	(cdr (get-attribute attributes c-unit-link-import/body-vocab-attr)))))

  (define make-vars-attribute
    (lambda (attributes)
      (put-attribute attributes 'unit-vars
	(cons (make-hash-table)
	  (get-attribute attributes 'unit-vars (lambda () '()))))))

  (define get-vars-attribute
    (lambda (attributes)
      (car (get-attribute attributes 'unit-vars))))

  (define remove-vars-attribute
    (lambda (attributes)
      (put-attribute attributes 'unit-vars
	(cdr (get-attribute attributes 'unit-vars)))))

  (define make-unresolved-attribute
    (lambda (attributes)
      (put-attribute attributes 'unresolved-unit-vars
	(cons '()
	  (get-attribute attributes
	    'unresolved-unit-vars (lambda () '()))))))

  (define get-unresolved-attribute
    (lambda (attributes)
      (car (get-attribute attributes 'unresolved-unit-vars))))

  (define update-unresolved-attribute
    (lambda (attributes new-value)
      (let ((current (get-attribute attributes 'unresolved-unit-vars
		       (lambda () '(()))))) ; List of lists to accomodate
					; nested units
	(unless (null? current)
	  (put-attribute attributes 'unresolved-unit-vars
	    (cons
	      (cons new-value (car current))
	      (cdr current)))))))

  (define remove-unresolved-attribute
    (lambda (attributes)
      (put-attribute attributes 'unresolved-unit-vars
	(cdr (get-attribute attributes 'unresolved-unit-vars)))))

  ; --------------------------------------------------------------------

  (define-struct import-id (id))
  (define-struct export-id (id))
  (define-struct internal-id (id))
  (define-struct link-id (id))

  (define register-links
    (lambda (ids attributes)
      (map
	(lambda (id)
	  (let ((id-table (get-vars-attribute attributes))
		 (id-name (z:read-object id)))
	    (let ((entry (hash-table-get id-table id-name
			   (lambda () #f))))
	      (cond
		((not entry)
		  (hash-table-put! id-table id-name
		    (make-link-id id)))
		((link-id? entry)
		  (static-error id "Duplicate link name"))
		(else
		  (internal-error entry "Invalid in register-links"))))))
	ids)))

  (define check-link
    (lambda (id attributes)
      (let ((id-table (get-vars-attribute attributes))
	     (id-name (z:read-object id)))
	(let ((entry (hash-table-get id-table id-name
		       (lambda () #f))))
	  (link-id? entry)))))

  (define check-import
    (lambda (id attributes)
      (let ((id-table (get-vars-attribute attributes))
	     (id-name (z:read-object id)))
	(let ((entry (hash-table-get id-table id-name
		       (lambda () #f))))
	  (import-id? entry)))))

  (define register-import
    (lambda (id attributes)
      (let ((id-table (get-vars-attribute attributes))
	     (id-name (z:read-object id)))
	(let ((entry (hash-table-get id-table id-name
		       (lambda () #f))))
	  (cond
	    ((not entry)
	      (hash-table-put! id-table id-name
		(make-import-id id)))
	    ((import-id? entry)
	      (static-error id "Duplicate import identifier ~a" id-name))
	    ((export-id? entry)
	      (static-error id "Exported identifier ~a being imported"
		id-name))
	    ((internal-id? entry)
	      (static-error id
		"Defined identifier ~a being imported" id-name))
	    (else
	      (internal-error entry
		"Invalid in register-import/export")))))))
    
  (define register-definitions
    (lambda (ids attributes)
      (map
	(lambda (id)
	  (let ((id-table (get-vars-attribute attributes))
		 (id-name (z:read-object id)))
	    (let ((entry (hash-table-get id-table id-name
			   (lambda () #f))))
	      (cond
		((not entry)
		  (hash-table-put! id-table id-name
		    (make-internal-id id)))
		((import-id? entry)
		  (static-error id "Redefined imported identifier ~a" id-name))
		((export-id? entry)
		  'do-nothing)
		((internal-id? entry)
		  (static-error id "Duplicate internal definition for ~a"
		    id-name))
		(else
		  (internal-error entry
		    "Invalid entry in register-definition"))))))
	ids)))

  (define register-export
    (lambda (id attributes)
      (let ((id-table (get-vars-attribute attributes))
	     (id-name (z:read-object id)))
	(let ((entry (hash-table-get id-table id-name
		       (lambda () #f))))
	  (cond
	    ((not entry)
	      (static-error id "Exported identifier ~a not defined"
		id-name))
	    ((import-id? entry)
	      (static-error id "Imported identifier ~a being exported"
		id-name))
	    ((export-id? entry)
	      (static-error id "Duplicate export identifier ~a" id-name))
	    ((internal-id? entry)
	      (hash-table-put! id-table id-name
		(make-export-id id)))
	    (else
	      (internal-error entry
		"Invalid in register-import/export")))))))

  (define check-unresolved-vars
    (lambda (attributes)
      (let ((id-table (get-vars-attribute attributes))
	     (unresolveds (get-unresolved-attribute attributes)))
	(map (lambda (uid)
	       (let ((entry (hash-table-get id-table
			      (z:read-object uid) (lambda () #f))))
		 (cond
		   ((or (internal-id? entry) (export-id? entry))
		     'do-nothing)
		   ((not entry)
		     (static-error uid
		       "Reference to undefined identifier ~a"
		       (z:read-object uid)))
		   (else
		     (internal-error entry
		       "Invalid in check-unresolved-vars")))))
	  unresolveds))))

  ; ----------------------------------------------------------------------

  (define c/imports-vocab
    (create-vocabulary 'c/imports-vocab #f
      "Invalid import declaration"
      "Invalid import declaration"
      "Invalid import declaration"
      "Invalid import declaration"))

  (add-sym-micro c/imports-vocab
    (lambda (expr env attributes vocab)
      (register-import expr attributes)
      (create-lexical-binding+marks expr)))

  ; ----------------------------------------------------------------------

  (define unit-exports-vocab
    (create-vocabulary 'unit-exports-vocab #f
      "Invalid export declaration"
      "Invalid export declaration"
      "Invalid export declaration"
      "Invalid export declaration"))

  (add-sym-micro unit-exports-vocab
    (lambda (expr env attributes vocab)
      (register-export expr attributes)
      (let ((expand-vocab (get-attribute attributes 'exports-expand-vocab)))
	(cons (expand-expr expr env attributes expand-vocab)
	  expr))))

  (add-list-micro unit-exports-vocab
    (let* ((kwd '())
	    (in-pattern '(internal-id external-id))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((internal (pat:pexpand 'internal-id p-env kwd))
		     (external (pat:pexpand 'external-id p-env kwd)))
		(valid-syntactic-id? internal)
		(valid-syntactic-id? external)
		(register-export internal attributes)
		(let ((expand-vocab (get-attribute attributes
				      'exports-expand-vocab)))
		  (cons (expand-expr internal env attributes expand-vocab)
		    external)))))
	  (else
	    (static-error expr "Malformed export declaration"))))))

  (add-primitivized-micro-form 'unit scheme-vocabulary
    (let* ((kwd `(import export))
	    (in-pattern `(_
			   (import imports ...)
			   (export exports ...)
			   clauses ...))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((top-level? (get-top-level-status attributes))
		     (old-top-level
		       (get-attribute attributes 'top-levels))
		     (unit-clauses-vocab
		       (append-vocabulary unit-clauses-vocab-delta
			 vocab 'unit-clauses-vocab)))
		(set-top-level-status attributes #t)
		(when old-top-level
		  (put-attribute attributes 'top-levels (make-hash-table)))
		(let ((in:imports (pat:pexpand '(imports ...) p-env kwd))
		       (in:exports (pat:pexpand '(exports ...) p-env kwd))
		       (in:clauses (pat:pexpand '(clauses ...) p-env kwd)))
		  (make-vars-attribute attributes)
		  (make-unresolved-attribute attributes)
		  (let*
		    ((proc:imports (map (lambda (e)
					  (expand-expr e env
					    attributes c/imports-vocab))
				     in:imports))
		      (_ (extend-env proc:imports env))
		      (proc:clauses (map (lambda (e)
					   (expand-expr e env
					     attributes
					     unit-clauses-vocab))
				      in:clauses))
		      (_ (put-attribute attributes 'exports-expand-vocab
			   unit-clauses-vocab))
		      (proc:exports (map (lambda (e)
					   (expand-expr e env
					     attributes
					     unit-exports-vocab))
				      in:exports))
		      (_ (retract-env (map car proc:imports) env)))
		    (check-unresolved-vars attributes)
		    (remove-vars-attribute attributes)
		    (remove-unresolved-attribute attributes)
		    (when old-top-level
		      (put-attribute attributes 'top-levels old-top-level))
		    (set-top-level-status attributes top-level?)
		    (create-unit-form
		      (map car proc:imports)
		      proc:exports
		      proc:clauses expr))))))
	  (else
	    (static-error expr "Malformed unit"))))))

  ; ----------------------------------------------------------------------

  (define c-unit-link-import-vocab
    (create-vocabulary 'c-unit-link-import-vocab #f
      "Invalid link import declaration"
      "Invalid link import declaration"
      "Invalid link import declaration"
      "Invalid link import declaration"))

  (add-sym-micro c-unit-link-import-vocab
    (lambda (expr env attributes vocab)
      (if (check-import expr attributes)
	(list (expand-expr expr env attributes
		(get-c-unit-vocab-attribute attributes)))
	(static-error expr "~a: Not an imported identifier"
	  (z:read-object expr)))))

  (add-list-micro c-unit-link-import-vocab
    (let* ((kwd '())
	    (in-pattern '(tag id ...))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((tag (pat:pexpand 'tag p-env kwd))
		     (ids (pat:pexpand '(id ...) p-env kwd)))
		(map (lambda (id) (cons tag id)) ids))))
	  (else
	    (static-error expr "Invalid link syntax"))))))

  (define c-unit-link-body-vocab
    (create-vocabulary 'c-unit-link-body-vocab #f
      "Invalid link body declaration"
      "Invalid link body declaration"
      "Invalid link body declaration"
      "Invalid link body declaration"))

  (add-list-micro c-unit-link-body-vocab
    (let* ((kwd '())
	    (in-pattern '(sub-unit-expr imported-var ...))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((sub-unit-expr (pat:pexpand 'sub-unit-expr p-env kwd))
		     (imported-vars
		       (pat:pexpand '(imported-var ...) p-env kwd)))
		(cons (expand-expr sub-unit-expr env attributes
			(get-c-unit-vocab-attribute attributes))
		  (map (lambda (imported-var)
			 (expand-expr imported-var env attributes
			   c-unit-link-import-vocab))
		    imported-vars)))))
	  (else
	    (static-error expr "Invalid linkage body"))))))

  (define c-unit-exports-vocab
    (create-vocabulary 'c-unit-exports-vocab #f
      "Invalid unit export declaration"
      "Invalid unit export declaration"
      "Invalid unit export declaration"
      "Invalid unit export declaration"))

  (add-sym-micro c-unit-exports-vocab
    (lambda (expr env attributes vocab)
      (cons expr expr)))

  (add-list-micro c-unit-exports-vocab
    (let* ((kwd '())
	    (in-pattern '(internal-id external-id))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((internal-id (pat:pexpand 'internal-id p-env kwd))
		     (external-id (pat:pexpand 'external-id p-env kwd)))
		(valid-syntactic-id? internal-id)
		(valid-syntactic-id? external-id)
		(cons internal-id external-id))))
	  (else
	    (static-error expr "Invalid export clause"))))))

  (define c-unit-export-clause-vocab
    (create-vocabulary 'c-unit-export-clause-vocab #f
      "Invalid export clause declaration"
      "Invalid export clause declaration"
      "Invalid export clause declaration"
      "Invalid export clause declaration"))

  (add-list-micro c-unit-export-clause-vocab
    (let* ((kwd '())
	    (in-pattern '(tag exports ...))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((tag (pat:pexpand 'tag p-env kwd))
		     (exports (pat:pexpand '(exports ...) p-env kwd)))
		(valid-syntactic-id? tag)
		(if (check-link tag attributes)
		  (map (lambda (e)
			 (cons tag
			   (expand-expr e env attributes
			     c-unit-exports-vocab)))
		    exports)
		  (static-error tag "Not a valid tag")))))
	  (else
	    (static-error expr "Invalid export clause"))))))

  (add-primitivized-micro-form 'compound-unit scheme-vocabulary
    (let* ((kwd `(import link export))
	    (in-pattern `(_
			   (import imports ...)
			   (link
			     (link-tag link-body) ...)
			   (export export-clause ...)))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((in:imports (pat:pexpand '(imports ...) p-env kwd))
		     (in:link-tags (pat:pexpand '(link-tag ...) p-env kwd))
		     (in:link-bodies
		       (pat:pexpand '(link-body ...) p-env kwd))
		     (in:export-clauses
		       (pat:pexpand '(export-clause ...) p-env kwd)))
		(distinct-valid-syntactic-id/s? in:link-tags)
		(make-vars-attribute attributes)
		(put-c-unit-vocab-attribute attributes vocab)
		(let*
		  ((top-level? (get-top-level-status attributes))
		    (_ (set-top-level-status attributes))
		    (proc:imports (map (lambda (e)
					 (expand-expr e env
					   attributes c/imports-vocab))
				    in:imports))
		    (_ (extend-env proc:imports env))
		    (_ (register-links in:link-tags attributes))
		    (raw-link-clauses (map z:read-object in:link-tags))
		    (proc:link-clauses
		      (map (lambda (link-tag link-body)
			     (let ((expanded-body
				     (expand-expr link-body env
				       attributes
				       c-unit-link-body-vocab)))
			       (let ((unit-expr (car expanded-body))
				      (unit-args (apply append
						   (cdr expanded-body)))
				      (this-tag (z:read-object link-tag)))
				 (let loop ((args unit-args))
				   (if (null? args)
				     (cons link-tag
				       (cons unit-expr unit-args))
				     (begin
				       (if (pair? (car args))
					 (let ((arg (caar args)))
					   (if (z:symbol? arg)
					     (let ((arg-name
						     (z:read-object arg)))
					       (if (eq? this-tag arg-name)
						 (static-error arg
						   "Self-import not allowed")
						 (if (not (memq arg-name
							    raw-link-clauses))
						   (static-error arg
						     "Not a valid tag"))))
					     (static-error arg
					       "Tag must be a symbol"))))
				       (loop (cdr args))))))))
			in:link-tags in:link-bodies))
		    (proc:export-clauses
		      (apply append
			(map (lambda (e)
			       (expand-expr e env
				 attributes c-unit-export-clause-vocab))
			  in:export-clauses)))
		    (_ (retract-env (map car proc:imports) env)))
		  (set-top-level-status attributes top-level?)
		  (remove-c-unit-vocab-attribute attributes)
		  (remove-vars-attribute attributes)
		  (create-compound-unit-form
		    (map car proc:imports)
		    proc:link-clauses
		    proc:export-clauses
		    expr)))))
	  (else
	    (static-error expr "Malformed compound-unit"))))))

  ; --------------------------------------------------------------------

  (add-primitivized-micro-form 'invoke-unit scheme-vocabulary
    (let* ((kwd '())
	    (in-pattern `(_ unit vars ...))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((unit (pat:pexpand 'unit p-env kwd))
		     (vars (pat:pexpand '(vars ...) p-env kwd)))
		(valid-syntactic-id/s? vars)
		(let* ((top-level? (get-top-level-status
				     attributes))
			(_ (set-top-level-status attributes))
			(expr-expr
			  (expand-expr unit env attributes vocab))
			(var-exprs
			  (map (lambda (e)
				 (expand-expr e env
				   attributes vocab))
			    vars)))
		  (set-top-level-status attributes top-level?)
		  (create-invoke-unit-form
		    expr-expr
		    var-exprs
		    expr)))))
	  (else
	    (static-error expr "Malformed invoke-unit"))))))

  (add-primitivized-micro-form 'invoke-open-unit scheme-vocabulary
    (let* ((kwd '())
	    (in-pattern-1 `(_ unit))
	    (in-pattern-2 `(_ unit name-spec vars ...))
	    (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	    (m&e-2 (pat:make-match&env in-pattern-2 kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e-1 expr env)
	    =>
	    (lambda (p-env)
	      (let ((unit (pat:pexpand 'unit p-env kwd))
		     (name-spec (pat:pexpand 'name-spec p-env kwd)))
		(create-invoke-open-unit-form
		  (expand-expr unit env attributes vocab)
		  #f '() expr))))
	  ((pat:match-against m&e-2 expr env)
	    =>
	    (lambda (p-env)
	      (let ((unit (pat:pexpand 'unit p-env kwd))
		     (name-spec (pat:pexpand 'name-spec p-env kwd))
		     (vars (pat:pexpand '(vars ...) p-env kwd)))
		(valid-syntactic-id/s? vars)
		(let* ((top-level? (get-top-level-status
				     attributes))
			(_ (set-top-level-status attributes))
			(expr-expr
			  (expand-expr unit env attributes vocab))
			(expanded-spec
			  (if (or (z:symbol? name-spec)
				(and (z:boolean? name-spec)
				  (not (z:read-object name-spec))))
			    (z:read-object name-spec)
			    (static-error name-spec
			      "Invalid name specifier")))
			(vars-expr
			  (map (lambda (v)
				 (expand-expr v env
				   attributes vocab))
			    vars)))
		  (set-top-level-status attributes top-level?)
		  (create-invoke-open-unit-form
		    expr-expr
		    expanded-spec
		    vars-expr
		    expr)))))
	  (else
	    (static-error expr "Malformed invoke-open-unit"))))))

  ; --------------------------------------------------------------------

  (extend-parsed->raw unit-form?
    (lambda (expr p->r)
      `(unit (import ,@(map p->r (unit-form-imports expr)))
	 (export ,@(map (lambda (e)
			  `(,(p->r (car e)) ,(sexp->raw (cdr e))))
		     (unit-form-exports expr)))
	 ,@(map p->r (unit-form-clauses expr)))))

  (extend-parsed->raw compound-unit-form?
    (lambda (expr p->r)
      `(compound-unit
	 (import ,@(map p->r (compound-unit-form-imports expr)))
	 (link
	   ,@(map (lambda (link-clause)
		    (let ((tag (car link-clause))
			   (sub-unit (cadr link-clause))
			   (imports (map (lambda (import)
					   (if (lexical-varref? import)
					     (p->r import)
					     `(,(sexp->raw (car import))
						,(sexp->raw (cdr import)))))
				      (cddr link-clause))))
		      `(,(sexp->raw tag)
			 (,(p->r sub-unit)
			   ,@imports))))
	       (compound-unit-form-links expr)))
	 (export
	   ,@(map (lambda (export-clause)
		    `(,(sexp->raw (car export-clause))
		       (,(sexp->raw (cadr export-clause))
			 ,(sexp->raw (cddr export-clause)))))
	       (compound-unit-form-exports expr))))))

  (extend-parsed->raw invoke-unit-form?
    (lambda (expr p->r)
      `(invoke-unit ,(p->r (invoke-unit-form-unit expr))
	 ,@(map p->r (invoke-unit-form-variables expr)))))

  (extend-parsed->raw invoke-open-unit-form?
    (lambda (expr p->r)
      (if (null? (invoke-open-unit-form-name-specifier expr))
	`(invoke-open-unit ,(p->r (invoke-open-unit-form-unit expr)))
	`(invoke-open-unit ,(p->r (invoke-open-unit-form-unit expr))
	   ,(invoke-open-unit-form-name-specifier expr)
	   ,@(map p->r (invoke-open-unit-form-variables expr))))))

  ; ----------------------------------------------------------------------

  (define unit-clauses-vocab-delta
    (create-vocabulary 'unit-clauses-vocab-delta))

  (let* ((kwd '())
	  (in-pattern-1 `(_ (var ...) val))
	  (m&e-1 (pat:make-match&env in-pattern-1 kwd)))
    (let ((define-values-helper
	    (lambda (handler)
	      (lambda (expr env attributes vocab)
		(unless (at-top-level? attributes)
		  (static-error expr "Not at top-level"))
		(cond
		  ((pat:match-against m&e-1 expr env)
		    =>
		    (lambda (p-env)
		      (let* ((top-level? (get-top-level-status
					   attributes))
			      (_ (set-top-level-status
				   attributes))
			      (vars (pat:pexpand '(var ...)
				      p-env kwd))
			      (_ (map valid-syntactic-id? vars))
			      (out (handler expr env attributes
				     vocab p-env vars)))
			(set-top-level-status attributes
			  top-level?)
			out)))
		  (else (static-error expr "Malformed define-values")))))))
      (add-primitivized-micro-form 'define-values unit-clauses-vocab-delta
	(define-values-helper
	  (lambda (expr env attributes vocab p-env vars)
	    (register-definitions vars attributes)
	    (let* ((id-exprs (map (lambda (v)
				    (expand-expr v env attributes
				      define-values-id-parse-vocab))
			       vars))
		    (expr-expr (expand-expr
				 (pat:pexpand 'val p-env kwd)
				 env attributes vocab)))
	      (create-define-values-form id-exprs expr-expr expr)))))))

  (define define-values-id-parse-vocab
    (create-vocabulary 'define-values-id-parse-vocab #f
      "Invalid in identifier position"
      "Invalid in identifier position"
      "Invalid in identifier position"
      "Invalid in identifier position"))

  (add-sym-micro define-values-id-parse-vocab
    (lambda (expr env attributes vocab)
      (let ((r (resolve expr env vocab)))
	(cond
	  ((or (macro-resolution? r) (micro-resolution? r))
	    (static-error expr
	      "Invalid use of keyword ~s" (z:symbol-orig-name expr)))
	  ((lexical-binding? r)
	    (create-lexical-varref r expr))
	  ((top-level-resolution? r)
	    (let ((id (z:read-object expr)))
	      (let ((top-level-space (get-attribute attributes 'top-levels)))
		(if top-level-space
		  (begin
		    (let ((ref
			    (create-top-level-varref/bind
			      id
			      (hash-table-get top-level-space id
				(lambda ()
				  (let ((b (box '())))
				    (hash-table-put! top-level-space id b)
				    b)))
			      expr)))
		      (let ((b (top-level-varref/bind-slot ref)))
			(set-box! b (cons ref (unbox b))))
		      ref))
		  (create-top-level-varref id expr)))))
	  (else
	    (internal-error expr
	      "Invalid resolution in unit define-values: ~s" r))))))

  (add-primitivized-micro-form 'set! unit-clauses-vocab-delta
    (let* ((kwd '())
	    (in-pattern `(_ var val))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(let ((p-env (pat:match-against m&e expr env)))
	  (if p-env
	    (let* ((top-level? (get-top-level-status attributes))
		    (_ (set-top-level-status attributes))
		    (var-p (pat:pexpand 'var p-env kwd))
		    (_ (valid-syntactic-id? var-p))
		    (id-expr (expand-expr var-p env attributes vocab))
		    (expr-expr (expand-expr
				 (pat:pexpand 'val p-env kwd)
				 env attributes vocab)))
	      (when (check-import var-p attributes)
		(static-error var-p "Mutating imported identifier"))
	      (set-top-level-status attributes top-level?)
	      (create-set!-form id-expr expr-expr expr))
	    (static-error expr "Malformed set!"))))))

  (add-primitivized-micro-form 'if unit-clauses-vocab-delta
    (let* ((kwd '())
	    (in-pattern-1 `(_ test then))
	    (in-pattern-2 `(_ test then else))
	    (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	    (m&e-2 (pat:make-match&env in-pattern-2 kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e-1 expr env)
	    =>
	    (lambda (p-env)
	      (when (language<=? 'structured)
		(static-error expr "If must have an else clause"))
	      (expand-expr
		(structurize-syntax
		  (pat:pexpand '(if test then (#%void)) p-env kwd)
		  expr '(-1))
		env attributes vocab)))
	  ((pat:match-against m&e-2 expr env)
	    =>
	    (lambda (p-env)
	      (let* ((top-level? (get-top-level-status attributes))
		      (_ (set-top-level-status attributes))
		      (test-exp (expand-expr
				  (pat:pexpand 'test p-env kwd)
				  env attributes vocab))
		      (then-exp (expand-expr
				  (pat:pexpand 'then p-env kwd)
				  env attributes vocab))
		      (else-exp (expand-expr
				  (pat:pexpand 'else p-env kwd)
				  env attributes vocab))
		      (_ (set-top-level-status attributes
			   top-level?)))
		(create-if-form test-exp then-exp else-exp expr))))
	  (else
	    (static-error expr "Malformed if"))))))

  (add-sym-micro unit-clauses-vocab-delta
    (lambda (expr env attributes vocab)
      (let ((r (resolve expr env vocab)))
	(cond
	  ((or (macro-resolution? r) (micro-resolution? r))
	    (static-error expr
	      "Invalid use of keyword ~s" (z:symbol-orig-name expr)))
	  ((lexical-binding? r)
	    (create-lexical-varref r expr))
	  ((top-level-resolution? r)
	    (let ((id (z:read-object expr)))
	      (unless (built-in-name id)
		(update-unresolved-attribute attributes expr))
	      (let ((top-level-space (get-attribute attributes 'top-levels)))
		(if top-level-space
		  (begin
		    (let ((ref
			    (create-top-level-varref/bind
			      id
			      (hash-table-get top-level-space id
				(lambda ()
				  (let ((b (box '())))
				    (hash-table-put! top-level-space id b)
				    b)))
			      expr)))
		      (let ((b (top-level-varref/bind-slot ref)))
			(set-box! b (cons ref (unbox b))))
		      ref))
		  (create-top-level-varref id expr)))))
	  (else
	    (internal-error expr "Invalid resolution in unit delta: ~s" r))))))

  ; --------------------------------------------------------------------

  (include "scm-hanc.ss")

  ; --------------------------------------------------------------------

  (define reference-unit-maker
    (lambda (form-name sig?)
      (add-primitivized-micro-form form-name scheme-vocabulary
	(let* ((kwd '())
		(in-pattern `(_ filename))
		(m&e (pat:make-match&env in-pattern kwd)))
	  (lambda (expr env attributes vocab)
	    (cond
	      ((pat:match-against m&e expr env)
		=>
		(lambda (p-env)
		  (let ((filename (pat:pexpand 'filename p-env kwd)))
		    (let ((f (expand-expr filename env attributes vocab)))
		      (if (and (quote-form? f)
			    (z:string? (quote-form-expr f)))
			(expand-expr
			  (structurize-syntax
			    `(let ((result (#%load/use-compiled
					     ,(quote-form-expr f))))
			       (unless (,(if sig?
					   '#%unit/sig?
					   '#%unit?)
					 result)
				 (#%raise
				   (,(if sig?
				       '#%make-exn:unit:signature:non-signed-unit
				       '#%make-exn:unit:non-unit)
				     ,(format
					"~s: result from ~s is not ~aunit"
					form-name
					(sexp->raw (quote-form-expr f))
					(if sig? "signed " ""))
				     ((debug-info-handler))
				     result)))
			       result)
			    expr)
			  env attributes vocab)
			(static-error filename
			  "Does not yield a filename"))))))
	      (else
		(static-error expr "Malformed ~a" form-name))))))))

  (reference-unit-maker 'reference-unit #f)
  (reference-unit-maker 'reference-unit/sig #t)

  (define reference-library-unit-maker
    (lambda (form-name sig?)
      (add-primitivized-micro-form form-name scheme-vocabulary
	(let* ((kwd '())
		(in-pattern-1 `(_ filename))
		(in-pattern-2 `(_ filename collection))
		(m&e-1 (pat:make-match&env in-pattern-1 kwd))
		(m&e-2 (pat:make-match&env in-pattern-2 kwd)))
	  (lambda (expr env attributes vocab)
	    (cond
	      ((pat:match-against m&e-1 expr env)
		=>
		(lambda (p-env)
		  (expand-expr
		    (structurize-syntax
		      (pat:pexpand
			`(,form-name filename "standard")
			p-env kwd)
		      expr)
		    env attributes vocab)))
	      ((pat:match-against m&e-2 expr env)
		=>
		(lambda (p-env)
		  (let ((filename (pat:pexpand 'filename p-env kwd))
			 (collection (pat:pexpand 'collection p-env kwd)))
		    (let ((f (expand-expr filename env attributes vocab))
			   (c (expand-expr collection env attributes vocab)))
		      (unless (and (quote-form? f)
				(z:string? (quote-form-expr f)))
			(static-error filename
			  "Does not yield a filename"))
		      (unless (and (quote-form? c)
				(z:string? (quote-form-expr c)))
			(static-error collection
			  "Does not yield a string"))
		      (let ((raw-f (z:read-object (quote-form-expr f)))
			     (raw-c (z:read-object (quote-form-expr c))))
			(unless (relative-path? raw-f)
			  (static-error f
			    "Library path ~s must be a relative path"
			    raw-f))
			(expand-expr
			  (structurize-syntax
			    `(let ((result (#%require-library
					     ,(quote-form-expr f)
					     ,(quote-form-expr c))))
			       (unless (,(if sig?
					   '#%unit/sig?
					   '#%unit?)
					 result)
				 (#%raise
				   (,(if sig?
				       '#%make-exn:unit:signature:non-signed-unit
				       '#%make-exn:unit:non-unit)
				     ,(format
					"~s: result from ~s in collection ~a not a ~aunit"
					form-name
					raw-f
					raw-c
					(if sig? "signed " ""))
				     ((debug-info-handler))
				     result)))
			       result)
			    expr)
			  env attributes vocab))))))
	      (else
		(static-error expr "Malformed ~a" form-name))))))))

  (reference-library-unit-maker 'reference-library-unit #f)
  (reference-library-unit-maker 'reference-library-unit/sig #t)

  )
