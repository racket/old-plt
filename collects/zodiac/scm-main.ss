; $Id: scm-main.ss,v 1.155 1998/06/08 23:07:20 shriram Exp $

(unit/sig zodiac:scheme-main^
  (import zodiac:misc^ zodiac:structures^
    (z : zodiac:scanner-parameters^)
    (z : zodiac:reader-structs^)
    (z : zodiac:reader-code^)
    zodiac:sexp^ (pat : zodiac:pattern^) zodiac:scheme-core^
    zodiac:back-protocol^ zodiac:expander^ zodiac:interface^
    (param : plt:parameters^))

  ; ----------------------------------------------------------------------

  (define-struct (if-form struct:form) (test then else))
  (define-struct (set!-form struct:form) (var val))
  (define-struct (define-values-form struct:form) (vars val))
  (define-struct (let-values-form struct:form) (vars vals body))
  (define-struct (letrec*-values-form struct:form) (vars vals body))
  (define-struct (quote-form struct:form) (expr))
  (define-struct (begin-form struct:form) (bodies))
  (define-struct (begin0-form struct:form) (bodies))
  (define-struct (case-lambda-form struct:form) (args bodies))
  (define-struct (struct-form struct:form) (type super fields))

  ; ----------------------------------------------------------------------

  (define create-const
    (lambda (c s)
      (make-quote-form (zodiac-origin s)
	(zodiac-start s) (zodiac-finish s)
	(make-empty-back-box) c)))

  (add-lit-micro scheme-vocabulary
    (lambda (expr env attributes vocab)
      (create-const expr expr)))

  ; ----------------------------------------------------------------------

  (define create-case-lambda-form
    (lambda (args bodies source)
      (make-case-lambda-form (zodiac-origin source)
	(zodiac-start source) (zodiac-finish source)
	(make-empty-back-box) args bodies)))

  (define create-if-form
    (lambda (test then else source)
      (make-if-form (zodiac-origin source)
	(zodiac-start source) (zodiac-finish source)
	(make-empty-back-box) test then else)))

  (define create-begin-form
    (lambda (bodies source)
      (make-begin-form (zodiac-origin source)
	(zodiac-start source) (zodiac-finish source)
	(make-empty-back-box) bodies)))

  (define create-begin0-form
    (lambda (bodies source)
      (make-begin0-form (zodiac-origin source)
	(zodiac-start source) (zodiac-finish source)
	(make-empty-back-box) bodies)))

  (define create-quote-form
    (lambda (expr source)
      (make-quote-form (zodiac-origin source)
	(zodiac-start source) (zodiac-finish source)
	(make-empty-back-box) expr)))

  (define create-set!-form
    (lambda (var val source)
      (make-set!-form (zodiac-origin source)
	(zodiac-start source) (zodiac-finish source)
	(make-empty-back-box) var val)))

  (define create-define-values-form
    (lambda (vars val source)
      (make-define-values-form (zodiac-origin source)
	(zodiac-start source) (zodiac-finish source)
	(make-empty-back-box) vars val)))

  (define create-let-values-form
    (lambda (vars vals body source)
      (make-let-values-form  (zodiac-origin source)
	(zodiac-start source) (zodiac-finish source)
	(make-empty-back-box) vars vals body)))

  (define create-letrec*-values-form
    (lambda (vars vals body source)
      (make-letrec*-values-form (zodiac-origin source)
	(zodiac-start source) (zodiac-finish source)
	(make-empty-back-box) vars vals body)))

  (define create-struct-form
    (lambda (type super fields source)
      (make-struct-form (zodiac-origin source)
	(zodiac-start source) (zodiac-finish source)
	(make-empty-back-box) type super fields)))

  ; ----------------------------------------------------------------------

  (extend-parsed->raw if-form?
    (lambda (expr p->r)
      `(if ,(p->r (if-form-test expr))
	 ,(p->r (if-form-then expr))
	 ,(p->r (if-form-else expr)))))

  (extend-parsed->raw set!-form?
    (lambda (expr p->r)
      `(set! ,(p->r (set!-form-var expr))
	 ,(p->r (set!-form-val expr)))))

  (extend-parsed->raw define-values-form?
    (lambda (expr p->r)
      `(define-values ,(map p->r (define-values-form-vars expr))
	 ,(p->r (define-values-form-val expr)))))

  (extend-parsed->raw let-values-form?
    (lambda (expr p->r)
      `(let-values
	 ,(map (lambda (vars val)
		 (list (map p->r vars) (p->r val)))
	    (let-values-form-vars expr) (let-values-form-vals expr))
	 ,(p->r (let-values-form-body expr)))))

  (extend-parsed->raw letrec*-values-form?
    (lambda (expr p->r)
      `(letrec*-values 
	 ,(map (lambda (vars val)
		 (list (map p->r vars) (p->r val)))
	    (letrec*-values-form-vars expr) (letrec*-values-form-vals expr))
	 ,(p->r (letrec*-values-form-body expr)))))

  (extend-parsed->raw quote-form?
    (lambda (expr p->r)
      `(quote ,(sexp->raw (quote-form-expr expr)))))

  (extend-parsed->raw begin-form?
    (lambda (expr p->r)
      `(begin ,@(map p->r (begin-form-bodies expr)))))

  (extend-parsed->raw begin0-form?
    (lambda (expr p->r)
      `(begin0 ,@(map p->r (begin0-form-bodies expr)))))

  (extend-parsed->raw case-lambda-form?
    (lambda (expr p->r)
      `(case-lambda
	 ,@(map (lambda (arg body)
		  `(,(p->r arg) ,(p->r body)))
	     (case-lambda-form-args expr)
	     (case-lambda-form-bodies expr)))))

  (extend-parsed->raw struct-form?
    (lambda (expr p->r)
      `(struct
	 ,(if (struct-form-super expr)
	    (list (sexp->raw (struct-form-type expr))
	      (p->r (struct-form-super expr)))
	    (sexp->raw (struct-form-type expr)))
	 ,(map sexp->raw (struct-form-fields expr)))))

  ; ----------------------------------------------------------------------

  (define expr-pattern
    (if (language>=? 'side-effecting)
      '(expr ...)
      '(expr)))

  (define parse-expr
    (lambda (expr env attributes vocab source)
      (if (or (null? expr)
	    (not (null? (cdr expr))))
	(expand-expr (structurize-syntax
		       `(begin ,@expr) source '(-1))
	  env attributes vocab)
	(let ((v (expand-expr (car expr) env attributes vocab)))
	  (if (internal-definition? v)
	    (static-error (car expr)
	      "Internal definition not followed by expression")
	    v)))))

  ; ----------------------------------------------------------------------

  (add-primitivized-micro-form 'case-lambda scheme-vocabulary
    (let* ((kwd `(else))
	    (in-pattern `(_
			   (args ,@expr-pattern)
			   ...))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((args (pat:pexpand '(args ...) p-env kwd))
		     (bodies (pat:pexpand `((,@expr-pattern) ...)
			       p-env kwd)))
		(let*
		  ((top-level? (get-top-level-status attributes))
		    (_ (set-top-level-status attributes))
		    (arglists+exprs
		      (map
			(lambda (arg body)
			  (distinct-valid-syntactic-id/s? arg)
			  (let* ((arglist
				   (expand-expr arg env attributes
				     arglist-decls-vocab))
				  (arg-vars+marks
				    (arglist-vars arglist)))
			    (extend-env arg-vars+marks env)
			    (begin0
			      (cons
				(make-argument-list arglist)
				(parse-expr body env attributes vocab expr))
			      (retract-env (map car arg-vars+marks) env))))
			args bodies)))
		  (set-top-level-status attributes top-level?)
		  (create-case-lambda-form
		    (map car arglists+exprs)
		    (map cdr arglists+exprs)
		    expr)))))
	  (else
	    (static-error expr "Malformed case-lambda"))))))

  (add-primitivized-macro-form 'lambda scheme-vocabulary
    (let* ((kwd '())
	    (in-pattern `(_ args ,@expr-pattern))
	    (out-pattern `(case-lambda
			    (args ,@expr-pattern)))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env)
	(or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	  (static-error expr "Malformed lambda")))))

  (define-struct internal-definition (vars val))

  (define internal-define-vocab-delta
    (create-vocabulary 'internal-define-vocab-delta))

  '(add-sym-micro internal-define-vocab-delta
    (lambda (expr env attributes vocab)
      expr))

  (add-primitivized-micro-form 'define-values internal-define-vocab-delta
    (let* ((kwd '())
	    (in-pattern `(_ (var ...) val))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let* ((vars (pat:pexpand '(var ...) p-env kwd))
		      (_ (map valid-syntactic-id? vars))
		      (val (pat:pexpand 'val p-env kwd)))
		(for-each (lambda (var)
			    (let ((r (resolve var env vocab)))
			      (when (or (micro-resolution? r)
				      (macro-resolution? r))
				(static-error var
				  "Cannot bind keyword ~s"
				  (z:symbol-orig-name var)))))
		  vars)
		(make-internal-definition vars val))))
	  (else
	    (static-error expr
	      "Malformed internal definition"))))))

  (when (language>=? 'side-effecting)
    (add-primitivized-micro-form 'begin scheme-vocabulary
      (let* ((kwd '())
	      (in-pattern `(_ b ...))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env attributes vocab)
	  (cond
	    ((pat:match-against m&e expr env)
	      =>
	      (lambda (p-env)
		(let ((bodies (pat:pexpand '(b ...) p-env kwd)))
		  (if (get-top-level-status attributes)
		    (if (null? bodies)
		      (static-error expr "Malformed begin")
		      (if (null? (cdr bodies))
			(expand-expr (car bodies) env attributes vocab)
			(create-begin-form
			  (map (lambda (e)
				 (expand-expr e env attributes vocab))
			    bodies)
			  expr)))
		    (let*-values
		      (((internal-define-vocab)
			 (append-vocabulary internal-define-vocab-delta
			   vocab 'internal-define-vocab))
			((definitions parsed-first-term rest-terms bindings)
			  (let loop ((seen null) (rest bodies) (bindings null)
				      (vars-seen null))
			    (if (null? rest)
			      (static-error expr
				(if (null? seen)
				  "Malformed begin"
				  (if (null? (cdr seen))
				    "Internal definition not followed by expression"
				    "Internal definitions not followed by expression")))
			      (let ((first (car rest)))
				(let ((e-first
					(expand-expr first env
					  attributes
					  internal-define-vocab)))
				  (if (internal-definition? e-first)
				    (let ((def-vars (internal-definition-vars e-first)))
				      (let* ((new-vars+marks
					       (map create-lexical-binding+marks
						 def-vars)))
					(for-each
					  (lambda (v)
					    (when (memq (z:read-object v)
						    vars-seen)
					      (static-error v
						"Duplicate internally defined identifier ~a"
						(z:read-object v))))
					  def-vars)
					(extend-env new-vars+marks env)
					(loop (cons e-first seen)
					  (cdr rest)
					  (cons new-vars+marks bindings)
					  (append vars-seen
					    (map z:read-object def-vars)))))
				    (values (reverse seen)
				      e-first
				      (cdr rest)
				      bindings))))))))
		      (if (null? definitions)
			(if (null? rest-terms)
			  parsed-first-term
			  (create-begin-form
			    (cons parsed-first-term
			      (map (lambda (e)
				     (expand-expr e env attributes
				       vocab))
				rest-terms))
			    expr))
			(begin0
			  (create-letrec*-values-form
			    (reverse (map (lambda (vars+marks)
					    (map car vars+marks))
				       bindings))
			    (map (lambda (def)
				   (expand-expr (internal-definition-val def)
				     env attributes vocab))
			      definitions)
			    (if (null? rest-terms)
			      parsed-first-term
			      (create-begin-form
				(cons parsed-first-term
				  (map (lambda (e)
					 (expand-expr e env attributes vocab))
				    rest-terms))
				expr))
			    expr)
			  (for-each (lambda (new-vars+marks)
				      (retract-env (map car new-vars+marks) env))
			    bindings))))))))
	    (else
	      (static-error expr "Malformed begin")))))))

  (when (language>=? 'side-effecting)
    (add-primitivized-micro-form 'begin0 scheme-vocabulary
      (let* ((kwd '())
	      (in-pattern `(_ b ...))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env attributes vocab)
	  (cond
	    ((pat:match-against m&e expr env)
	      =>
	      (lambda (p-env)
		(let ((bodies (pat:pexpand '(b ...) p-env kwd))
		       (top-level? (get-top-level-status attributes)))
		  (when (null? bodies)
		    (static-error expr "Malformed begin0"))
		  (set-top-level-status attributes)
		  (let*-values
		    (((internal-define-vocab)
		       (append-vocabulary internal-define-vocab-delta
			 vocab 'internal-define-vocab))
		      ((parsed-return-value-term)
			(expand-expr (car bodies) env attributes vocab))
		      ((definitions parsed-first-term rest-terms bindings)
			(let loop ((seen null) (rest (cdr bodies))
				    (bindings null) (vars-seen null))
			  (if (null? rest)
			    (if (null? seen)
			      (values null #f null null)
			      (static-error expr
				"Internal definitions not followed by expression"))
			    (let ((first (car rest)))
			      (let ((e-first
				      (expand-expr first env
					attributes
					internal-define-vocab)))
				(if (internal-definition? e-first)
				  (let ((def-vars (internal-definition-vars e-first)))
				    (let* ((new-vars+marks
					     (map create-lexical-binding+marks
					       def-vars)))
				      (for-each
					(lambda (v)
					  (when (memq (z:read-object v)
						  vars-seen)
					    (static-error v
					      "Duplicate internally defined identifier ~a"
					      (z:read-object v))))
					def-vars)
				      (extend-env new-vars+marks env)
				      (loop (cons e-first seen)
					(cdr rest)
					(cons new-vars+marks bindings)
					(append vars-seen
					  (map z:read-object def-vars)))))
				  (values (reverse seen)
				    e-first
				    (cdr rest)
				    bindings))))))))
		    (if parsed-first-term
		      (if (null? definitions)
			(create-begin0-form
			  (cons parsed-return-value-term
			    (cons parsed-first-term
			      (map (lambda (e)
				     (expand-expr e env attributes vocab))
				rest-terms)))
			  expr)
			(begin0
			  (create-begin0-form
			    (list parsed-return-value-term
			      (create-letrec*-values-form
				(reverse (map (lambda (vars+marks)
						(map car vars+marks))
					   bindings))
				(map (lambda (def)
				       (expand-expr (internal-definition-val def)
					 env attributes vocab))
				  definitions)
				(if (null? rest-terms)
				  parsed-first-term
				  (create-begin-form
				    (cons parsed-first-term
				      (map (lambda (e)
					     (expand-expr e env attributes vocab))
					rest-terms))
				    expr))
				expr))
			    expr)
			  (set-top-level-status attributes
			    top-level?)
			  (for-each (lambda (new-vars+marks)
				      (retract-env (map car new-vars+marks) env))
			    bindings)))
		      parsed-return-value-term)))))
	    (else
	      (static-error expr "Malformed begin0")))))))

  (add-primitivized-micro-form 'if scheme-vocabulary
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
	      (set-macro-origin
		(expand-expr
		  (structurize-syntax
		    (pat:pexpand '(if test then (#%void)) p-env kwd)
		    expr '(-1))
		  env attributes vocab)
		(syntax-car expr))))
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

  ; Don't "simplify" this.  If replaced with a pattern match, it will
  ; die when passed a quote form whose underlying object is an actual
  ; Scheme value (as opposed to a struct:read), because the matcher
  ; will attempt to extract the source locations of the underlying
  ; object, which will fail in this case.

  (add-primitivized-micro-form 'quote scheme-vocabulary
    (lambda (expr env attributes vocab)
      (if (and (z:list? expr)
	    (= 2 (z:sequence-length expr)))
	(let ((contents (expose-list expr)))
	  (if (and (z:symbol? (car contents))
		(or (eq? 'quote (z:read-object (car contents)))
		  (eq? '#%quote (z:read-object (car contents)))))
	    (if (or (language>=? 'side-effecting)
		  (z:symbol? (cadr contents)))
	      (create-quote-form (cadr contents) expr)
	      (static-error expr "'~e is not a symbol"
		(sexp->raw (cadr contents))))
	    (static-error expr "Malformed quote")))
	(static-error expr "Malformed quote"))))

  (when (language>=? 'side-effecting)
    (add-primitivized-micro-form 'set! scheme-vocabulary
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
		      (id-expr (expand-expr var-p env attributes
				 vocab))
		      (expr-expr (expand-expr
				   (pat:pexpand 'val p-env kwd)
				   env attributes vocab)))
		(set-top-level-status attributes top-level?)
		(create-set!-form id-expr expr-expr expr))
	      (static-error expr "Malformed set!")))))))

  (when (language>=? 'side-effecting)
    (add-primitivized-micro-form 'set!-values scheme-vocabulary
      (let* ((kwd '())
	      (in-pattern '(_ (vars ...) val))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env attributes vocab)
	  (cond
	    ((pat:match-against m&e expr env)
	      =>
	      (lambda (p-env)
		(let* ((vars (pat:pexpand '(vars ...) p-env kwd))
			(val (pat:pexpand 'val p-env kwd)))
		  (map valid-syntactic-id? vars)
		  (let ((new-names (map generate-name vars)))
		    (expand-expr
		      (structurize-syntax
			`(let-values ((,new-names ,val))
			   ,@(map (lambda (var new-name)
				    `(set! ,var ,new-name))
			       vars new-names)
			   (#%void))
			expr '(-1))
		      env attributes vocab)))))
	    (else
	      (static-error expr "Malformed set!-values")))))))

  (define local-extract-vocab
    (create-vocabulary 'local-extract-vocab #f
      "Invalid expression for local clause"
      "Invalid expression for local clause"
      "Invalid expression for local clause"
      "Invalid expression for local clause"))

  (when (language>=? 'structured)
    (add-primitivized-micro-form 'local scheme-vocabulary
      (let* ((kwd '())
	      (in-pattern `(_ (defs ...) ,@expr-pattern))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env attributes vocab)
	  (let ((p-env (pat:match-against m&e expr env)))
	    (if p-env
	      (let ((top-level? (get-top-level-status attributes)))
		(set-top-level-status attributes #t)
		(let*
		  ((defs (pat:pexpand '(defs ...) p-env kwd))
		    (vars+exprs
		      (map
			(lambda (e)
			  (let ((out
				  (expand-expr e env
				    attributes local-extract-vocab)))
			    out))
			defs)))
		  (set-top-level-status attributes)
		  (begin0
		    (set-macro-origin
		      (expand-expr
			(structurize-syntax
			  `(letrec*-values
			     ,(map (lambda (vars+expr)
				     `(,(car vars+expr) ,(cdr vars+expr)))
				vars+exprs)
			     ,@(pat:pexpand expr-pattern p-env kwd))
			  expr '(-1))
			env attributes vocab)
		      (syntax-car expr))
		    (set-top-level-status attributes top-level?))))
	      (static-error expr "Malformed local")))))))

  (let* ((kwd '())
	  (in-pattern-1 `(_ (fun . args) ,@expr-pattern))
	  (out-pattern-1 `(define-values (fun) (lambda args ,@expr-pattern)))
	  (in-pattern-2 `(_ var val))
	  (out-pattern-2 `(define-values (var) val))
	  (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	  (m&e-2 (pat:make-match&env in-pattern-2 kwd)))
    (add-primitivized-macro-form 'define scheme-vocabulary
      (lambda (expr env)
	(or (pat:match-and-rewrite expr m&e-1 out-pattern-1 kwd env)
	  (pat:match-and-rewrite expr m&e-2 out-pattern-2 kwd env)
	  (static-error expr "Malformed define"))))
    (add-primitivized-micro-form 'define local-extract-vocab
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e-1 expr env)
	    =>
	    (lambda (p-env)
	      (let ((fun (pat:pexpand 'fun p-env kwd))
		     (expr (pat:pexpand `(lambda args ,@expr-pattern)
			     p-env kwd)))
		(valid-syntactic-id? fun)
		(cons (list fun) expr))))
	  ((pat:match-against m&e-2 expr env)
	    =>
	    (lambda (p-env)
	      (let ((var (pat:pexpand 'var p-env kwd))
		     (val (pat:pexpand 'val p-env kwd)))
		(valid-syntactic-id? var)
		(cons (list var) val))))
	  (else
	    (static-error expr "Malformed define in local clause"))))))

  (let* ((kwd '())
	  (in-pattern-1 `(_ (var ...) val))
	  (m&e-1 (pat:make-match&env in-pattern-1 kwd)))
    (let ((define-values-helper
	    (lambda (handler)
	      (lambda (expr env attributes vocab)
		(unless (at-top-level? attributes)
		  (static-error expr
		    (if (language<=? 'structured)
		      "Invalid definition: must be at the top level"
		      "Invalid position for internal definition")))
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
			      (val (pat:pexpand 'val p-env kwd))
			      (out (handler expr env
				     attributes vocab vars val)))
			(set-top-level-status attributes
			  top-level?)
			out)))
		  (else (static-error expr
			  "Malformed define-values")))))))
      (add-primitivized-micro-form 'define-values scheme-vocabulary
	(define-values-helper
	  (lambda (expr env attributes vocab vars val)
	    (let* ((id-exprs (map (lambda (v)
				    (expand-expr v env
				      attributes vocab))
			       vars))
		    (expr-expr (expand-expr val env
				 attributes vocab)))
	      (create-define-values-form id-exprs
		expr-expr expr)))))
      (add-primitivized-micro-form 'define-values local-extract-vocab
	(define-values-helper
	  (lambda (expr env attributes vocab vars val)
	    (cons vars val))))))

  (define extract-type&super
    (let* ((kwd '())
	    (ts-pattern '(type super))
	    (m&e-ts (pat:make-match&env ts-pattern kwd)))
      (lambda (type-spec env)
	(cond
	  ((pat:match-against m&e-ts type-spec env)
	    =>
	    (lambda (tsp-env)
	      (let* ((type (pat:pexpand 'type tsp-env '()))
		      (super (pat:pexpand 'super tsp-env '())))
		(and (or (z:symbol? type)
		       (static-error type "Not an identifier"))
		  (values type super)))))
	  ((z:symbol? type-spec)
	    (values type-spec #f))
	  (else
	    (static-error type-spec "Invalid specification"))))))

  (when (language>=? 'structured)
    (add-primitivized-micro-form 'struct scheme-vocabulary
      (let* ((kwd '())
	      (in-pattern `(_ type-spec (fields ...)))
	      (m&e-in (pat:make-match&env in-pattern kwd)))
	(lambda (expr env attributes vocab)
	  (cond
	    ((pat:match-against m&e-in expr env)
	      =>
	      (lambda (p-env)
		(let* ((fields (pat:pexpand '(fields ...) p-env kwd))
			(type-spec (pat:pexpand 'type-spec p-env kwd)))
		  (distinct-valid-syntactic-id/s? fields)
		  (let-values (((type super)
				 (extract-type&super type-spec env)))
		    (create-struct-form
		      type
		      (and super (expand-expr super env attributes vocab))
		      fields
		      expr)))))
	    (else
	      (static-error expr "Malformed struct")))))))

  (define generate-struct-names
    (opt-lambda (type fields source
		  (omit-selectors? #f) (omit-setters? #f))
      (let ((name (lambda parts
		    (structurize-syntax
		      (apply symbol-append parts)
		      source))))
	(let ((type (z:read-object type))
	       (fields (map z:read-object fields)))
	  (cons
	    (name "struct:" type)
	    (cons
	      (name "make-" type)
	      (cons
		(name type "?")
		(apply append
		  (map (lambda (field)
			 (append
			   (if omit-selectors?
			     '()
			     (list (name type "-" field)))
			   (if omit-setters?
			     '()
			     (list (name "set-" type "-" field "!")))))
		    fields)))))))))

  (when (language>=? 'structured)
    (let* ((kwd '())
	    (in-pattern '(_ type-spec (fields ...)))
	    (m&e-in (pat:make-match&env in-pattern kwd)))
      (let ((ds-core
	      (lambda (handler)
		(lambda (expr env attributes vocab)
		  (cond
		    ((pat:match-against m&e-in expr env)
		      =>
		      (lambda (p-env)
			(let ((fields (pat:pexpand '(fields ...) p-env kwd))
			       (type-spec (pat:pexpand 'type-spec p-env kwd)))
			  (distinct-valid-syntactic-id/s? fields)
			  (let*-values
			    (((type super) (extract-type&super type-spec env))
			      ((names) (generate-struct-names type fields expr))
			      ((struct-expr)
				`(struct ,type-spec ,fields)))
			    (handler expr env attributes vocab
			      names struct-expr)))))
		    (else
		      (static-error expr "Malformed define-struct")))))))
	(add-primitivized-micro-form 'define-struct scheme-vocabulary
	  (ds-core
	    (lambda (expr env attributes vocab names struct-expr)
	      (expand-expr
		(structurize-syntax
		  `(define-values ,names ,struct-expr)
		  expr '(-1))
		env attributes vocab))))
	(add-primitivized-micro-form 'define-struct local-extract-vocab
	  (ds-core
	    (lambda (expr env attributes vocab names struct-expr)
	      (cons names struct-expr)))))))

  (when (language>=? 'structured)
    (let* ((kwd '())
	    (in-pattern '(_ (type-spec fields ...)))
	    (out-pattern '(define-struct type-spec (fields ...)))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (add-primitivized-macro-form 'define-structure scheme-vocabulary
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	    (static-error expr "Malformed define-structure"))))
      (add-primitivized-macro-form 'define-structure local-extract-vocab
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	    (static-error expr "Malformed define-structure"))))))

  (when (language>=? 'structured)
    (let* ((kwd '())
	    (in-pattern `(_ type-spec (fields ...) ,@expr-pattern))
	    (m&e-in (pat:make-match&env in-pattern kwd)))
      (let ((ls-core
	      (lambda (handler)
		(lambda (expr env attributes vocab)
		  (cond
		    ((pat:match-against m&e-in expr env)
		      =>
		      (lambda (p-env)
			(handler expr env attributes vocab p-env)))
		    (else
		      (static-error expr "Malformed let-struct")))))))
	(add-primitivized-micro-form 'let-struct scheme-vocabulary
	  (ls-core
	    (lambda (expr env attributes vocab p-env)
	      (let* ((fields (pat:pexpand '(fields ...) p-env kwd))
		      (type-spec (pat:pexpand 'type-spec p-env kwd))
		      (body (pat:pexpand `(,@expr-pattern) p-env kwd)))
		(distinct-valid-syntactic-id/s? fields)
		(let-values (((type super)
			       (extract-type&super type-spec env)))
		  (expand-expr
		    (structurize-syntax
		      `(let-values
			 ((,(generate-struct-names type fields expr)
			    (struct ,type-spec ,fields)))
			 ,@body)
		      expr '(-1))
		    env attributes vocab)))))))))

  ; ----------------------------------------------------------------------

  (when (language>=? 'structured)
    (add-primitivized-macro-form 'let scheme-vocabulary
      (let* ((kwd '())
	      (in-pattern-1 (if (language<=? 'structured)
			      '(_ ((v e) ...) b)
			      '(_ fun ((v e) ...) b ...)))
	      (out-pattern-1 (if (language<=? 'structured)
			       '(let-values (((v) e) ...) b)
			       '((letrec ((fun (lambda (v ...) b ...)))
				   fun)
				  e ...)))
	      (in-pattern-2 (if (language<=? 'structured)
			      in-pattern-1
			      '(_ ((v e) ...) b ...)))
	      (out-pattern-2 (if (language<=? 'structured)
			       out-pattern-1
			       '(let-values (((v) e) ...) b ...)))
	      (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	      (m&e-2 (pat:make-match&env in-pattern-2 kwd)))
	(lambda (expr env)
	  (let ((p-env (pat:match-against m&e-1 expr env)))
	    (if (and p-env (z:symbol? (pat:pexpand 'fun p-env kwd)))
	      (pat:pexpand out-pattern-1 p-env kwd)
	      (or (pat:match-and-rewrite expr m&e-2 out-pattern-2 kwd env)
		(static-error expr "Malformed let"))))))))

					; Turtle Macros for Robby

  (unless (language<=? 'structured)
    (let ([add-patterned-macro
	    (lambda (formname in-pattern out-pattern)
	      (add-macro-form
		formname
		scheme-vocabulary
		(let* ((kwd (list formname))
			(m&e (pat:make-match&env in-pattern kwd)))
		  (lambda (expr env)
		    (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
		      (static-error expr
			(format "Malformed ~a" formname)))))))])
      (add-patterned-macro 'tprompt
	'(tprompt E ...)
	'(let ([grab-Turtles Turtles]
		[grab-Cache Cache])
	   (begin E ...)
	   (set! Turtles grab-Turtles)
	   (set! Cache grab-Cache)))
      (add-patterned-macro 'split
	'(split E ...)
	'(splitfn (lambda () E ...)))
      (add-patterned-macro 'split*
	'(split* E ...)
	'(split*fn (lambda () E) ...))))

  (when (language>=? 'structured)
    (add-primitivized-macro-form 'let* scheme-vocabulary
      (let* ((kwd '())
	      (in-pattern-1 (if (language<=? 'structured)
			      '(_ () b) '(_ () b ...)))
	      (out-pattern-1 (if (language<=? 'structured)
			       '(let-values () b)
			       '(let-values () b ...)))
	      (in-pattern-2 (if (language<=? 'structured)
			      '(_ ((v0 e0) (v1 e1) ...) b)
			      '(_ ((v0 e0) (v1 e1) ...) b ...)))
	      (out-pattern-2 (if (language<=? 'structured)
			       '(let ((v0 e0)) (let* ((v1 e1) ...) b))
			       '(let ((v0 e0)) (let* ((v1 e1) ...) b ...))))
	      (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	      (m&e-2 (pat:make-match&env in-pattern-2 kwd)))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e-1 out-pattern-1 kwd env)
	    (pat:match-and-rewrite expr m&e-2 out-pattern-2 kwd env)
	    (static-error expr "Malformed let*"))))))

  (when (language>=? 'structured)
    (add-primitivized-macro-form 'delay scheme-vocabulary
      (let* ((kwd '())
	      (in-pattern '(_ expr))
	      (out-pattern '(#%make-promise (lambda () expr)))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	    (static-error expr "Malformed delay"))))))

  (when (language>=? 'structured)
    (add-primitivized-macro-form 'time scheme-vocabulary
      (let* ((kwd '())
	      (in-pattern '(_ e0 e1 ...))
	      (out-pattern '(let-values (((s)
					   (#%current-gc-milliseconds))
					  ((v cpu user)
					    (#%time-apply (lambda ()
							    e0
							    e1 ...))))
			      (#%printf
				"cpu time: ~s real time: ~s gc time: ~s~n"
				cpu user (#%- (#%current-gc-milliseconds) s))
			      (#%apply #%values v)))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	    (static-error expr "Malformed time"))))))

  (define break-list
    (lambda (elements counter)
      (let loop ((rev-head '()) (tail elements) (counter counter))
	(if (null? counter)
	  (values (reverse rev-head) tail)
	  (loop (cons (car tail) rev-head) (cdr tail) (cdr counter))))))

  (when (language>=? 'structured)
    (add-primitivized-micro-form 'let-values scheme-vocabulary
      (let* ((kwd '())
	      (in-pattern `(_ (((v ...) e) ...) ,@expr-pattern))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env attributes vocab)
	  (cond
	    ((pat:match-against m&e expr env)
	      =>
	      (lambda (p-env)
		(let ((vars (pat:pexpand '((v ...) ...) p-env kwd))
		       (vals (pat:pexpand '(e ...) p-env kwd))
		       (body (pat:pexpand `(,@expr-pattern)
			       p-env kwd)))
		  (let*
		    ((top-level? (get-top-level-status attributes))
		      (_ (set-top-level-status attributes))
		      (all-vars (apply append vars))
		      (_ (distinct-valid-syntactic-id/s? all-vars))
		      (expanded-vals
			(map (lambda (e)
			       (expand-expr e env attributes vocab))
			  vals))
		      (new-vars+marks
			(map create-lexical-binding+marks all-vars))
		      (new-vars
			(map car new-vars+marks))
		      (_
			(extend-env new-vars+marks env))
		      (result
			(create-let-values-form
			  (let loop ((var-lists vars)
				      (new-vars new-vars))
			    (if (null? var-lists)
			      '()
			      (let-values (((head tail)
					     (break-list new-vars
					       (car var-lists))))
				(cons head
				  (loop (cdr var-lists) tail)))))
			  expanded-vals
			  (parse-expr body env
			    attributes vocab expr)
			  expr))
		      (_ (retract-env new-vars env)))
		    (set-top-level-status attributes top-level?)
		    result))))
	    (else
	      (static-error expr "Malformed let-values")))))))

  (when (language>=? 'structured)
    (add-primitivized-macro-form 'let*-values scheme-vocabulary
      (let* ((kwd '())
	      (in-pattern-1 `(_ () ,@expr-pattern))
	      (out-pattern-1 `(let-values () ,@expr-pattern))
	      (in-pattern-2 `(_ ((v0 e0) (v1 e1) ...)
			       ,@expr-pattern))
	      (out-pattern-2 `(let-values ((v0 e0))
				(let*-values ((v1 e1) ...)
				  ,@expr-pattern)))
	      (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	      (m&e-2 (pat:make-match&env in-pattern-2 kwd)))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e-1 out-pattern-1 kwd env)
	    (pat:match-and-rewrite expr m&e-2 out-pattern-2 kwd env)
	    (static-error expr "Malformed let*-values"))))))

  (when (language>=? 'structured)
    (add-primitivized-micro-form 'letrec*-values scheme-vocabulary
      (let* ((kwd '())
	      (in-pattern `(_ (((v ...) e) ...) ,@expr-pattern))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env attributes vocab)
	  (cond
	    ((pat:match-against m&e expr env)
	      =>
	      (lambda (p-env)
		(let ((vars (pat:pexpand '((v ...) ...) p-env kwd))
		       (vals (pat:pexpand '(e ...) p-env kwd))
		       (body (pat:pexpand `(,@expr-pattern)
			       p-env kwd)))
		  (let*
		    ((all-vars (apply append vars))
		      (top-level? (get-top-level-status attributes))
		      (_ (set-top-level-status attributes))
		      (_ (distinct-valid-syntactic-id/s? all-vars))
		      (new-vars+marks
			(map create-lexical-binding+marks all-vars))
		      (new-vars
			(map car new-vars+marks))
		      (_
			(extend-env new-vars+marks env))
		      (expanded-vals
			(map (lambda (e)
			       (expand-expr e env attributes vocab))
			  vals))
		      (result
			(create-letrec*-values-form
			  (let loop ((var-lists vars)
				      (new-vars new-vars))
			    (if (null? var-lists)
			      '()
			      (let-values (((head tail)
					     (break-list new-vars
					       (car var-lists))))
				(cons head
				  (loop (cdr var-lists) tail)))))
			  expanded-vals
			  (parse-expr body env
			    attributes vocab expr)
			  expr))
		      (_ (retract-env new-vars env)))
		    (set-top-level-status attributes top-level?)
		    result))))
	    (else
	      (static-error expr "Malformed letrec*-values")))))))

  (when (language>=? 'structured)
    (add-primitivized-macro-form 'letrec-values scheme-vocabulary
      (let* ((kwd '())
	      (in-pattern `(_ (((v ...) e) ...) ,@expr-pattern))
	      (m&e (pat:make-match&env in-pattern kwd))
	      (out-pattern `(letrec*-values (((v ...) e) ...) ,@expr-pattern)))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	    (static-error expr "Malformed letrec-values"))))))

  (when (language>=? 'structured)
    (add-primitivized-macro-form 'letrec* scheme-vocabulary
      (let* ((kwd '())
	      (in-pattern `(_ ((v e) ...) ,@expr-pattern))
	      (m&e (pat:make-match&env in-pattern kwd))
	      (out-pattern `(letrec*-values (((v) e) ...) ,@expr-pattern)))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	    (static-error expr "Malformed letrec*"))))))

  (when (language>=? 'structured)
    (add-primitivized-macro-form 'letrec scheme-vocabulary
      (let* ((kwd '())
	      (in-pattern `(_ ((v e) ...) ,@expr-pattern))
	      (m&e (pat:make-match&env in-pattern kwd))
	      (out-pattern `(letrec*-values (((v) e) ...) ,@expr-pattern)))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	    (static-error expr "Malformed letrec"))))))

  (add-primitivized-macro-form
    'or
    scheme-vocabulary
    (let* ((kwd '())
	    (in-pattern-1 '(_))
	    (out-pattern-1 '#f)
	    (in-pattern-2 '(_ e))
	    (out-pattern-2 (if (language>=? 'structured)
			     'e
			     '(if e #t #f)))
	    (in-pattern-3 '(_ e0 e1 ...))
	    (out-pattern-3 (if (language>=? 'structured)
			     '(let ((t e0)) (if t t (or e1 ...)))
			     '(if e0 #t (or e1 ...))))
	    (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	    (m&e-2 (pat:make-match&env in-pattern-2 kwd))
	    (m&e-3 (pat:make-match&env in-pattern-3 kwd)))
      (lambda (expr env)
	(let ((p-env (pat:match-against m&e-1 expr env)))
	  (if p-env
	    (pat:pexpand out-pattern-1 p-env kwd)
	    (or (pat:match-and-rewrite expr m&e-2 out-pattern-2 kwd env)
	      (pat:match-and-rewrite expr m&e-3 out-pattern-3 kwd env)
	      (static-error expr "Malformed or")))))))

  (add-primitivized-macro-form
    'nor
    scheme-vocabulary
    (let* ((kwd '())
	    (in-pattern '(_ e0 ...))
	    (out-pattern '(#%not (or e0 ...)))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env)
	(or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	  (static-error expr "Malformed nor")))))

  (add-primitivized-macro-form
    'and
    scheme-vocabulary
    (let* ((kwd '())
	    (in-pattern-1 '(_))
	    (out-pattern-1 '#t)
	    (in-pattern-2 '(_ e))
	    (out-pattern-2 'e)
	    (in-pattern-3 '(_ e0 e1 ...))
	    (out-pattern-3 '(if e0 (and e1 ...) #f))
	    (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	    (m&e-2 (pat:make-match&env in-pattern-2 kwd))
	    (m&e-3 (pat:make-match&env in-pattern-3 kwd)))
      (lambda (expr env)
	(or (pat:match-and-rewrite expr m&e-1 out-pattern-1 kwd env)
	  (pat:match-and-rewrite expr m&e-2 out-pattern-2 kwd env)
	  (pat:match-and-rewrite expr m&e-3 out-pattern-3 kwd env)
	  (static-error expr "Malformed and")))))

  (add-primitivized-macro-form
    'nand
    scheme-vocabulary
    (let* ((kwd '())
	    (in-pattern '(_ e0 ...))
	    (out-pattern '(#%not (and e0 ...)))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env)
	(or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	  (static-error expr "Malformed nand")))))

  (when (language>=? 'side-effecting)
    (add-primitivized-macro-form
      'recur
      scheme-vocabulary
      (let* ((kwd '())
	      (in-pattern '(_ fun ((v e) ...) b ...))
	      (out-pattern '(let fun ((v e) ...) b ...))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	    (static-error expr "Malformed recur"))))))

  (when (language>=? 'side-effecting)
    (add-primitivized-macro-form
      'rec
      scheme-vocabulary
      (let* ((kwd '())
	      (in-pattern '(_ looper body))
	      (out-pattern '(letrec ((looper body)) looper))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	    (static-error expr "Malformed rec"))))))

  (define-struct cond-clause (text question answer else? =>? or?))

  (define cond-clause-vocab
    (create-vocabulary 'cond-clause-vocab #f
      "Symbol cannot be a cond question-answer pair"
      "Literal cannot be a cond question-answer pair"
      "List cannot be a cond question-answer pair"
      "Improper-list cannot be a cond question-answer pair"))

  (add-list-micro cond-clause-vocab
    (let* ((kwd '(else =>))
	    (in-pattern-1 (if (language<=? 'structured)
			    '(else answer)
			    '(else answer ...)))
	    (get-pattern-1 (if (language<=? 'structured)
			     'answer '(begin answer ...)))
	    (in-pattern-3 '(question => answer))
	    (in-pattern-2 '(question => answer ...))
	    (in-pattern-5 (if (language<=? 'side-effecting)
			    '(question => answer) ; will not match
			    '(question)))
	    (in-pattern-4 (if (language<=? 'structured)
			    '(question answer)
			    '(question answer ...)))
	    (get-pattern-4 (if (language<=? 'structured)
			     'answer '(begin answer ...)))
	    (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	    (m&e-2 (pat:make-match&env in-pattern-2 kwd))
	    (m&e-3 (pat:make-match&env in-pattern-3 kwd))
	    (m&e-4 (pat:make-match&env in-pattern-4 kwd))
	    (m&e-5 (pat:make-match&env in-pattern-5 kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e-1 expr env)
	    =>
	    (lambda (p-env)
	      (let ((answer (pat:pexpand get-pattern-1 p-env kwd)))
		(make-cond-clause expr #f answer #t #f #f))))
	  ((pat:match-against m&e-3 expr env)
	    =>
	    (lambda (p-env)
	      (let ((question (pat:pexpand 'question p-env kwd))
		     (answer (pat:pexpand 'answer p-env kwd)))
		(make-cond-clause expr question answer #f #t #f))))
	  ((pat:match-against m&e-2 expr env)
	    =>
	    (lambda (p-env)
	      (static-error expr "=> not followed by exactly one receiver")))
	  ((pat:match-against m&e-5 expr env)
	    =>
	    (lambda (p-env)
	      (let ((question (pat:pexpand 'question p-env kwd)))
		(make-cond-clause expr question #f #f #f #t))))
	  ((pat:match-against m&e-4 expr env)
	    =>
	    (lambda (p-env)
	      (let ((question (pat:pexpand 'question p-env kwd))
		     (answer (pat:pexpand get-pattern-4 p-env kwd)))
		(make-cond-clause expr question answer #f #f #f))))
	  (else (static-error expr "clause not in question-answer format"))))))

  ; We need this at the core and structured levels since we are
  ; disallowing procedure applications whose procedure position is not
  ; a symbol (here, the inner expression of ((debug-info-handler))
  ; violates that requirement).

  (define debug-info-handler-expression
    '(#%apply (#%debug-info-handler) (#%list)))

  (add-primitivized-micro-form 'cond scheme-vocabulary
    (let* ((kwd '())
	    (in-pattern '(_ bodies ...))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((bodies (pat:pexpand '(bodies ...) p-env kwd)))
		(let ((exp-bodies
			(map (lambda (e)
			       (expand-expr e env attributes
				 cond-clause-vocab))
			  bodies)))
		  (let ((had-no-clauses? (null? exp-bodies)))
		    (expand-expr
		      (structurize-syntax
			(let loop ((exps exp-bodies))
			  (if (null? exps)
			    (if (compile-allow-cond-fallthrough)
			      '(#%void)
			      `(#%raise
				 (#%make-exn:else
				   ,(if had-no-clauses?
				      "cond must contain at least one clause"
				      "no matching cond clause")
				   ,debug-info-handler-expression)))
			    (let ((first (car exps))
				   (rest (cdr exps)))
			      (cond
				((cond-clause-=>? first)
				  `(let ((test ,(cond-clause-question first)))
				     (if test
				       (,(cond-clause-answer first) test)
				       ,(loop rest))))
				((cond-clause-else? first)
				  (if (null? rest)
				    (cond-clause-answer first)
				    (static-error (cond-clause-text first)
				      "else allowed only in last position")))
				((cond-clause-or? first)
				  `(or ,(cond-clause-question first)
				     ,(loop rest)))
				(else
				  `(if ,(cond-clause-question first)
				     ,(cond-clause-answer first)
				     ,(loop rest)))))))
			expr '(-1))
		      env attributes vocab))))))
	  (else
	    (static-error expr "Malformed cond"))))))

  (when (language>=? 'side-effecting)
    (add-primitivized-macro-form
      'case
      scheme-vocabulary
      (let* ((kwd-1 '(else))
	      (in-pattern-1 '(_ val (else b ...)))
	      (out-pattern-1 '(begin val b ...))
	      (kwd-2 '())
	      (in-pattern-2 '(_ val))
	      (out-pattern-2-signal-error
		`(#%raise (#%make-exn:else
			    "no matching else clause"
			    ,debug-info-handler-expression)))
	      (out-pattern-2-no-error
		'(begin val (#%void)))
	      (in-pattern-3 '(_ val ((keys ...) b ...) rest ...))
	      (out-pattern-3 '(let ((tmp val))
				(if (#%memv tmp (quote (keys ...)))
				  (begin b ...)
				  (case tmp rest ...))))
	      (m&e-1 (pat:make-match&env in-pattern-1 kwd-1))
	      (m&e-2 (pat:make-match&env in-pattern-2 kwd-2))
	      (m&e-3 (pat:make-match&env in-pattern-3 kwd-2)))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e-1 out-pattern-1 kwd-1 env)
	    (if (compile-allow-cond-fallthrough)
	      (pat:match-and-rewrite expr m&e-2
		out-pattern-2-no-error kwd-2 env)
	      (pat:match-and-rewrite expr m&e-2
		out-pattern-2-signal-error kwd-2 env))
	    (pat:match-and-rewrite expr m&e-3 out-pattern-3 kwd-2 env)
	    (static-error expr "Malformed case"))))))

  (when (language>=? 'side-effecting)
    (add-primitivized-macro-form
      'evcase
      scheme-vocabulary
      (let* ((kwd-1 '(else))
	      (in-pattern-1 '(_ val (else b ...)))
	      (out-pattern-1 '(begin val b ...))
	      (kwd-2 '())
	      (in-pattern-2 '(_ val))
	      (out-pattern-2-signal-error
		`(#%raise (#%make-exn:else
			    "no matching else clause"
			    ,debug-info-handler-expression)))
	      (out-pattern-2-no-error
		'(begin val (#%void)))
	      (kwd-3 '(else))
	      (in-pattern-3 '(_ val (else b ...) rest))
	      (kwd-4 '())
	      (in-pattern-4 '(_ val (test-expr b ...) rest ...))
	      (out-pattern-4 '(let ((tmp val))
				(if (#%eqv? tmp test-expr)
				  (begin b ...)
				  (evcase tmp rest ...))))
	      (m&e-1 (pat:make-match&env in-pattern-1 kwd-1))
	      (m&e-2 (pat:make-match&env in-pattern-2 kwd-2))
	      (m&e-3 (pat:make-match&env in-pattern-3 kwd-3))
	      (m&e-4 (pat:make-match&env in-pattern-4 kwd-4)))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e-1 out-pattern-1 kwd-1 env)
	    (if (compile-allow-cond-fallthrough)
	      (pat:match-and-rewrite expr m&e-2
		out-pattern-2-no-error kwd-2 env)
	      (pat:match-and-rewrite expr m&e-2
		out-pattern-2-signal-error kwd-2 env))
	    (let ((penv (pat:match-against m&e-3 expr env)))
	      (if penv
		(static-error expr "else used before last evcase branch")
		(or (pat:match-and-rewrite expr m&e-4 out-pattern-4 kwd-4 env)
		  (static-error expr "Malformed evcase")))))))))

  (when (language>=? 'side-effecting)
    (add-primitivized-macro-form
      'when
      scheme-vocabulary
      (let* ((kwd '())
	      (in-pattern `(_ test ,@expr-pattern))
	      (out-pattern `(if test (begin ,@expr-pattern) (#%void)))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	    (static-error expr "Malformed when"))))))

  (when (language>=? 'side-effecting)
    (add-primitivized-macro-form
      'unless
      scheme-vocabulary
      (let* ((kwd '())
	      (in-pattern `(_ test ,@expr-pattern))
	      (out-pattern `(if test (#%void) (begin ,@expr-pattern)))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	    (static-error expr "Malformed unless"))))))

  (when (language>=? 'side-effecting)
    (let
      ((rewriter
	 (lambda (the-kwd kwd-text)
	   (let* ((kwd '())
		   (in-pattern `(_ var ,@expr-pattern))
		   (out-pattern `(#%call/cc (lambda (var) ,@expr-pattern)))
		   (m&e (pat:make-match&env in-pattern kwd)))
	     (lambda (expr env)
	       (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
		 (static-error expr
		   (string-append "Malformed " kwd-text))))))))
      (add-primitivized-macro-form 'let/cc scheme-vocabulary
	(rewriter 'let/cc "let/cc"))
      (add-primitivized-macro-form 'letcc scheme-vocabulary
	(rewriter 'letcc "letcc"))))

  (when (language>=? 'side-effecting)
    (let
      ((rewriter
	 (lambda (the-kwd kwd-text)
	   (let* ((kwd '())
		   (in-pattern `(_ var ,@expr-pattern))
		   (out-pattern `(#%call/ec (lambda (var) ,@expr-pattern)))
		   (m&e (pat:make-match&env in-pattern kwd)))
	     (lambda (expr env)
	       (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
		 (static-error expr
		   (string-append "Malformed " kwd-text))))))))
      (add-primitivized-macro-form 'let/ec scheme-vocabulary
	(rewriter 'let/ec "let/ec"))))

  (when (language>=? 'side-effecting)
    (add-primitivized-macro-form 'do scheme-vocabulary
      (let* ((in-kwd '())
	      (in-pattern `(_ (var-init-step ...)
			     (test seq ...)
			     ,@expr-pattern))
	      (out-pattern `(letrec ((loop
				       (lambda (var ...)
					 (if test
					   (begin (#%void) seq ...)
					   (begin ,@expr-pattern
					     (loop step ...))))))
			      (loop init ...)))
	      (in-m&e (pat:make-match&env in-pattern in-kwd))
	      (vis-kwd '())
	      (vis-pattern-1 '(var init step))
	      (vis-m&e-1 (pat:make-match&env vis-pattern-1 vis-kwd))
	      (vis-pattern-2 '(var init))
	      (vis-m&e-2 (pat:make-match&env vis-pattern-2 vis-kwd)))
	(lambda (expr env)
	  (cond
	    ((pat:match-against in-m&e expr env)
	      =>
	      (lambda (p-env)
		(let ((var-init-steps (pat:pexpand '(var-init-step ...)
					p-env in-kwd))
		       (test (pat:pexpand 'test p-env in-kwd))
		       (seqs (pat:pexpand '(seq ...) p-env in-kwd))
		       (body (pat:pexpand `(,@expr-pattern)
			       p-env in-kwd)))
		  (let
		    ((normalized-var-init-steps
		       (map
			 (lambda (vis)
			   (cond
			     ((pat:match-against vis-m&e-1 vis vis-kwd)
			       =>
			       (lambda (p-env)
				 `(,(pat:pexpand 'var p-env vis-kwd)
				    ,(pat:pexpand 'init p-env vis-kwd)
				    ,(pat:pexpand 'step p-env vis-kwd))))
			     ((pat:match-against vis-m&e-2 vis vis-kwd)
			       =>
			       (lambda (p-env)
				 `(,(pat:pexpand 'var p-env vis-kwd)
				    ,(pat:pexpand 'init p-env vis-kwd)
				    ,(pat:pexpand 'var p-env vis-kwd))))
			     (else
			       (static-error vis
				 "Malformed var-init-step"))))
			 var-init-steps)))
		    (let ((vars (map car normalized-var-init-steps))
			   (inits (map cadr normalized-var-init-steps))
			   (steps (map caddr normalized-var-init-steps)))
		      (structurize-syntax
			`(letrec ((loop
				    (lambda (,@vars)
				      (if ,test
					(begin (#%void) ,@seqs)
					(begin ,@body
					  (loop ,@steps))))))
			   (loop ,@inits))
			expr '(-1)))))))
	    (else
	      (static-error expr "Malformed do")))))))

  (when (language>=? 'side-effecting)
    (add-primitivized-micro-form 'fluid-let scheme-vocabulary
      (let* ((kwd '())
	      (in-pattern `(_ ((var val) ...) body ...))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env attributes vocab)
	  (cond
	    ((pat:match-against m&e expr env)
	      =>
	      (lambda (p-env)
		(let ((vars (pat:pexpand '(var ...) p-env kwd))
		       (vals (pat:pexpand '(val ...) p-env kwd))
		       (body (pat:pexpand '(body ...) p-env kwd)))
		  (distinct-valid-syntactic-id/s? vars)
		  (let* ((new-vars (map generate-name vars)))
		    (expand-expr
		      (structurize-syntax
			(if (null? vars)
			  `(begin ,@body)
			  `(let ,(map list new-vars vars)
			     (#%dynamic-wind
			       (lambda ()
				 ,@(map (lambda (var val)
					  `(set! ,var ,val))
				     vars vals))
			       (lambda ()
				 ,@body)
			       (lambda ()
				 ,@(map (lambda (var tmp)
					  `(set! ,var ,tmp))
				     vars new-vars)))))
			expr '(-1))
		      env attributes vocab)))))
	    (else
	      (static-error expr "Malformed fluid-let")))))))

  (when (language>=? 'side-effecting)
    (add-primitivized-macro-form 'parameterize scheme-vocabulary
      (let* ((kwd '())
	      (in-pattern-1 '(_ () body ...))
	      (out-pattern-1 '(begin body ...))
	      (in-pattern-2 '(_ ((param value) rest ...) body ...))
	      (out-pattern-2 '(let* ((pz (#%in-parameterization
					   (#%current-parameterization)
					   param))
				      (orig (pz)))
				(dynamic-wind
				  (lambda () (pz value))
				  (lambda () (parameterize (rest ...)
					       body ...))
				  (lambda () (pz orig)))))
	      (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	      (m&e-2 (pat:make-match&env in-pattern-2 kwd)))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e-1 out-pattern-1 kwd env)
	    (pat:match-and-rewrite expr m&e-2 out-pattern-2 kwd env)
	    (static-error expr "Malformed parameterize"))))))

  (when (language>=? 'side-effecting)
    (add-primitivized-macro-form 'with-handlers scheme-vocabulary
      (let* ((kwd '())
	      (in-pattern-1 (if (language<=? 'structured)
			      '(_ () b) '(_ () b ...)))
	      (out-pattern-1 (if (language<=? 'structured)
			       'b '(begin b ...)))
	      (in-pattern-2 '(_ ((pred handler) ...) body ...))
	      (out-pattern-2
		'((#%call/ec
		    (lambda (k)
		      (let ((handlers (#%list
					(cons pred handler)
					...)))
			(parameterize
			  ((#%current-exception-handler
			     (lambda (e)
			       (k
				 (lambda ()
				   (let loop ((handlers handlers))
				     (cond
				       ((#%null? handlers)
					 (#%raise e))
				       (((#%caar handlers) e)
					 ((#%cdar handlers) e))
				       (else
					 (loop (#%cdr handlers))))))))))
			  (#%call-with-values
			    (lambda () body ...)
			    (lambda args
			      (lambda () (#%apply #%values args))))))))))
	      (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	      (m&e-2 (pat:make-match&env in-pattern-2 kwd)))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e-1 out-pattern-1 kwd env)
	    (pat:match-and-rewrite expr m&e-2 out-pattern-2 kwd env)
	    (static-error expr "Malformed with-handlers"))))))

  (add-primitivized-micro-form 'define-macro scheme-vocabulary
    (let* ((kwd '())
	    (in-pattern `(_ macro-name macro-handler))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((macro-name (pat:pexpand 'macro-name p-env kwd))
		     (macro-handler (pat:pexpand 'macro-handler p-env kwd)))
		(valid-syntactic-id? macro-name)
		(unless (get-top-level-status attributes)
		  (static-error expr "Only supported at top-level"))
		(let* ((top-level? (get-top-level-status
				     attributes))
			(_ (set-top-level-status attributes))
			(real-name (sexp->raw macro-name))
			(expanded-handler (expand-expr macro-handler
					    env attributes vocab))
			(real-handler (m3-elaboration-evaluator
					expanded-handler
					parsed->raw
					'define-macro))
			(cache-table (make-hash-table)))
		  (set-top-level-status attributes top-level?)
		  (unless (procedure? real-handler)
		    (static-error expr "Expander is not a procedure"))
		  (add-user-macro-form real-name vocab
		    (lambda (m-expr m-env)
		      (structurize-syntax
			(apply m3-macro-body-evaluator real-handler
			  (cdr (sexp->raw m-expr cache-table)))
			m-expr '() cache-table)))
		  (expand-expr (structurize-syntax '(#%void) expr)
		    env attributes vocab)))))
	  (else
	    (static-error expr "Malformed define-macro"))))))

  (add-primitivized-micro-form 'let-macro scheme-vocabulary
    (let* ((kwd '())
	    (in-pattern `(_ macro-name macro-handler b0 b1 ...))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((macro-name (pat:pexpand 'macro-name p-env kwd))
		     (macro-handler (pat:pexpand 'macro-handler p-env kwd))
		     (body (pat:pexpand '(begin b0 b1 ...) p-env kwd)))
		(valid-syntactic-id? macro-name)
		(let* ((top-level? (get-top-level-status
				     attributes))
			(_ (set-top-level-status attributes))
			(real-name (sexp->raw macro-name))
			(expanded-handler (expand-expr macro-handler
					    env attributes vocab))
			(real-handler (m3-elaboration-evaluator
					expanded-handler
					parsed->raw
					'let-macro))
			(cache-table (make-hash-table)))
		  (set-top-level-status attributes top-level?)
		  (unless (procedure? real-handler)
		    (static-error expr "Expander is not a procedure"))
		  (let ((extended-vocab
			  (create-vocabulary 'user-macro-extended-vocab
			    vocab)))
		    (add-user-macro-form real-name extended-vocab
		      (lambda (m-expr m-env)
			(structurize-syntax
			  (apply m3-macro-body-evaluator real-handler
			    (cdr (sexp->raw m-expr cache-table)))
			  m-expr '() cache-table)))
		    (expand-expr
		      (structurize-syntax body expr)
		      env attributes extended-vocab))))))
	  (else
	    (static-error expr "Malformed let-macro"))))))

  (let ((b-e/c-t
	  (lambda (kwd-symbol kwd-string phase-string)
	    (add-micro-form kwd-symbol scheme-vocabulary
	      (let* ((kwd '())
		      (in-pattern '(_ e0 e1 ...))
		      (m&e (pat:make-match&env in-pattern kwd)))
		(lambda (expr env attributes vocab)
		  (cond
		    ((pat:match-against m&e expr env)
		      =>
		      (lambda (p-env)
			(let ((exprs (pat:pexpand '(begin e0 e1 ...)
				       p-env kwd)))
			  (expand-expr
			    (structurize-syntax
			      (with-handlers
				((exn? (lambda (exn)
					 (static-error expr
					   "Exception at ~a time: ~a"
					   phase-string
					   (exn-message exn)))))
				(m3-elaboration-evaluator
				  (expand-expr
				    (structurize-syntax exprs expr)
				    env attributes vocab)
				  parsed->raw
				  kwd-symbol))
			      expr)
			    env attributes vocab))))
		    (else
		      (static-error expr
			(string-append "Malformed " kwd-string))))))))))
    (b-e/c-t 'begin-construction-time "begin-construction-time" "construction")
    (b-e/c-t 'begin-elaboration-time "begin-elaboration-time" "elaboration"))

  (add-primitivized-macro-form 'unquote scheme-vocabulary
    (lambda (expr env)
      (static-error expr "Unquote outside quasiquote")))

  (add-primitivized-macro-form 'unquote-splicing scheme-vocabulary
    (lambda (expr env)
      (static-error expr "Unquote-splicing outside quasiquote")))

  (include "quasi.ss")

;  (include "shared.ss")

  (add-primitivized-micro-form 'require scheme-vocabulary
    (let* ((kwd '())
	    (in-pattern '(_ filename))
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
			`(#%load/use-compiled ,(quote-form-expr f))
			expr '(-1))
		      env attributes vocab)
		    (static-error filename "Does not yield a filename"))))))
	  (else
	    (static-error expr "Malformed require"))))))

  (add-primitivized-micro-form 'require-library scheme-vocabulary
    (let* ((kwd '())
	    (in-pattern '(_ filename collections ...))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((filename (pat:pexpand 'filename p-env kwd))
		     (collections (pat:pexpand '(collections ...) p-env kwd)))
		(let ((f (expand-expr filename env attributes vocab))
		       (cs (map (lambda (c)
				  (expand-expr c env attributes vocab))
			     collections)))
		  (unless (and (quote-form? f)
			    (z:string? (quote-form-expr f)))
		    (static-error filename "Does not yield a filename"))
		  (for-each
		    (lambda (c collection)
		      (unless (and (quote-form? c)
				(z:string? (quote-form-expr c)))
			(static-error collection "Does not yield a string")))
		    cs collections)
		  (let ((raw-f (z:read-object (quote-form-expr f)))
			 (raw-cs (map (lambda (c)
					(z:read-object (quote-form-expr c)))
				   cs)))
		    (unless (relative-path? raw-f)
		      (static-error f
			"Library path ~s must be a relative path"
			raw-f))
		    (expand-expr
		      (structurize-syntax
			(if (and (or (null? raw-cs)
				   (and (null? (cdr raw-cs))
				     (string=? (car raw-cs) "mzlib")))
			      (member raw-f mzscheme-libraries-provided))
			  `(#%void)
			  `(#%require-library/proc ,(quote-form-expr f)
			     ,@(map quote-form-expr cs)))
			expr '(-1))
		      env attributes vocab))))))
	  (else
	    (static-error expr "Malformed require-library"))))))

  (add-primitivized-micro-form 'require-relative-library scheme-vocabulary
    (let* ((kwd '())
	    (in-pattern '(_ filename collections ...))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((filename (pat:pexpand 'filename p-env kwd))
		     (collections (pat:pexpand '(collections ...) p-env kwd)))
		(let ((f (expand-expr filename env attributes vocab))
		       (cs (map (lambda (c)
				  (expand-expr c env attributes vocab))
			     collections)))
		  (unless (and (quote-form? f)
			    (z:string? (quote-form-expr f)))
		    (static-error filename "Does not yield a filename"))
		  (for-each
		    (lambda (c collection)
		      (unless (and (quote-form? c)
				(z:string? (quote-form-expr c)))
			(static-error collection "Does not yield a string")))
		    cs collections)
		  (let ((raw-f (z:read-object (quote-form-expr f)))
			 (raw-cs (map (lambda (c)
					(z:read-object (quote-form-expr c)))
				   cs)))
		    (unless (relative-path? raw-f)
		      (static-error f
			"Library path ~s must be a relative path"
			raw-f))
		    (expand-expr
		      (structurize-syntax
			(if (and (or (null? raw-cs)
				   (and (null? (cdr raw-cs))
				     (string=? (car raw-cs) "mzlib")))
			      (member raw-f mzscheme-libraries-provided))
			  `(#%void)
			  `(#%require-relative-library/proc ,(quote-form-expr f)
			     ,@(map quote-form-expr cs)))
			expr '(-1))
		      env attributes vocab))))))
	  (else
	    (static-error expr "Malformed require-relative-library"))))))

  (add-macro-form 'define-constructor scheme-vocabulary
    (let* ((kwd '())
	    (in-pattern '(_ sym modes ...))
	    (m&e (pat:make-match&env in-pattern kwd))
	    (out-pattern '(#%void)))
      (lambda (expr env)
	(or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	  (static-error expr "Malformed define-constructor")))))

  (add-macro-form 'define-type scheme-vocabulary
    (let* ((kwd '())
	    (in-pattern '(_ sym type))
	    (m&e (pat:make-match&env in-pattern kwd))
	    (out-pattern '(#%void)))
      (lambda (expr env)
	(or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	  (static-error expr "Malformed define-type")))))

  (add-macro-form ': scheme-vocabulary
    (let* ((kwd '())
	    (in-pattern '(_ expr type))
	    (m&e (pat:make-match&env in-pattern kwd))
	    (out-pattern 'expr))
      (lambda (expr env)
	(or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	  (static-error expr "Malformed :")))))

  (add-macro-form 'type: scheme-vocabulary
    (let* ((kwd '())
	    (in-pattern '(_ type attr ...))
	    (out-pattern '(#%void))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env)
	(or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	  (static-error expr "Malformed type:")))))

  (add-macro-form 'mrspidey:control scheme-vocabulary
    (let* ((kwd '())
	    (in-pattern '(_ para val))
	    (m&e (pat:make-match&env in-pattern kwd))
	    (out-pattern '(#%void)))
      (lambda (expr env)
	(or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	  (static-error expr "Malformed st:control")))))

  (add-macro-form 'polymorphic scheme-vocabulary
    (let* ((kwd '())
	    (in-pattern '(_ body))
	    (m&e (pat:make-match&env in-pattern kwd))
	    (out-pattern 'body))
      (lambda (expr env)
	(or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	  (static-error expr "Malformed polymorphic")))))

  )
