(require-library "include.ss")
(require-library "unitsig.ss")

(define zodiac:scheme-main@
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
	(case (length expr)
	  ((1)
	    (expand-expr (car expr) env attributes vocab))
	  (else
	    (expand-expr (structurize-syntax
			   `(begin ,@expr) source '(-1))
	      env attributes vocab)))))

    ; ----------------------------------------------------------------------

    (let ((case-lambda-handler
	    (lambda (cl-kwd)
	      (add-micro-form cl-kwd scheme-vocabulary
		(let* ((kwd `(,cl-kwd else))
			(in-pattern `(,cl-kwd
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
			(static-error expr "Malformed case-lambda")))))))))
      (case-lambda-handler 'case-lambda)
      (case-lambda-handler '#%case-lambda))

    (let ((lambda-handler
	    (lambda (l-kwd)
	      (add-macro-form l-kwd scheme-vocabulary
		(let* ((kwd (list l-kwd))
			(in-pattern `(,l-kwd args ,@expr-pattern))
			(out-pattern `(case-lambda
					(args ,@expr-pattern)))
			(m&e (pat:make-match&env in-pattern kwd)))
		  (lambda (expr env)
		    (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
		      (static-error expr "Malformed lambda"))))))))
      (lambda-handler 'lambda)
      (lambda-handler '#%lambda))

    (define-struct internal-definition (vars val))

    (define internal-define-vocab
      (create-vocabulary 'internal-define-vocab scheme-vocabulary))

    (let ((define-values-handler
	    (lambda (d-kwd)
	      (add-micro-form d-kwd internal-define-vocab
		(let* ((kwd (list d-kwd))
			(in-pattern `(,d-kwd (var ...) val))
			(m&e (pat:make-match&env in-pattern kwd)))
		  (lambda (expr env attributes vocab)
		    (cond
		      ((pat:match-against m&e expr env)
			=>
			(lambda (p-env)
			  (let* ((vars (pat:pexpand '(var ...) p-env kwd))
				  (_ (map valid-syntactic-id? vars))
				  (val (pat:pexpand 'val p-env kwd)))
			    (make-internal-definition vars val))))
		      (else
			(static-error expr
			  "Malformed internal definition")))))))))
      (define-values-handler 'define-values)
      (define-values-handler '#%define-values))

    (when (language>=? 'side-effecting)
      (let
	((begin-handler
	   (lambda (b-kwd)
	     (add-micro-form b-kwd scheme-vocabulary
	       (let* ((kwd (list b-kwd))
		       (in-pattern `(,b-kwd b ...))
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
			       (create-begin-form
				 (map (lambda (e)
					(expand-expr e env attributes vocab))
				   bodies)
				 expr))
			     (let-values
			       (((definitions terms)
				  (let loop ((seen '()) (rest bodies))
				    (if (null? rest)
				      (static-error expr
					(if (null? seen)
					  "Malformed begin"
					  "Internal definitions not followed by expression"))
				      (let ((first (car rest)))
					(let ((e-first
						(expand-expr first env
						  attributes
						  internal-define-vocab)))
					  (if (internal-definition? e-first)
					    (loop (cons e-first seen)
					      (cdr rest))
					    (values (reverse seen)
					      rest))))))))
			       (if (null? definitions)
				 (create-begin-form
				   (map (lambda (e)
					  (expand-expr e env attributes
					    vocab))
				     terms)
				   expr)
				 (expand-expr
				   (structurize-syntax
				     `(letrec*-values
					,(map (lambda (def)
						(list
						  (internal-definition-vars
						    def)
						  (internal-definition-val
						    def)))
					   definitions)
					,@terms)
				     expr)
				   env attributes vocab)))))))
		     (else
		       (static-error expr "Malformed begin")))))))))
	(begin-handler 'begin)
	(begin-handler '#%begin)))

    (when (language>=? 'side-effecting)
      (let
	((begin0-handler
	   (lambda (b-kwd)
	     (add-micro-form b-kwd scheme-vocabulary
	       (let* ((kwd (list b-kwd))
		       (in-pattern `(,b-kwd b ...))
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
			     (((first-body) (car bodies))
			       ((definitions terms)
				 (let loop ((seen '()) (rest (cdr bodies)))
				   (if (null? rest)
				     (if (null? seen)
				       (values '() '())
				       (static-error expr
					 "Internal definitions not followed by expression"))
				     (let ((first (car rest)))
				       (let ((e-first
					       (expand-expr first env
						 attributes
						 internal-define-vocab)))
					 (if (internal-definition? e-first)
					   (loop (cons e-first seen)
					     (cdr rest))
					   (values (reverse seen)
					     rest))))))))
			     (begin0
			       (if (null? definitions)
				 (create-begin0-form
				   (map (lambda (e)
					  (expand-expr e env attributes
					    vocab))
				     bodies)
				   expr)
				 (expand-expr
				   (structurize-syntax
				     `(begin0
					,first-body
					(letrec*-values
					  ,(map
					     (lambda (def)
					       (list
						 (internal-definition-vars
						   def)
						 (internal-definition-val
						   def)))
					     definitions)
					  ,@terms))
				     expr)
				   env attributes vocab))
			       (set-top-level-status attributes
				 top-level?))))))
		     (else
		       (static-error expr "Malformed begin0")))))))))
	(begin0-handler 'begin0)
	(begin0-handler '#%begin0)))

    ; (if test then)                                            [core]
    ; (if test then else)                                       [core]

    (let ((if-handler
	    (lambda (i-kwd)
	      (add-micro-form i-kwd scheme-vocabulary
		(let* ((kwd (list i-kwd))
			(in-pattern-1 `(,i-kwd test then))
			(in-pattern-2 `(,i-kwd test then else))
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
				  (_ (set-top-level-status attributes
				       top-level?))
				  (then-exp (expand-expr
					      (pat:pexpand 'then p-env kwd)
					      env attributes vocab))
				  (else-exp (expand-expr
					      (pat:pexpand 'else p-env kwd)
					      env attributes vocab)))
			    (create-if-form test-exp then-exp else-exp expr))))
		      (else
			(static-error expr "Malformed if")))))))))
      (if-handler 'if)
      (if-handler '#%if))

    ; (quote expr)                                              [core]

    (let ((quote-handler
	    (lambda (q-kwd)
	      (add-micro-form q-kwd scheme-vocabulary
		(let* ((kwd (list q-kwd))
			(in-pattern `(,q-kwd body))
			(m&e (pat:make-match&env in-pattern kwd)))
		  (lambda (expr env attributes vocab)
		    (if (and (z:list? expr)
			  (= 2 (z:sequence-length expr)))
		      (let ((contents (expose-list expr)))
			(if (and (z:symbol? (car contents))
			      (or (eq? 'quote (z:read-object (car contents)))
				(eq? '#%quote (z:read-object (car contents)))))
			  (create-quote-form (cadr contents) expr)
			  (static-error expr "Malformed quote")))
		      (static-error expr "Malformed quote"))))))))
      (quote-handler 'quote)
      (quote-handler '#%quote))

    (when (language>=? 'side-effecting)
      (let ((set!-handler
	      (lambda (s-kwd)
		(add-micro-form s-kwd scheme-vocabulary
		  (let* ((kwd (list s-kwd))
			  (in-pattern `(,s-kwd var val))
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
			  (static-error expr "Malformed set!")))))))))
	(set!-handler 'set!)
	(set!-handler '#%set!)))

    (when (language>=? 'side-effecting)
      (add-micro-form 'set!-values scheme-vocabulary
	(let* ((kwd '(set!-values))
		(in-pattern '(set!-values (vars ...) val))
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
			  expr)
			env attributes vocab)))))
	      (else
		(static-error expr "Malformed set!")))))))

    ; (local (defs ...) body ...)                                    [micro]

    (define local-extract-vocab (create-vocabulary 'local-extract-vocab))

    (add-micro-form 'local scheme-vocabulary
      (let* ((kwd '(local))
	      (in-pattern `(local (defs ...) ,@expr-pattern))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env attributes vocab)
	  (let ((p-env (pat:match-against m&e expr env)))
	    (if p-env
	      (let*
		((defs (pat:pexpand '(defs ...) p-env kwd))
		  (vars+exprs
		    (map
		      (lambda (e)
			(let ((out
				(expand-expr e env
				  attributes local-extract-vocab)))
			  out))
		      defs))
		  (top-level? (get-top-level-status attributes)))
		(set-top-level-status attributes)
		(begin0
		  (expand-expr
		    (structurize-syntax
		      `(letrec*-values
			 ,(map (lambda (vars+expr)
				 `(,(car vars+expr) ,(cdr vars+expr)))
			    vars+exprs)
			 ,@(pat:pexpand expr-pattern p-env kwd))
		      expr)
		    env attributes vocab)
		  (set-top-level-status attributes top-level?)))
	      (static-error expr "Malformed local"))))))

    ; (define var val)                                          [core]

    (let* ((kwd '(define))
	    (in-pattern-1 `(define (fun . args) ,@expr-pattern))
	    (out-pattern-1 `(define-values (fun) (lambda args ,@expr-pattern)))
	    (in-pattern-2 `(define var val))
	    (out-pattern-2 `(define-values (var) val))
	    (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	    (m&e-2 (pat:make-match&env in-pattern-2 kwd)))
      (add-macro-form 'define scheme-vocabulary
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e-1 out-pattern-1 kwd env)
	    (pat:match-and-rewrite expr m&e-2 out-pattern-2 kwd env)
	    (static-error expr "Malformed define"))))
      (add-micro-form 'define local-extract-vocab
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

    (let ((define-values-handler
	    (lambda (d-kwd)
	      (let* ((kwd (list d-kwd))
		      (in-pattern-1 `(,d-kwd (var ...) val))
		      (m&e-1 (pat:make-match&env in-pattern-1 kwd)))
		(let ((define-values-helper
			(lambda (handler)
			  (lambda (expr env attributes vocab)
			    (unless (at-top-level? attributes)
			      (static-error expr
				(if (language<=? 'structured)
				  "Not at top-level"
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
		  (add-micro-form d-kwd scheme-vocabulary
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
		  (add-micro-form d-kwd local-extract-vocab
		    (define-values-helper
		      (lambda (expr env attributes vocab vars val)
			(cons vars val)))))))))
      (define-values-handler 'define-values)
      (define-values-handler '#%define-values))

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
      (let ((struct-handler
	      (lambda (s-kwd)
		(add-micro-form s-kwd scheme-vocabulary
		  (let* ((kwd (list s-kwd))
			  (in-pattern `(,s-kwd type-spec (fields ...)))
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
			  (static-error expr "Malformed struct")))))))))
	(struct-handler 'struct)
	(struct-handler '#%struct)))

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
      (let* ((kwd '(define-struct))
	      (in-pattern '(define-struct type-spec (fields ...)))
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
	  (add-micro-form 'define-struct scheme-vocabulary
	    (ds-core
	      (lambda (expr env attributes vocab names struct-expr)
		(expand-expr
		  (structurize-syntax
		    `(define-values ,names ,struct-expr)
		    expr)
		  env attributes vocab))))
	  (add-micro-form 'define-struct local-extract-vocab
	    (ds-core
	      (lambda (expr env attributes vocab names struct-expr)
		(cons names struct-expr)))))))

    (when (language>=? 'structured)
      (let* ((kwd '(let-struct))
	      (in-pattern `(let-struct type-spec (fields ...)
			     ,@expr-pattern))
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
	  (add-micro-form 'let-struct scheme-vocabulary
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
			expr)
		      env attributes vocab)))))))))

    ; ----------------------------------------------------------------------

    ; (let ((var val) ...) body ...)                           [macro]
    ; (let fun ((var val) ...) body ...)                       [macro]

    (add-macro-form 'let scheme-vocabulary
      (let* ((kwd '(let))
	      (in-pattern-1 (if (language<=? 'structured)
			      '(let ((v e) ...) b)
			      '(let fun ((v e) ...) b ...)))
	      (out-pattern-1 (if (language<=? 'structured)
			       '(let-values (((v) e) ...) b)
			       '((letrec ((fun (lambda (v ...) b ...)))
				   fun)
				  e ...)))
	      (in-pattern-2 (if (language<=? 'structured)
			      in-pattern-1
			      '(let ((v e) ...) b ...)))
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
		(static-error expr "Malformed let")))))))

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

    ; (let* () body ...)                                       [macro]
    ; (let* ((var0 val0) (var1 val1) ...) body ...)            [macro]

    (add-macro-form
      'let*
      scheme-vocabulary
      (let* ((kwd '(let*))
	      (in-pattern-1 (if (language<=? 'structured)
			      '(let* () b) '(let* () b ...)))
	      (out-pattern-1 (if (language<=? 'structured)
			       'b '(begin b ...)))
	      (in-pattern-2 (if (language<=? 'structured)
			      '(let* ((v0 e0) (v1 e1) ...) b)
			      '(let* ((v0 e0) (v1 e1) ...) b ...)))
	      (out-pattern-2 (if (language<=? 'structured)
			       '(let ((v0 e0)) (let* ((v1 e1) ...) b))
			       '(let ((v0 e0)) (let* ((v1 e1) ...) b ...))))
	      (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	      (m&e-2 (pat:make-match&env in-pattern-2 kwd)))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e-1 out-pattern-1 kwd env)
	    (pat:match-and-rewrite expr m&e-2 out-pattern-2 kwd env)
	    (static-error expr "Malformed let*")))))

    ; (delay expr)                                              [core]

    (when (language>=? 'structured)
      (add-macro-form 'delay scheme-vocabulary
	(let* ((kwd '(delay))
		(in-pattern '(delay expr))
		(out-pattern '(#%make-promise (lambda () expr)))
		(m&e (pat:make-match&env in-pattern kwd)))
	  (lambda (expr env)
	    (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	      (static-error expr "Malformed delay"))))))

    (when (language>=? 'structured)
      (add-macro-form 'time scheme-vocabulary
	(let* ((kwd '(time))
		(in-pattern '(time expr))
		(out-pattern '(let-values (((v cpu user)
					     (#%time-apply (lambda () expr))))
				(#%printf "cpu time: ~s real time: ~s~n"
				  cpu user)
				v))
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

    (let ((let-values-handler
	    (lambda (l-kwd)
	      (add-micro-form l-kwd scheme-vocabulary
		(let* ((kwd (list l-kwd))
			(in-pattern `(,l-kwd (((v ...) e) ...)
				       ,@expr-pattern))
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
			(static-error expr "Malformed let-values")))))))))
      (let-values-handler 'let-values)
      (let-values-handler '#%let-values))

    (add-macro-form 'let*-values scheme-vocabulary
      (let* ((kwd '(let*-values))
	      (in-pattern-1 `(let*-values () ,@expr-pattern))
	      (out-pattern-1 `(let-values () ,@expr-pattern))
	      (in-pattern-2 `(let*-values ((v0 e0) (v1 e1) ...)
			       ,@expr-pattern))
	      (out-pattern-2 `(let-values ((v0 e0))
				(let*-values ((v1 e1) ...)
				  ,@expr-pattern)))
	      (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	      (m&e-2 (pat:make-match&env in-pattern-2 kwd)))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e-1 out-pattern-1 kwd env)
	    (pat:match-and-rewrite expr m&e-2 out-pattern-2 kwd env)
	    (static-error expr "Malformed let*-values")))))

    (let ((letrec*-values-handler
	    (lambda (l-kwd)
	      (add-micro-form l-kwd scheme-vocabulary
		(let* ((kwd (list l-kwd))
			(in-pattern `(,l-kwd (((v ...) e) ...)
				       ,@expr-pattern))
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
			(static-error expr "Malformed letrec*-values")))))))))
      (letrec*-values-handler 'letrec*-values)
      (letrec*-values-handler '#%letrec*-values))

    (add-macro-form 'letrec-values scheme-vocabulary
      (let* ((kwd '(letrec-values))
	      (in-pattern `(letrec-values (((v ...) e) ...) ,@expr-pattern))
	      (m&e (pat:make-match&env in-pattern kwd))
	      (out-pattern `(letrec*-values (((v ...) e) ...) ,@expr-pattern)))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	    (static-error expr "Malformed letrec-values")))))

    (add-macro-form 'letrec* scheme-vocabulary
      (let* ((kwd '(letrec*))
	      (in-pattern `(letrec* ((v e) ...) ,@expr-pattern))
	      (m&e (pat:make-match&env in-pattern kwd))
	      (out-pattern `(letrec*-values (((v) e) ...) ,@expr-pattern)))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	    (static-error expr "Malformed letrec*")))))

    (add-macro-form 'letrec scheme-vocabulary
      (let* ((kwd '(letrec))
	      (in-pattern `(letrec ((v e) ...) ,@expr-pattern))
	      (m&e (pat:make-match&env in-pattern kwd))
	      (out-pattern `(letrec*-values (((v) e) ...) ,@expr-pattern)))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	    (static-error expr "Malformed letrec")))))

    ; (or)                                                     [macro]
    ; (or e)                                                   [macro]
    ; (or e0 e1 ...)                                           [macro]

    (add-macro-form
      'or
      scheme-vocabulary
      (let* ((kwd '(or))
	      (in-pattern-1 '(or))
	      (out-pattern-1 '#f)
	      (in-pattern-2 '(or e))
	      (out-pattern-2 'e)
	      (in-pattern-3 '(or e0 e1 ...))
	      (out-pattern-3 '(let ((t e0)) (if t t (or e1 ...))))
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

    ; nor                                                      [macro]

    (add-macro-form
      'nor
      scheme-vocabulary
      (let* ((kwd '(nor))
	      (in-pattern '(nor e0 ...))
	      (out-pattern '(#%not (or e0 ...)))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	    (static-error expr "Malformed nor")))))

    ; (and)                                                    [macro]
    ; (and e)                                                  [macro]
    ; (and e0 e1 ...)                                          [macro]

    (add-macro-form
      'and
      scheme-vocabulary
      (let* ((kwd '(and))
	      (in-pattern-1 '(and))
	      (out-pattern-1 '#t)
	      (in-pattern-2 '(and e))
	      (out-pattern-2 'e)
	      (in-pattern-3 '(and e0 e1 ...))
	      (out-pattern-3 '(if e0 (and e1 ...) #f))
	      (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	      (m&e-2 (pat:make-match&env in-pattern-2 kwd))
	      (m&e-3 (pat:make-match&env in-pattern-3 kwd)))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e-1 out-pattern-1 kwd env)
	    (pat:match-and-rewrite expr m&e-2 out-pattern-2 kwd env)
	    (pat:match-and-rewrite expr m&e-3 out-pattern-3 kwd env)
	    (static-error expr "Malformed and")))))

    ; nand                                                     [macro]

    (add-macro-form
      'nand
      scheme-vocabulary
      (let* ((kwd '(nand))
	      (in-pattern '(nand e0 ...))
	      (out-pattern '(#%not (and e0 ...)))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	    (static-error expr "Malformed nand")))))

    ; (recur fun ((v e) ...) b ...)                            [macro]

    (when (language>=? 'side-effecting)
      (add-macro-form
	'recur
	scheme-vocabulary
	(let* ((kwd '(recur))
		(in-pattern '(recur fun ((v e) ...) b ...))
		(out-pattern '(let fun ((v e) ...) b ...))
		(m&e (pat:make-match&env in-pattern kwd)))
	  (lambda (expr env)
	    (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	      (static-error expr "Malformed recur"))))))

    (add-macro-form
      'cond
      scheme-vocabulary
      (let* ((kwd-1 '(cond else =>))
	      (in-pattern-1 (if (language<=? 'structured)
			      '(cond (else b)) '(cond (else b ...))))
	      (out-pattern-1 (if (language<=? 'structured)
			       'b '(begin b ...)))
	      (in-pattern-2 (if (language<=? 'structured)
			      '(cond (else b) rest ...)
			      '(cond (else b ...) rest ...)))
	      (out-pattern-2 'this-is-an-error-case)
	      (in-pattern-3 '(cond))
	      (out-pattern-3 (if (or (language<=? 'structured)
				   param:unmatched-cond/case-is-error?)
			       '(#%raise (#%make-exn:else
					   "no matching clause"
					   ((debug-info-handler))))
			       '(#%void)))
	      (in-pattern-4 (if (language<=? 'side-effecting)
			      '()	; will never match
			      '(cond (item) rest ...)))
	      (out-pattern-4 '(or item (cond rest ...)))
	      (in-pattern-5 '(cond (test => receiver) rest ...))
	      (out-pattern-5 '(let ((t test))
				(if t (receiver t) (cond rest ...))))
	      (in-pattern-6 '(cond (test =>) rest ...))
	      (out-pattern-6 'this-is-an-error-case)
	      (in-pattern-7 (if (language<=? 'structured)
			      '(cond (test b) rest ...)
			      '(cond (test b0 b1 ...) rest ...)))
	      (out-pattern-7 (if (language<=? 'structured)
			       '(if test b (cond rest ...))
			       '(if test (begin b0 b1 ...) (cond rest ...))))
	      (m&e-1 (pat:make-match&env in-pattern-1 kwd-1))
	      (m&e-2 (pat:make-match&env in-pattern-2 kwd-1))
	      (m&e-3 (pat:make-match&env in-pattern-3 kwd-1))
	      (m&e-4 (pat:make-match&env in-pattern-4 kwd-1))
	      (m&e-5 (pat:make-match&env in-pattern-5 kwd-1))
	      (m&e-6 (pat:make-match&env in-pattern-6 kwd-1))
	      (m&e-7 (pat:make-match&env in-pattern-7 kwd-1)))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e-1 out-pattern-1 kwd-1 env)
	    (pat:match-and-rewrite expr m&e-2 out-pattern-2 kwd-1
	      (lambda (_)
		(static-error expr "else allowed only in last position"))
	      (lambda () #f)
	      env)
	    (pat:match-and-rewrite expr m&e-3 out-pattern-3 kwd-1 env)
	    (pat:match-and-rewrite expr m&e-4 out-pattern-4 kwd-1 env)
	    (pat:match-and-rewrite expr m&e-5 out-pattern-5 kwd-1 env)
	    (pat:match-and-rewrite expr m&e-6 out-pattern-6 kwd-1
	      (lambda (_)
		(static-error expr "=> must be followed by a receiver"))
	      (lambda () #f)
	      env)
	    (pat:match-and-rewrite expr m&e-7 out-pattern-7 kwd-1 env)
	    (static-error expr "Malformed cond")))))

    ; (case val (else b ...))                                  [macro]
    ; (case val ((keys ...) b ...))                            [macro]
    ; (case val ((keys ...) b ...) rest ...)                   [macro]

    (add-macro-form
      'case
      scheme-vocabulary
      (let* ((kwd-1 '(case else))
	      (in-pattern-1 (if (language<=? 'structured)
			      '(case val (else b))
			      '(case val (else b ...))))
	      (out-pattern-1 (if (language<=? 'structured) 'b '(begin b ...)))
	      (kwd-2 '(case))
	      (in-pattern-2 '(case val))
	      (out-pattern-2 (if (or (language<=? 'structured)
				   param:unmatched-cond/case-is-error?)
			       '(#%raise (#%make-exn:else
					   "no matching clause"
					   ((debug-info-handler))))
			       '(#%void)))
	      (in-pattern-3 (if (language<=? 'structured)
			      '(case val ((keys ...) b) rest ...)
			      '(case val ((keys ...) b ...) rest ...)))
	      (out-pattern-3 (if (language<=? 'structured)
			       '(let ((tmp val))
				  (if (#%memv tmp (quote (keys ...)))
				    b
				    (case tmp rest ...)))
			       '(let ((tmp val))
				  (if (#%memv tmp (quote (keys ...)))
				    (begin b ...)
				    (case tmp rest ...)))))
	      (m&e-1 (pat:make-match&env in-pattern-1 kwd-1))
	      (m&e-2 (pat:make-match&env in-pattern-2 kwd-2))
	      (m&e-3 (pat:make-match&env in-pattern-3 kwd-2)))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e-1 out-pattern-1 kwd-1 env)
	    (pat:match-and-rewrite expr m&e-2 out-pattern-2 kwd-2 env)
	    (pat:match-and-rewrite expr m&e-3 out-pattern-3 kwd-2 env)
	    (static-error expr "Malformed case")))))

    ; (when test expr ...)                                     [macro]

    (when (language>=? 'side-effecting)
      (add-macro-form
	'when
	scheme-vocabulary
	(let* ((kwd '(when))
		(in-pattern `(when test ,@expr-pattern))
		(out-pattern `(if test (begin ,@expr-pattern) (#%void)))
		(m&e (pat:make-match&env in-pattern kwd)))
	  (lambda (expr env)
	    (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	      (static-error expr "Malformed when"))))))

    ; (unless test expr ...)                                   [macro]

    (when (language>=? 'side-effecting)
      (add-macro-form
	'unless
	scheme-vocabulary
	(let* ((kwd '(unless))
		(in-pattern `(unless test ,@expr-pattern))
		(out-pattern `(if test (#%void) (begin ,@expr-pattern)))
		(m&e (pat:make-match&env in-pattern kwd)))
	  (lambda (expr env)
	    (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	      (static-error expr "Malformed unless"))))))

    ; (letcc var body ...)                                     [macro]
    ; (let/cc var body ...)                                    [macro]

    (when (language>=? 'side-effecting)
      (let
	((rewriter
	   (lambda (the-kwd kwd-text)
	     (let* ((kwd (list the-kwd))
		     (in-pattern (cons the-kwd `(var ,@expr-pattern)))
		     (out-pattern `(#%call/cc (lambda (var) ,@expr-pattern)))
		     (m&e (pat:make-match&env in-pattern kwd)))
	       (lambda (expr env)
		 (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
		   (static-error expr
		     (string-append "Malformed " kwd-text))))))))
	(add-macro-form 'letcc scheme-vocabulary
	  (rewriter 'letcc "letcc"))
	(add-macro-form 'let/cc scheme-vocabulary
	  (rewriter 'let/cc "let/cc"))))

    (when (language>=? 'side-effecting)
      (let
	((rewriter
	   (lambda (the-kwd kwd-text)
	     (let* ((kwd (list the-kwd))
		     (in-pattern (cons the-kwd `(var ,@expr-pattern)))
		     (out-pattern `(#%call/ec (lambda (var) ,@expr-pattern)))
		     (m&e (pat:make-match&env in-pattern kwd)))
	       (lambda (expr env)
		 (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
		   (static-error expr
		     (string-append "Malformed " kwd-text))))))))
	(add-macro-form 'letec scheme-vocabulary
	  (rewriter 'letec "letec"))
	(add-macro-form 'let/ec scheme-vocabulary
	  (rewriter 'let/ec "let/ec"))
	(add-macro-form 'catch scheme-vocabulary
	  (rewriter 'catch "catch"))))

    ; (define-schema var exp)                                  [macro]

    (add-macro-form
      'define-schema
      scheme-vocabulary
      (let* ((kwd '(define-schema))
	      (in-pattern '(define-schema var exp))
	      (out-pattern '((lambda (var) (var)) #%void))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	    (static-error expr "Malformed define-schema")))))

    (when (language>=? 'side-effecting)
      (add-macro-form 'do scheme-vocabulary
	(let* ((in-kwd '(do))
		(in-pattern `(do (var-init-step ...)
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
			  expr))))))
	      (else
		(static-error expr "Malformed do")))))))

    (when (language>=? 'side-effecting)
      (add-micro-form 'fluid-let scheme-vocabulary
	(let* ((kwd '(fluid-let))
		(in-pattern `(fluid-let ((var val) ...) body ...))
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
				     vars new-vars))))
			  expr)
			env attributes vocab)))))
	      (else
		(static-error expr "Malformed fluid-let")))))))

    (when (language>=? 'side-effecting)
      (add-macro-form 'parameterize scheme-vocabulary
	(let* ((kwd '(parameterize))
		(in-pattern-1 '(parameterize () body ...))
		(out-pattern-1 '(begin body ...))
		(in-pattern-2 '(parameterize ((param value) rest ...) body ...))
		(out-pattern-2 '(let* ((pz (#%in-parameterization
					     (#%current-parameterization)
					     param #t))
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
      (add-macro-form 'with-handlers scheme-vocabulary
	(let* ((kwd '(with-handlers))
		(in-pattern '(with-handlers ((pred handler) ...) body ...))
		(out-pattern
		  '((#%call/ec
		      (lambda (k)
			(let ((handlers (#%list
					  (cons pred handler)
					  ...)))
			  (parameterize
			    ((#%current-exception-handler
			       (lambda (e)
				 (k
				   (let loop ((handlers handlers))
				     (cond
				       ((#%null? handlers)
					 (lambda () (#%raise e)))
				       (((#%caar handlers) e)
					 (lambda () ((#%cdar handlers) e)))
				       (else
					 (loop (#%cdr handlers)))))))))
			    (#%call-with-values
			      (lambda () body ...)
			      (lambda args
				(k
				  (lambda () (#%apply #%values args)))))))))))
		(m&e (pat:make-match&env in-pattern kwd)))
	  (lambda (expr env)
	    (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	      (static-error expr "Malformed with-handlers"))))))

    (let ((d-m-handler
	    (lambda (d-m-kwd)
	      (add-micro-form d-m-kwd scheme-vocabulary
		(let* ((kwd `(,d-m-kwd))
			(in-pattern `(,d-m-kwd macro-name macro-handler))
			(m&e (pat:make-match&env in-pattern kwd)))
		  (lambda (expr env attributes vocab)
		    (unless (at-top-level? attributes)
		      (static-error expr "Not at top-level"))
		    (cond
		      ((pat:match-against m&e expr env)
			=>
			(lambda (p-env)
			  (let ((macro-name (pat:pexpand 'macro-name p-env kwd))
				 (macro-handler (pat:pexpand 'macro-handler p-env kwd)))
			    (valid-syntactic-id? macro-name)
			    (let* ((top-level? (get-top-level-status
						 attributes))
				    (_ (set-top-level-status attributes))
				    (real-name (sexp->raw macro-name))
				    (raw-handler (sexp->raw macro-handler))
				    (real-handler (with-parameterization
						    zodiac-user-parameterization
						    (lambda ()
						      (eval raw-handler))))
				    (cache-table (make-hash-table)))
			      (set-top-level-status attributes top-level?)
			      (unless (procedure? real-handler)
				(static-error expr "Expander is not a procedure"))
			      (add-macro-form real-name vocab
				(lambda (m-expr m-env)
				  (structurize-syntax
				    (apply real-handler
				      (let ((in (cdr (sexp->raw m-expr cache-table))))
					in))
				    m-expr '() cache-table)))
			      (expand-expr (structurize-syntax '(#%void) expr)
				env attributes vocab)))))
		      (else
			(static-error expr "Malformed define-macro")))))))))
      (d-m-handler 'define-macro)
      (d-m-handler '#%define-macro))

    (add-micro-form 'include scheme-vocabulary
      (let* ((kwd '(include))
	      (in-pattern '(include filename))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env attributes vocab)
	  (cond
	    ((pat:match-against m&e expr env)
	      =>
	      (lambda (p-env)
		(let ((filename (pat:pexpand 'filename p-env kwd)))
		  (unless (z:string? filename)
		    (static-error filename "File name must be a string"))
		  (let ((raw-filename (z:read-object filename)))
		    (let-values (((base name dir?) (split-path raw-filename)))
		      (when dir?
			(static-error filename "Cannot include a directory"))
		      (let ((original-directory (current-directory))
			     (p (with-handlers
				  ((exn:i/o:filesystem:filename?
				     (lambda (exn)
				       (static-error filename
					 "Unable to open file"))))
				  (open-input-file raw-filename))))
			(dynamic-wind
			  (lambda ()
			    (when (string? base)
			      (current-directory base)))
			  (lambda ()
			    (let ((exprs
				    (let ((reader
					    (z:read p
					      (make-location
						(location-line
						  z:default-initial-location)
						(location-column
						  z:default-initial-location)
						(location-offset
						  z:default-initial-location)
						(build-path
						  (current-directory)
						  name)))))
				      (let loop ()
					(let ((input (reader)))
					  (if (eof? input)
					    '()
					    (cons input
					      (loop))))))))
			      (expand-expr
				(structurize-syntax
				  (if (null? exprs)
				    `(#%void)
				    `(begin ,@exprs))
				  expr)
				env attributes vocab)))
			  (lambda ()
			    (current-directory original-directory)
			    (close-input-port p)))))))))
	    (else
	      (static-error expr "Malformed include"))))))

    (add-macro-form 'unquote scheme-vocabulary
      (lambda (expr env)
	(static-error expr "Unquote outside quasiquote")))

    (add-macro-form 'unquote-splicing scheme-vocabulary
      (lambda (expr env)
	(static-error expr "Unquote-splicing outside quasiquote")))

    (include "qq.ss")

    (include "shared.ss")

    ))
