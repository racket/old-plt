(require-library "unitsig.ss")

(define zodiac:scheme-core@
  (unit/sig zodiac:scheme-core^
    (import zodiac:structures^ zodiac:misc^ zodiac:sexp^
      (z : zodiac:reader-structs^) zodiac:expander^ zodiac:interface^
      (pat : zodiac:pattern^) (param : plt:parameters^))

    (define-struct (parsed struct:zodiac) (back))
     (define-struct (varref struct:parsed) (var))
      (define-struct (top-level-varref struct:varref) ())
       (define-struct (top-level-varref/bind struct:top-level-varref) (slot))
      (define-struct (bound-varref struct:varref) (binding))
       (define-struct (lexical-varref struct:bound-varref) ())
     (define-struct (app struct:parsed) (fun args))
     (define-struct (binding struct:parsed) (var orig-name))
      (define-struct (lexical-binding struct:binding) ())
     (define-struct (form struct:parsed) ())

    ; ----------------------------------------------------------------------

    (define name-eq? eq?)

    (define marks-equal? equal?)

    ; ----------------------------------------------------------------------

    (define make-empty-back-box (lambda () (box '())))

    (define generate-name
      (lambda (var)
	(string->symbol
	  (string-append
	    (symbol->string (gensym)) ":"
	    (symbol->string (z:symbol-orig-name var))))))

    (define create-binding+marks
      (opt-lambda (constructor (nom-de-plume generate-name))
	(opt-lambda (v (s v))
	  (cons
	    (constructor (zodiac-origin s)
	      (zodiac-start s) (zodiac-finish s)
	      (make-empty-back-box)
	      (nom-de-plume v)
	      (z:symbol-orig-name s))
	    (z:symbol-marks v)))))

    (define create-lexical-binding+marks
      (create-binding+marks make-lexical-binding))

    (define create-top-level-varref
      (lambda (v s)
	(make-top-level-varref (zodiac-origin s)
	  (zodiac-start s) (zodiac-finish s)
	  (make-empty-back-box) v)))

    (define create-top-level-varref/bind
      (lambda (v b s)
	(make-top-level-varref/bind (zodiac-origin s)
	  (zodiac-start s) (zodiac-finish s)
	  (make-empty-back-box) v b)))

    (define create-bound-varref
      (lambda (constructor)
	(opt-lambda (v (s v))
	  (constructor (zodiac-origin s)
	    (zodiac-start s) (zodiac-finish s)
	    (make-empty-back-box) (binding-var v)
	    v))))

    (define create-lexical-varref
      (create-bound-varref make-lexical-varref))

    (define create-app
      (lambda (fun args source)
	(make-app (zodiac-origin source)
	  (zodiac-start source) (zodiac-finish source)
	  (make-empty-back-box) fun args)))

    ; ----------------------------------------------------------------------

    (define p->r-table
      '())

    (define extend-parsed->raw
      (lambda (predicate handler)
	(set! p->r-table
	  (cons (cons predicate handler)
	    p->r-table))))

    (define parsed->raw
      (opt-lambda (expr (handler #f))
	(let loop ((table p->r-table))
	  (if (null? table)
	    (internal-error expr "Invalid object for parsed->raw")
	    (let ((first (car table)))
	      (if ((car first) expr)
		((cdr first) expr (or handler parsed->raw))
		(loop (cdr table))))))))

    (extend-parsed->raw varref?
      (lambda (expr p->r) (varref-var expr)))
    (extend-parsed->raw binding?
      (lambda (expr p->r) (binding-var expr)))

    (extend-parsed->raw app?
      (lambda (expr p->r)
	(cons (p->r (app-fun expr))
	  (map p->r (app-args expr)))))

    ; ----------------------------------------------------------------------

    (define scheme-vocabulary (make-vocabulary))

    (add-sym-micro scheme-vocabulary
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
		    (create-top-level-varref/bind
		      id
		      (hash-table-get top-level-space id
			(lambda ()
			  (let ((b (box '())))
			    (hash-table-put! top-level-space id b)
			    b)))
		      expr)
		    (create-top-level-varref id expr)))))
	    (else
	      (internal-error expr "Invalid resolution ~s" r))))))

    (add-list-micro scheme-vocabulary
      (lambda (expr env attributes vocab)
	(let ((contents (expose-list expr)))
	  (if (null? contents)
	    (if (language>=? 'advanced)
	      (expand-expr (structurize-syntax `(quote ,expr) expr)
		env attributes vocab)
	      (static-error expr "Empty combination is a syntax error"))
	    (let ((bodies
		    (map
		      (lambda (e)
			(expand-expr e env attributes vocab))
		      contents)))
	      (create-app (car bodies) (cdr bodies) expr))))))

    (define lexically-resolved?
      (lambda (expr env)
	(let ((name (z:read-object expr)) (marks (z:symbol-marks expr)))
	  (let ((res (resolve-in-env name marks env)))
	    (and res (binding? res))))))

    (define in-lexically-extended-env
      (lambda (env vars handler)
	(let ((new-vars+marks
		(map create-lexical-binding+marks
		  vars)))
	  (let ((new-vars (map car new-vars+marks)))
	    (extend-env new-vars+marks env)
	    (let ((result (handler new-vars env)))
	      (retract-env new-vars env)
	      result)))))

    ; ----------------------------------------------------------------------

    (define scheme-expand
      (opt-lambda (expr (params (current-parameterization)))
	(let ((attr (make-attributes)))
	  (expand expr attr scheme-vocabulary params))))

    (define scheme-expand-program
      (opt-lambda (exprs (params (current-parameterization)))
	(let ((attr (make-attributes)))
	  (expand-program exprs attr scheme-vocabulary params))))

    ; ----------------------------------------------------------------------

    (define valid-syntactic-id?
      (lambda (id)
	(or (z:symbol? id) 
	  (static-error id "Invalid identifier"))))

    (define valid-syntactic-id/s?
      (lambda (ids)
	(cond
	  ((null? ids) '())
	  ((pair? ids)
	    (let ((first (car ids)) (rest (cdr ids)))
	      (if valid-syntactic-id?
		(cons (z:read-object first) (valid-syntactic-id/s? rest))
		(static-error first "Invalid identifier"))))
	  (else (internal-error ids "Illegal to check validity of id/s")))))

    (define distinct-valid-syntactic-id/s?
      (lambda (ids)
	(let ((input-ids (syntactic-id/s->ids ids)))
	  (let loop ((ids (valid-syntactic-id/s? input-ids)) (index 0))
	    (or (null? ids)
	      (if (symbol? (car ids))
		(if (memq (car ids) (cdr ids))
		  (static-error (list-ref input-ids index)
		    "Repeated identifier")
		  (loop (cdr ids) (add1 index)))
		(static-error (list-ref input-ids index)
		  "Not an identifier")))))))

    (define syntactic-id/s->ids
      (lambda (ids)
	(cond
	  ((or (z:list? ids) (z:improper-list? ids))
	    (expose-list ids))
	  ((z:symbol? ids) (list ids))
	  ((pair? ids) ids)
	  ((null? ids) ids)
	  (else (static-error ids "Invalid identifier")))))

    ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    (define valid-id?
      (lambda (id)
	(or (binding? id) 
	  (static-error id "Invalid identifier"))))

    (define valid-id/s?
      (lambda (ids)
	(cond
	  ((null? ids) '())
	  ((pair? ids)
	    (let ((first (car ids)) (rest (cdr ids)))
	      (if (valid-id? first)
		(cons (binding-orig-name first) (valid-id/s? rest))
		(static-error first "Invalid identifier"))))
	  (else (internal-error ids "Illegal to check validity of id/s")))))

    (define distinct-valid-id/s?
      (lambda (ids)
	(let ((input-ids (id/s->ids ids)))
	  (let loop ((ids (valid-id/s? input-ids)) (index 0))
	    (or (null? ids)
	      (if (memq (car ids) (cdr ids))
		(static-error (list-ref input-ids index) "Repeated identifier")
		(loop (cdr ids) (add1 index))))))))

    (define id/s->ids
      (lambda (ids)
	(cond
	  ((or (z:list? ids) (z:improper-list? ids))
	    (expose-list ids))
	  ((z:symbol? ids) (list ids))
	  ((pair? ids) ids)
	  ((null? ids) ids)
	  (else (static-error ids "Invalid identifier")))))

    
    ; ----------------------------------------------------------------------

    (define language-levels '(core structured side-effecting advanced))

    (define the-language param:check-syntax-level)

    (define language<=?
      (let ((table
	      (let loop ((<? #f) (levels language-levels))
		(if (null? levels) '()
		  (let ((first (car levels)) (rest (cdr levels)))
		    (if (eq? the-language first)
		      (cons (cons first #t) (loop #t rest))
		      (cons (cons first <?) (loop <? rest))))))))
	(lambda (language)
	  (let ((lookup (assq language table)))
	    (if lookup (cdr lookup)
	      (internal-error language "Illegal language level name"))))))

    (define language>=?
      (lambda (language)
	(or (eq? language the-language)
	  (not (language<=? language)))))

    ; ----------------------------------------------------------------------

    (define optarglist-pattern 'vars)

    (define-struct optarglist-entry (var+marks))
    (define-struct (initialized-optarglist-entry struct:optarglist-entry)
      (expr))

    (define-struct optarglist (vars))
    (define-struct (sym-optarglist struct:optarglist) ())
    (define-struct (list-optarglist struct:optarglist) ())
    (define-struct (ilist-optarglist struct:optarglist) ())

    ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    (define optarglist-decl-entry-parser-vocab (make-vocabulary))

    (add-sym-micro optarglist-decl-entry-parser-vocab
      (lambda (expr env attributes vocab)
	(let ((status-holder (get-attribute attributes 'optarglist-status)))
	  (case (unbox status-holder)
	    ((proper improper) (void))
	    ((proper/defaults)
	      (static-error expr
		"Appears after initial value specifications"))
	    ((improper/defaults)
	      (set-box! status-holder 'improper/done))
	    ((improper/done)
	      (static-error expr
		"Appears past catch-all argument"))
	    (else (internal-error (unbox status-holder)
		    "Invalid in optarglist-decl-entry-parser-vocab sym"))))
	(make-optarglist-entry
	  (create-lexical-binding+marks expr))))

    (add-list-micro optarglist-decl-entry-parser-vocab
      (let* ((kwd '())
	      (in-pattern '(var val))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env attributes vocab)
	  (let ((status-holder (get-attribute attributes 'optarglist-status)))
	    (case (unbox status-holder)
	      ((proper) (set-box! status-holder 'proper/defaults))
	      ((improper) (set-box! status-holder 'improper/defaults))
	      ((proper/defaults improper/defaults) (void))
	      ((improper/done) (static-error expr
				 "Invalid default value specification"))
	      (else (internal-error (unbox status-holder)
		      "Invalid in optarglist-decl-entry-parser-vocab list"))))
	  (cond
	    ((pat:match-against m&e expr env)
	      =>
	      (lambda (p-env)
		(let ((var (pat:pexpand 'var p-env kwd))
		       (val (pat:pexpand 'val p-env kwd)))
		  (valid-syntactic-id? var)
		  (make-initialized-optarglist-entry
		    (create-lexical-binding+marks var)
		    val))))
	    (else
	      (static-error expr "Invalid init-var declaration"))))))

    (define optarglist-decls-vocab (make-vocabulary))

    (add-sym-micro optarglist-decls-vocab
      (lambda (expr env attributes vocab)
	(make-sym-optarglist
	  (list
	    (make-optarglist-entry
	      (create-lexical-binding+marks expr))))))

    (add-list-micro optarglist-decls-vocab
      (lambda (expr env attributes vocab)
	(let ((expr (expose-list expr))
	       (new-attr (put-attribute attributes 'optarglist-status
			   (box 'proper))))
	  (make-list-optarglist
	    (map (lambda (decl)
		   (expand-expr decl env new-attr
		     optarglist-decl-entry-parser-vocab))
	      expr)))))

    (add-ilist-micro optarglist-decls-vocab
      (lambda (expr env attributes vocab)
	(let ((expr-list (expose-list expr))
	       (new-attr (put-attribute attributes 'optarglist-status
			   (box 'improper))))
	  (let ((result
		  (map (lambda (decl)
			 (expand-expr decl env new-attr
			   optarglist-decl-entry-parser-vocab))
		    expr-list)))
	    (let loop ((result result) (exprs expr-list))
	      (if (null? (cdr result))
		(when (initialized-optarglist-entry? (car result))
		  (static-error (car exprs)
		    "Last argument must not have an initial value"))
		(loop (cdr result) (cdr exprs))))
	    (make-ilist-optarglist result)))))

    (define make-optargument-list
      (lambda (optarglist env attributes vocab)
	(let ((result
		(map
		  (lambda (e)
		    (extend-env (list (optarglist-entry-var+marks e)) env)
		    (if (initialized-optarglist-entry? e)
		      (cons
			(car (optarglist-entry-var+marks e))
			(expand-expr
			  (initialized-optarglist-entry-expr
			    e)
			  env attributes vocab))
		      (car (optarglist-entry-var+marks e))))
		  (optarglist-vars optarglist))))
	  (cond
	    ((sym-optarglist? optarglist)
	      (make-sym-optarglist result))
	    ((list-optarglist? optarglist)
	      (make-list-optarglist result))
	    ((ilist-optarglist? optarglist)
	      (make-ilist-optarglist result))
	    (else
	      (internal-error optarglist
		"Invalid in make-optargument-list"))))))

    ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    (extend-parsed->raw optarglist?
      (lambda (expr p->r)
	(let ((process-args
		(lambda (element)
		  (if (pair? element)
		    (list (p->r (car element)) (p->r (cdr element)))
		    (p->r element)))))
	  (cond
	    ((sym-optarglist? expr)
	      (process-args (car (optarglist-vars expr))))
	    ((list-optarglist? expr)
	      (map process-args (optarglist-vars expr)))
	    ((ilist-optarglist? expr)
	      (let loop ((vars (map process-args (optarglist-vars expr))))
		(cond
		  ((null? (cddr vars))
		    (cons (car vars) (cadr vars)))
		  (else
		    (cons (car vars) (loop (cdr vars)))))))
	    (else
	      (internal-error expr "p->r: not an optarglist"))))))

    ; ----------------------------------------------------------------------

    (define paroptarglist-pattern 'vars)

    (define-struct paroptarglist-entry (var+marks))
    (define-struct (initialized-paroptarglist-entry struct:paroptarglist-entry)
      (expr))

    (define-struct paroptarglist (vars))
    (define-struct (sym-paroptarglist struct:paroptarglist) ())
    (define-struct (list-paroptarglist struct:paroptarglist) ())
    (define-struct (ilist-paroptarglist struct:paroptarglist) ())

    ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    (define paroptarglist-decl-entry-parser-vocab (make-vocabulary))

    (add-sym-micro paroptarglist-decl-entry-parser-vocab
      (lambda (expr env attributes vocab)
	(let ((status-holder (get-attribute attributes 'paroptarglist-status)))
	  (case (unbox status-holder)
	    ((proper improper) (void))
	    ((proper/defaults)
	      (static-error expr
		"Appears after initial value specifications"))
	    ((improper/defaults)
	      (set-box! status-holder 'improper/done))
	    ((improper/done)
	      (static-error expr
		"Appears past catch-all argument"))
	    (else (internal-error (unbox status-holder)
		    "Invalid in paroptarglist-decl-entry-parser-vocab sym"))))
	(make-paroptarglist-entry
	  (create-lexical-binding+marks expr))))

    (add-list-micro paroptarglist-decl-entry-parser-vocab
      (let* ((kwd '())
	      (in-pattern '(var val))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env attributes vocab)
	  (let ((status-holder (get-attribute attributes 'paroptarglist-status)))
	    (case (unbox status-holder)
	      ((proper) (set-box! status-holder 'proper/defaults))
	      ((improper) (set-box! status-holder 'improper/defaults))
	      ((proper/defaults improper/defaults) (void))
	      ((improper/done) (static-error expr
				 "Invalid default value specification"))
	      (else (internal-error (unbox status-holder)
		      "Invalid in paroptarglist-decl-entry-parser-vocab list"))))
	  (cond
	    ((pat:match-against m&e expr env)
	      =>
	      (lambda (p-env)
		(let ((var (pat:pexpand 'var p-env kwd))
		       (val (pat:pexpand 'val p-env kwd)))
		  (valid-syntactic-id? var)
		  (make-initialized-paroptarglist-entry
		    (create-lexical-binding+marks var)
		    val))))
	    (else
	      (static-error expr "Invalid init-var declaration"))))))

    (define paroptarglist-decls-vocab (make-vocabulary))

    (add-sym-micro paroptarglist-decls-vocab
      (lambda (expr env attributes vocab)
	(make-sym-paroptarglist
	  (list
	    (make-paroptarglist-entry
	      (create-lexical-binding+marks expr))))))

    (add-list-micro paroptarglist-decls-vocab
      (lambda (expr env attributes vocab)
	(let ((expr (expose-list expr))
	       (new-attr (put-attribute attributes 'paroptarglist-status
			   (box 'proper))))
	  (make-list-paroptarglist
	    (map (lambda (decl)
		   (expand-expr decl env new-attr
		     paroptarglist-decl-entry-parser-vocab))
	      expr)))))

    (add-ilist-micro paroptarglist-decls-vocab
      (lambda (expr env attributes vocab)
	(let ((expr-list (expose-list expr))
	       (new-attr (put-attribute attributes 'paroptarglist-status
			   (box 'improper))))
	  (let ((result
		  (map (lambda (decl)
			 (expand-expr decl env new-attr
			   paroptarglist-decl-entry-parser-vocab))
		    expr-list)))
	    (let loop ((result result) (exprs expr-list))
	      (if (null? (cdr result))
		(when (initialized-paroptarglist-entry? (car result))
		  (static-error (car exprs)
		    "Last argument must not have an initial value"))
		(loop (cdr result) (cdr exprs))))
	    (make-ilist-paroptarglist result)))))

    (define make-paroptargument-list
      (lambda (paroptarglist env attributes vocab)
	(extend-env
	  (map paroptarglist-entry-var+marks
	    (paroptarglist-vars paroptarglist))
	  env)
	(let ((result
		(map
		  (lambda (e)
		    (if (initialized-paroptarglist-entry? e)
		      (cons
			(car (paroptarglist-entry-var+marks e))
			(expand-expr
			  (initialized-paroptarglist-entry-expr
			    e)
			  env attributes vocab))
		      (car (paroptarglist-entry-var+marks e))))
		  (paroptarglist-vars paroptarglist))))
	  (cond
	    ((sym-paroptarglist? paroptarglist)
	      (make-sym-paroptarglist result))
	    ((list-paroptarglist? paroptarglist)
	      (make-list-paroptarglist result))
	    ((ilist-paroptarglist? paroptarglist)
	      (make-ilist-paroptarglist result))
	    (else
	      (internal-error paroptarglist
		"Invalid in make-paroptargument-list"))))))

    ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    (extend-parsed->raw paroptarglist?
      (lambda (expr p->r)
	(let ((process-args
		(lambda (element)
		  (if (pair? element)
		    (list (p->r (car element)) (p->r (cdr element)))
		    (p->r element)))))
	  (cond
	    ((sym-paroptarglist? expr)
	      (process-args (car (paroptarglist-vars expr))))
	    ((list-paroptarglist? expr)
	      (map process-args (paroptarglist-vars expr)))
	    ((ilist-paroptarglist? expr)
	      (let loop ((vars (map process-args (paroptarglist-vars expr))))
		(cond
		  ((null? (cddr vars))
		    (cons (car vars) (cadr vars)))
		  (else
		    (cons (car vars) (loop (cdr vars)))))))
	    (else
	      (internal-error expr "p->r: not an paroptarglist"))))))

    ; ----------------------------------------------------------------------

    (define arglist-pattern 'args)

    (define-struct arglist (vars))
    (define-struct (sym-arglist struct:arglist) ())
    (define-struct (list-arglist struct:arglist) ())
    (define-struct (ilist-arglist struct:arglist) ())

    ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    (define arglist-decls-vocab (make-vocabulary))

    (add-sym-micro arglist-decls-vocab
      (lambda (expr env attributes vocab)
	(make-sym-arglist
	  (list
	    (create-lexical-binding+marks expr)))))

    (add-list-micro arglist-decls-vocab
      (lambda (expr env attributes vocab)
	(make-list-arglist
	  (map create-lexical-binding+marks
	    (expose-list expr)))))

    (add-ilist-micro arglist-decls-vocab
      (lambda (expr env attributes vocab)
	(make-ilist-arglist
	  (map create-lexical-binding+marks
	    (expose-list expr)))))

    ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    (define make-argument-list
      (lambda (arglist)
	(cond
	  ((sym-arglist? arglist)
	    (make-sym-arglist
	      (map car (arglist-vars arglist))))
	  ((list-arglist? arglist)
	    (make-list-arglist
	      (map car (arglist-vars arglist))))
	  ((ilist-arglist? arglist)
	    (make-ilist-arglist
	      (map car (arglist-vars arglist))))
	  (else
	    (internal-error arglist "Invalid in make-argument-list")))))

    (extend-parsed->raw arglist?
      (lambda (expr p->r)
	(cond
	  ((sym-arglist? expr)
	    (p->r (car (arglist-vars expr))))
	  ((list-arglist? expr)
	    (map p->r (arglist-vars expr)))
	  ((ilist-arglist? expr)
	    (let loop ((vars (map p->r (arglist-vars expr))))
	      (cond
		((null? (cddr vars))
		  (cons (car vars) (cadr vars)))
		(else
		  (cons (car vars) (loop (cdr vars)))))))
	  (else
	    (internal-error expr "p->r: not an arglist")))))

    ))
