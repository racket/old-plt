(unit/sig zodiac:scheme-mrspidey^
  (import zodiac:misc^ (z : zodiac:structures^) (z : zodiac:reader-structs^)
    zodiac:sexp^ (pat : zodiac:pattern^) zodiac:scheme-core^
    zodiac:scheme-main^ zodiac:back-protocol^
    zodiac:expander^ zodiac:interface^)

  (define-struct (poly-form struct:parsed) (exp))
  (define-struct (:-form struct:parsed) (exp type))
  (define-struct (type:-form struct:parsed) (type attrs))
  (define-struct (st:control-form struct:parsed) (para val))
  (define-struct (reference-unit-form struct:parsed) (file cd kind signed?))
  (define-struct (define-type-form struct:parsed) (sym type))
  (define-struct (define-constructor-form struct:parsed) (sym modes))

  (define create-poly-form
    (lambda (exp source)
      (make-poly-form (z:zodiac-origin source)
	(z:zodiac-start source) (z:zodiac-finish source)
	(make-empty-back-box)
	exp)))

  (define create-:-form
    (lambda (exp type source)
      (make-:-form (z:zodiac-origin source)
	(z:zodiac-start source) (z:zodiac-finish source)
	(make-empty-back-box)
	exp type)))

  (define create-type:-form
    (lambda (type attrs source)
      (make-type:-form (z:zodiac-origin source)
	(z:zodiac-start source) (z:zodiac-finish source)
	(make-empty-back-box)
	type attrs)))

  (define create-st:control-form
    (lambda (para val source)
      (make-st:control-form (z:zodiac-origin source)
	(z:zodiac-start source) (z:zodiac-finish source)
	(make-empty-back-box)
	para val)))

  (define create-reference-unit-form
    (lambda (file cd kind signed? source)
      (make-reference-unit-form (z:zodiac-origin source)
	(z:zodiac-start source) (z:zodiac-finish source)
	(make-empty-back-box)
	file cd kind signed?)))

  (define create-define-type-form
    (lambda (sym type source)
      (make-define-type-form (z:zodiac-origin source)
	(z:zodiac-start source) (z:zodiac-finish source)
	(make-empty-back-box)
	sym type)))

  (define create-define-constructor-form
    (lambda (sym modes source)
      (make-define-constructor-form (z:zodiac-origin source)
	(z:zodiac-start source) (z:zodiac-finish source)
	(make-empty-back-box)
	sym modes)))

  ; --------------------------------------------------------------------

  (define mrspidey-vocabulary
    (create-vocabulary 'mrspidey-vocabulary scheme-vocabulary))

  ; --------------------------------------------------------------------

  (add-micro-form 'poly mrspidey-vocabulary
    (let* ((kwd '(poly))
	    (in-pattern '(poly p-expr))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((p-expr (pat:pexpand 'p-expr p-env kwd)))
		(create-poly-form
		  (expand-expr p-expr env attributes vocab)
		  expr))))
	  (else
	    (static-error expr "Malformed poly"))))))

  (add-micro-form ': mrspidey-vocabulary
    (let* ((kwd '(:))
	    (in-pattern '(: :-expr type))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((:-expr (pat:pexpand ':-expr p-env kwd))
		     (type (pat:pexpand 'type p-env kwd)))
		(create-:-form
		  (expand-expr :-expr env attributes vocab)
		  (sexp->raw type)
		  expr))))
	  (else
	    (static-error expr "Malformed :"))))))

  (add-micro-form 'type: mrspidey-vocabulary
    (let* ((kwd '(type:))
	    (in-pattern '(type: type attr ...))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((type (pat:pexpand 'type p-env kwd))
		     (attrs (pat:pexpand '(attr ...) p-env kwd)))
		(create-type:-form
		  (sexp->raw type)
		  (map sexp->raw attrs)
		  expr))))
	  (else
	    (static-error expr "Malformed type:"))))))

  (add-micro-form 'st:control mrspidey-vocabulary
    (let* ((kwd '(st:control))
	    (in-pattern '(st:control para val))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((para (pat:pexpand 'para p-env kwd))
		     (val (pat:pexpand 'val p-env kwd)))
		(create-st:control-form
		  (sexp->raw para)
		  (sexp->raw val)
		  expr))))
	  (else
	    (static-error expr "Malformed st:control"))))))

  (add-micro-form 'define-type mrspidey-vocabulary
    (let* ((kwd '(define-type))
	    (in-pattern '(define-type sym type))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((sym (pat:pexpand 'sym p-env kwd))
		     (type (pat:pexpand 'type p-env kwd)))
		(valid-syntactic-id? sym)
		(create-define-type-form
		  (z:read-object sym)
		  (sexp->raw type)
		  expr))))
	  (else
	    (static-error expr "Malformed define-type"))))))

  (add-micro-form 'define-constructor mrspidey-vocabulary
    (let* ((kwd '(define-constructor))
	    (in-pattern '(define-constructor sym modes ...))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((sym (pat:pexpand 'sym p-env kwd))
		     (modes (pat:pexpand '(modes ...) p-env kwd)))
		(valid-syntactic-id? sym)
		; Cormac has an (assert-syn def (andmap boolean? modes))
		; here.  I only do the andmap z:boolean? part since
		; I have no idea what (assert-syn def ...) does.
		(map (lambda (mode)
		       (unless (z:boolean? mode)
			 (static-error mode "Malformed mode")))
		  modes)
		(create-define-constructor-form
		  (z:read-object sym)
		  (map sexp->raw modes)
		  expr))))
	  (else
	    (static-error expr "Malformed define-constructor"))))))

  (add-micro-form 'reference-unit mrspidey-vocabulary
    (let* ((kwd '(reference-unit))
	    (in-pattern '(reference-unit file))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((file (pat:pexpand 'file p-env kwd)))
		(create-reference-unit-form
		  file
		  (current-directory)
		  'exp
		  #f
		  expr))))
	  (else
	    (static-error expr "Malformed reference-unit"))))))

  (add-micro-form 'reference-unit/sig mrspidey-vocabulary
    (let* ((kwd '(reference-unit/sig))
	    (in-pattern '(reference-unit/sig file))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((file (pat:pexpand 'file p-env kwd)))
		(create-reference-unit-form
		  file
		  (current-directory)
		  'exp
		  #t
		  expr))))
	  (else
	    (static-error expr "Malformed reference-unit"))))))

'  (add-micro-form 'include-unit-imports mrspidey-vocabulary
    (let* ((kwd '(include-unit-imports))
	    (in-pattern '(include-unit-imports file))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (lambda (expr env attributes vocab)
	(cond
	  ((pat:match-against m&e expr env)
	    =>
	    (lambda (p-env)
	      (let ((file (pat:pexpand 'file p-env kwd)))
		(create-include-unit-form
		  file
		  (current-directory)
		  'imp
		  expr))))
	  (else
	    (static-error expr "Malformed include-unit-imports"))))))

  ; --------------------------------------------------------------------

  (extend-parsed->raw poly-form?
    (lambda (expr p->r)
      `(poly ,(p->r (poly-form-exp expr)))))

  (extend-parsed->raw :-form?
    (lambda (expr p->r)
      `(: ,(p->r (:-form-exp expr)) ,(:-form-type expr))))

  (extend-parsed->raw type:-form?
    (lambda (expr p->r)
      `(type: ,(type:-form-type expr) ,@(type:-form-attrs expr))))

  (extend-parsed->raw st:control-form?
    (lambda (expr p->r)
      `(st:control ,(st:control-form-para expr)
	 ,(st:control-form-val expr))))

  (extend-parsed->raw reference-unit-form?
    (lambda (expr p->r)
      (case (reference-unit-form-kind expr)
	((exp) `((if (reference-unit-form-signed? expr)
		   'reference-unit/sig
		   'reference-unit)
		  ,(sexp->raw (reference-unit-form-file expr))))
	((imp) `(reference-unit-imports
		  ,(sexp->raw (reference-unit-form-file expr))))
	(else (internal-error 'reference-unit-form "Invalid kind")))))

  (extend-parsed->raw define-type-form?
    (lambda (expr p->r)
      `(define-type ,(define-type-form-sym expr)
	 ,(define-type-form-type expr))))

  (extend-parsed->raw define-constructor-form?
    (lambda (expr p->r)
      `(define-constructor-form ,(define-constructor-form-sym expr)
	 ,@(define-constructor-form-modes expr))))

  )
