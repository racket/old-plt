(define zodiac:scheme-objects@
  (unit/sig zodiac:scheme-objects^
    (import zodiac:misc^ (z : zodiac:structures^) (z : zodiac:reader-structs^)
      zodiac:sexp^ (pat : zodiac:pattern^) zodiac:scheme-core^
      zodiac:scheme-main^ zodiac:back-protocol^
      zodiac:expander^ zodiac:interface^)

    (define-struct (class*/names-form struct:parsed)
      (this super-init super-expr interfaces init-vars inst-clauses))

    (define-struct (interface-form struct:parsed)
      (super-exprs variables))

    (define create-class*/names-form
      (lambda (this super-init super-expr interfaces
		init-vars inst-clauses source)
	(make-class*/names-form (z:zodiac-origin source)
	  (z:zodiac-start source) (z:zodiac-finish source)
	  (make-empty-back-box)
	  this super-init super-expr interfaces init-vars inst-clauses)))

    (define create-interface-form
      (lambda (super-exprs variables source)
	(make-interface-form (z:zodiac-origin source)
	  (z:zodiac-start source) (z:zodiac-finish source)
	  (make-empty-back-box)
	  super-exprs variables)))

    (define-struct (supervar-binding struct:binding) ())
    (define-struct (superinit-binding struct:binding) ())
    (define-struct (public-binding struct:binding) ())
    (define-struct (private-binding struct:binding) ())
    (define-struct (inherit-binding struct:binding) ())
    (define-struct (rename-binding struct:binding) ())

    (define create-supervar-binding+marks
      (create-binding+marks make-supervar-binding))
    (define create-superinit-binding+marks
      (create-binding+marks make-superinit-binding))
    (define create-public-binding+marks
      (create-binding+marks make-public-binding))
    (define create-private-binding+marks
      (create-binding+marks make-private-binding))
    (define create-inherit-binding+marks
      (create-binding+marks make-inherit-binding))
    (define create-rename-binding+marks
      (create-binding+marks make-rename-binding))

    (define-struct (supervar-varref struct:bound-varref) ())
    (define-struct (superinit-varref struct:bound-varref) ())
    (define-struct (public-varref struct:bound-varref) ())
    (define-struct (private-varref struct:bound-varref) ())
    (define-struct (inherit-varref struct:bound-varref) ())
    (define-struct (rename-varref struct:bound-varref) ())

    (define create-supervar-varref
      (create-bound-varref make-supervar-varref))
    (define create-superinit-varref
      (create-bound-varref make-superinit-varref))
    (define create-public-varref
      (create-bound-varref make-public-varref))
    (define create-private-varref
      (create-bound-varref make-private-varref))
    (define create-inherit-varref
      (create-bound-varref make-inherit-varref))
    (define create-rename-varref
      (create-bound-varref make-rename-varref))

    (define-struct public-clause (exports internals exprs))
    (define-struct private-clause (internals exprs))
    (define-struct inherit-clause (internals imports))
    (define-struct rename-clause (internals imports))
    (define-struct sequence-clause (exprs))

    ; --------------------------------------------------------------------

    (let
      ((interface-handler
	 (lambda (i-kwd)
	   (add-micro-form i-kwd scheme-vocabulary
	     (let* ((kwd (list i-kwd))
		     (in-pattern `(,i-kwd
				    (super-interfaces ...)
				    variables ...))
		     (m&e (pat:make-match&env in-pattern kwd)))
	       (lambda (expr env attributes vocab)
		 (cond
		   ((pat:match-against m&e expr env)
		     =>
		     (lambda (p-env)
		       (let ((super-interfaces
			       (pat:pexpand '(super-interfaces ...) p-env kwd))
			      (variables
				(pat:pexpand '(variables ...) p-env kwd)))
			 (distinct-valid-syntactic-id/s? variables)
			 (let* ((top-level? (get-top-level-status
					      attributes))
				 (_ (set-top-level-status attributes))
				 (proc:super-interfaces
				   (map (lambda (e)
					  (expand-expr e env
					    attributes vocab))
				     super-interfaces)))
			   (set-top-level-status attributes top-level?)
			   (create-interface-form
			     proc:super-interfaces
			     variables
			     expr)))))
		   (else
		     (static-error expr "Malformed interface")))))))))
      (interface-handler 'interface)
      (interface-handler '#%interface))

    ; ----------------------------------------------------------------------

    (add-sym-micro scheme-vocabulary
      (lambda (expr env attributes vocab)
	(let ((r (resolve expr env vocab)))
	  (cond
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
	    ((public-binding? r)
	      (create-public-varref r expr))
	    ((private-binding? r)
	      (create-private-varref r expr))
	    ((inherit-binding? r)
	      (create-inherit-varref r expr))
	    ((rename-binding? r)
	      (create-rename-varref r expr))
	    ((supervar-binding? r)
	      (create-supervar-varref r expr))
	    ((superinit-binding? r)
	      (create-superinit-varref r expr))
	    ((or (macro-resolution? r) (micro-resolution? r))
	      (static-error expr
		"Invalid use of keyword ~s" (z:symbol-orig-name expr)))
	    (else
	      (internal-error expr "Invalid resolution in obj: ~s" r))))))

    ; ----------------------------------------------------------------------

    (define-struct ivar-entry (bindings))
    (define-struct (public-entry struct:ivar-entry) (exports exprs))
    (define-struct (private-entry struct:ivar-entry) (exprs))
    (define-struct (inherit-entry struct:ivar-entry) (imports))
    (define-struct (rename-entry struct:ivar-entry) (imports))

    (define-struct sequence-entry (exprs))

    ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

    (define make-void-init-expr
      (lambda (expr)
	(structurize-syntax '(#%void) expr '(-1))))

    ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

    (define ivar-decls-vocab (create-vocabulary 'ivar-decls-vocab))

    (define public-ivar-decl-entry-parser-vocab
      (create-vocabulary 'public-ivar-decl-entry-parser-vocab))

    (add-sym-micro public-ivar-decl-entry-parser-vocab
      (lambda (expr env attributes vocab)
	(list
	  (create-public-binding+marks expr)
	  expr
	  (make-void-init-expr expr))))

    (add-list-micro public-ivar-decl-entry-parser-vocab
      (let* ((kwd '())
	      (in-pattern-1 '((internal-var var) expr))
	      (in-pattern-2 '(var expr))
	      (in-pattern-3 '(var))
	      (m&e-1 (pat:make-match&env in-pattern-1 '()))
	      (m&e-2 (pat:make-match&env in-pattern-2 '()))
	      (m&e-3 (pat:make-match&env in-pattern-3 '())))
	(lambda (expr env attributes vocab)
	  (cond
	    ((pat:match-against m&e-1 expr env)
	      =>
	      (lambda (p-env)
		(let ((internal-var (pat:pexpand 'internal-var p-env kwd))
		       (var (pat:pexpand 'var p-env kwd))
		       (expr (pat:pexpand 'expr p-env kwd)))
		  (valid-syntactic-id? internal-var)
		  (valid-syntactic-id? var)
		  (list (create-public-binding+marks internal-var) var expr))))
	    ((pat:match-against m&e-2 expr env)
	      =>
	      (lambda (p-env)
		(let ((var (pat:pexpand 'var p-env kwd))
		       (expr (pat:pexpand 'expr p-env kwd)))
		  (valid-syntactic-id? var)
		  (list (create-public-binding+marks var) var expr))))
	    ((pat:match-against m&e-3 expr env)
	      =>
	      (lambda (p-env)
		(let ((var (pat:pexpand 'var p-env kwd)))
		  (valid-syntactic-id? var)
		  (list
		    (create-public-binding+marks var)
		    var
		    (make-void-init-expr expr)))))
	    (else
	      (static-error expr "Invalid ivar declaration"))))))

    (let* ((kwd '(public))
	    (in-pattern '(public ivar-decl ...))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (add-micro-form 'public ivar-decls-vocab
	(lambda (expr env attributes vocab)
	  (cond
	    ((pat:match-against m&e expr env)
	      =>
	      (lambda (p-env)
		(let ((decls
			(map (lambda (decl)
			       (expand-expr decl env attributes
				 public-ivar-decl-entry-parser-vocab))
			  (pat:pexpand '(ivar-decl ...) p-env kwd))))
		  (make-public-entry
		    (map car decls)
		    (map cadr decls)
		    (map caddr decls)))))
	    (else
	      (static-error expr "Invalid public clause"))))))

    ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

    (define private-ivar-decl-entry-parser-vocab
      (create-vocabulary 'private-ivar-decl-entry-parser-vocab))

    (add-sym-micro private-ivar-decl-entry-parser-vocab
      (lambda (expr env attributes vocab)
	(cons (create-private-binding+marks expr)
	  (make-void-init-expr expr))))

    (add-list-micro private-ivar-decl-entry-parser-vocab
      (let* ((kwd '())
	      (in-pattern-1 '(var expr))
	      (in-pattern-2 '(var))
	      (m&e-1 (pat:make-match&env in-pattern-1 '()))
	      (m&e-2 (pat:make-match&env in-pattern-2 '())))
	(lambda (expr env attributes vocab)
	  (cond
	    ((pat:match-against m&e-1 expr env)
	      =>
	      (lambda (p-env)
		(let ((var (pat:pexpand 'var p-env kwd))
		       (expr (pat:pexpand 'expr p-env kwd)))
		  (valid-syntactic-id? var)
		  (cons (create-private-binding+marks var) expr))))
	    ((pat:match-against m&e-2 expr env)
	      =>
	      (lambda (p-env)
		(let ((var (pat:pexpand 'var p-env kwd)))
		  (valid-syntactic-id? var)
		  (cons (create-private-binding+marks var)
		    (make-void-init-expr expr)))))
	    (else
	      (static-error expr "Invalid ivar declaration"))))))

    (let* ((kwd '(private))
	    (in-pattern '(private ivar-decl ...))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (add-micro-form 'private ivar-decls-vocab
	(lambda (expr env attributes vocab)
	  (cond
	    ((pat:match-against m&e expr env)
	      =>
	      (lambda (p-env)
		(let ((decls
			(map (lambda (decl)
			       (expand-expr decl env attributes
				 private-ivar-decl-entry-parser-vocab))
			  (pat:pexpand '(ivar-decl ...) p-env kwd))))
		  (make-private-entry
		    (map car decls)
		    (map cdr decls)))))
	    (else
	      (static-error expr "Invalid private clause"))))))

    ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

    (define inherit-ivar-decl-entry-parser-vocab
      (create-vocabulary 'inherit-ivar-decl-entry-parser-vocab))

    (add-sym-micro inherit-ivar-decl-entry-parser-vocab
      (lambda (expr env attributes vocab)
	(cons
	  (create-inherit-binding+marks expr)
	  expr)))

    (add-list-micro inherit-ivar-decl-entry-parser-vocab
      (let* ((kwd '())
	      (in-pattern '(internal-var var))
	      (m&e (pat:make-match&env in-pattern '())))
	(lambda (expr env attributes vocab)
	  (cond
	    ((pat:match-against m&e expr env)
	      =>
	      (lambda (p-env)
		(let ((internal-var (pat:pexpand 'internal-var p-env kwd))
		       (var (pat:pexpand 'var p-env kwd)))
		  (valid-syntactic-id? internal-var)
		  (valid-syntactic-id? var)
		  (cons
		    (create-inherit-binding+marks internal-var)
		    var))))
	    (else
	      (static-error expr "Invalid ivar declaration"))))))

    (let* ((kwd '(inherit))
	    (in-pattern '(inherit ivar-decl ...))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (add-micro-form 'inherit ivar-decls-vocab
	(lambda (expr env attributes vocab)
	  (cond
	    ((pat:match-against m&e expr env)
	      =>
	      (lambda (p-env)
		(let ((decls
			(map (lambda (decl)
			       (expand-expr decl env attributes
				 inherit-ivar-decl-entry-parser-vocab))
			  (pat:pexpand '(ivar-decl ...) p-env kwd))))
		  (make-inherit-entry
		    (map car decls)
		    (map cdr decls)))))
	    (else
	      (static-error expr "Invalid inherit clause"))))))

    ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

    (define rename-ivar-decl-entry-parser-vocab
      (create-vocabulary 'rename-ivar-decl-entry-parser-vocab))

    (add-list-micro rename-ivar-decl-entry-parser-vocab
      (let* ((kwd '())
	      (in-pattern-1 '(var inherited-var))
	      (m&e-1 (pat:make-match&env in-pattern-1 '())))
	(lambda (expr env attributes vocab)
	  (cond
	    ((pat:match-against m&e-1 expr env)
	      =>
	      (lambda (p-env)
		(let ((var (pat:pexpand 'var p-env kwd))
		       (inherited-var (pat:pexpand 'inherited-var p-env kwd)))
		  (valid-syntactic-id? var)
		  (valid-syntactic-id? inherited-var)
		  (cons (create-rename-binding+marks var) inherited-var))))
	    (else
	      (static-error expr "Invalid ivar declaration"))))))

    (let* ((kwd '(rename))
	    (in-pattern '(rename ivar-decl ...))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (add-micro-form 'rename ivar-decls-vocab
	(lambda (expr env attributes vocab)
	  (cond
	    ((pat:match-against m&e expr env)
	      =>
	      (lambda (p-env)
		(let ((decls
			(map (lambda (decl)
			       (expand-expr decl env attributes
				 rename-ivar-decl-entry-parser-vocab))
			  (pat:pexpand '(ivar-decl ...) p-env kwd))))
		  (make-rename-entry
		    (map car decls)
		    (map cdr decls)))))
	    (else
	      (static-error expr "Invalid rename clause"))))))

    ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

    (let* ((kwd '(sequence))
	    (in-pattern '(sequence expr ...))
	    (m&e (pat:make-match&env in-pattern kwd)))
      (add-micro-form 'sequence ivar-decls-vocab
	(lambda (expr env attributes vocab)
	  (cond
	    ((pat:match-against m&e expr env)
	      =>
	      (lambda (p-env)
		(make-sequence-entry
		  (pat:pexpand '(expr ...) p-env kwd))))
	    (else
	      (static-error expr "Invalid sequence clause"))))))

    ; ----------------------------------------------------------------------

    (let ((handler
	    (lambda (c-kwd)
	      (let* ((kwd '())
		      (in-pattern `(,c-kwd super args insts ...))
		      (out-pattern '(class*/names (this super-init)
				      super () args insts ...))
		      (m&e (pat:make-match&env in-pattern kwd)))
		(add-micro-form c-kwd scheme-vocabulary
		  (lambda (expr env attributes vocab)
		    (cond
		      ((pat:match-against m&e expr env)
			=>
			(lambda (p-env)
			  (let* ((kwd-pos (pat:pexpand c-kwd p-env kwd))
				  (captured-this
				    (introduce-identifier 'this kwd-pos))
				  (captured-super-init
				    (introduce-identifier 'super-init kwd-pos))
				  (new-p-env (pat:extend-penv
					       'this captured-this
					       (pat:extend-penv
						 'super-init
						 captured-super-init
						 p-env))))
			    (expand-expr
			      (structurize-syntax
				(pat:pexpand out-pattern new-p-env kwd)
				expr '(-1))
			      env attributes vocab))))
		      (else
			(static-error expr "Malformed class")))))))))
      (handler 'class)
      (handler '#%class))

    (let ((handler
	    (lambda (c-kwd)
	      (let* ((kwd '())
		      (in-pattern `(,c-kwd super interfaces args insts ...))
		      (out-pattern '(class*/names (this super-init)
				      super interfaces args insts ...))
		      (m&e (pat:make-match&env in-pattern kwd)))
		(add-micro-form c-kwd scheme-vocabulary
		  (lambda (expr env attributes vocab)
		    (cond
		      ((pat:match-against m&e expr env)
			=>
			(lambda (p-env)
			  (let* ((kwd-pos (pat:pexpand c-kwd p-env kwd))
				  (captured-this
				    (introduce-identifier 'this kwd-pos))
				  (captured-super-init
				    (introduce-identifier 'super-init kwd-pos))
				  (new-p-env (pat:extend-penv
					       'this captured-this
					       (pat:extend-penv
						 'super-init
						 captured-super-init
						 p-env))))
			    (expand-expr
			      (structurize-syntax
				(pat:pexpand out-pattern new-p-env kwd)
				expr '(-1))
			      env attributes vocab))))
		      (else
			(static-error expr "Malformed class*")))))))))
      (handler 'class*)
      (handler '#%class*))

    (define flag-non-supervar
      (lambda (super env)
	(unless (supervar-binding?
		  (resolve-in-env (z:read-object super)
		    (z:symbol-marks super) env))
	  (static-error super "Not a superclass reference"))))

    (let
      ((class*/names-handler
	 (lambda (c-kwd)
	   (add-micro-form c-kwd scheme-vocabulary
	     (let* ((kwd '())
		     (in-pattern `(,c-kwd (this super-init)
				    super-expr
				    (interface ...)
				    ,paroptarglist-pattern
				    inst-vars ...))
		     (m&e (pat:make-match&env in-pattern kwd)))
	       (lambda (expr env attributes vocab)
		 (cond
		   ((pat:match-against m&e expr env)
		     =>
		     (lambda (p-env)
		       (let ((in:this (pat:pexpand 'this p-env kwd))
			      (in:superinit (pat:pexpand 'super-init
					       p-env kwd))
			      (in:super-expr (pat:pexpand 'super-expr
					       p-env kwd))
			      (in:interfaces (pat:pexpand '(interface ...)
					       p-env kwd))
			      (in:initvars (pat:pexpand `,paroptarglist-pattern
					     p-env kwd))
			      (in:ivars (pat:pexpand '(inst-vars ...)
					  p-env kwd)))
			 (valid-syntactic-id? in:this)
			 (valid-syntactic-id? in:superinit)
			 (let* ((top-level? (get-top-level-status
					      attributes))
				 (_ (set-top-level-status attributes))
				 (proc:superinit
				   (create-superinit-binding+marks
				     in:superinit))
				 (proc:super-expr
				   (expand-expr in:super-expr env
				     attributes vocab))
				 (proc:interfaces
				   (map (lambda (e)
					  (expand-expr e env
					    attributes vocab))
				     in:interfaces))
				 (proc:this (create-lexical-binding+marks
					      in:this))
				 (proc:initvar-info
				   (expand-expr in:initvars env attributes
				     paroptarglist-decls-vocab))
				 (proc:ivar-info
				   (map (lambda (iv-decl)
					  (expand-expr iv-decl env attributes
					    ivar-decls-vocab))
				     in:ivars)))
			   (let ((proc:initvars
				   (map paroptarglist-entry-var+marks
				     (paroptarglist-vars
				       proc:initvar-info)))
				  (proc:ivars
				    (apply append
				      (map (lambda (i)
					     (if (ivar-entry? i)
					       (ivar-entry-bindings i)
					       '()))
					proc:ivar-info))))
			     (let ((extensions
				     (cons proc:this
				       (cons proc:superinit
					 proc:ivars))))
			       (let* ((new-names (map car extensions))
				       (parsed-initvars
					 (make-paroptargument-list
					   proc:initvar-info
					   env attributes vocab)))
				 (distinct-valid-id/s? (append new-names
							 (map car
							   proc:initvars)))
				 (extend-env extensions env)
				 (let
				   ((result
				      (create-class*/names-form
					(car proc:this)
					(car proc:superinit)
					proc:super-expr
					proc:interfaces
					parsed-initvars
					(let ((expand-exprs
						(lambda (exprs)
						  (map (lambda (expr)
							 (expand-expr expr env
							   attributes vocab))
						    exprs))))
					  (map
					    (lambda (e)
					      (cond
						((public-entry? e)
						  (make-public-clause
						    (public-entry-exports e)
						    (map car (ivar-entry-bindings e))
						    (expand-exprs
						      (public-entry-exprs e))))
						((private-entry? e)
						  (make-private-clause
						    (map car (ivar-entry-bindings e))
						    (expand-exprs
						      (private-entry-exprs e))))
						((inherit-entry? e)
						  (make-inherit-clause
						    (map car
						      (ivar-entry-bindings e))
						    (inherit-entry-imports e)))
						((rename-entry? e)
						  (make-rename-clause
						    (map car (ivar-entry-bindings e))
						    (rename-entry-imports e)))
						((sequence-entry? e)
						  (make-sequence-clause
						    (expand-exprs
						      (sequence-entry-exprs e))))
						(else
						  (internal-error e
						    "Invalid entry in class*/names maker"))))
					    proc:ivar-info))
					expr)))
				   (retract-env (append
						  (map car proc:initvars)
						  new-names)
				     env)
				   (set-top-level-status attributes
				     top-level?)
				   result))))))))
		   (else
		     (static-error expr "Malformed class*/names")))))))))
      (class*/names-handler 'class*/names)
      (class*/names-handler '#%class*/names))

    ; ----------------------------------------------------------------------

    (add-micro-form 'ivar scheme-vocabulary
      (let* ((kwd '(ivar))
	      (in-pattern '(ivar object name))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env attributes vocab)
	  (cond
	    ((pat:match-against m&e expr env)
	      =>
	      (lambda (p-env)
		(let ((object (pat:pexpand 'object p-env kwd))
		       (name (pat:pexpand 'name p-env kwd)))
		  (valid-syntactic-id? name)
		  (expand-expr
		    (structurize-syntax
		      `(#%uq-ivar ,object (quote ,name))
		      expr)
		    env attributes vocab))))
	    (else
	      (static-error expr "Malformed ivar"))))))

    (add-macro-form 'send scheme-vocabulary
      (let* ((kwd '(send))
	      (in-pattern '(send object name arg ...))
	      (out-pattern '((ivar object name) arg ...))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	    (static-error expr "Malformed send")))))

    (add-macro-form 'send* scheme-vocabulary
      (let* ((kwd '(send*))
	      (in-pattern '(send* object (n0 a0 ...) ...))
	      (m&e (pat:make-match&env in-pattern kwd))
	      (out-pattern '(begin
			      (send object n0 a0 ...)
			      ...)))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	    (static-error expr "Malformed send*")))))

    (add-macro-form 'make-generic scheme-vocabulary
      (let* ((kwd '(make-generic))
	      (in-pattern '(make-generic class name))
	      (m&e (pat:make-match&env in-pattern kwd))
	      (out-pattern '(#%uq-make-generic class (quote name))))
	(lambda (expr env)
	  (or (pat:match-and-rewrite expr m&e out-pattern kwd env)
	    (static-error expr "Malformed make-generic")))))

    ; ----------------------------------------------------------------------

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
				  (id-expr (expand-expr var-p env attributes vocab))
				  (expr-expr (expand-expr
					       (pat:pexpand 'val p-env kwd)
					       env attributes vocab)))
			    (when (or (inherit-varref? id-expr)
				    (rename-varref? id-expr))
			      (static-error var-p
				"Cannot mutate inherit's and rename's"))
			    (set-top-level-status attributes top-level?)
			    (create-set!-form id-expr expr-expr expr))
			  (static-error expr "Malformed set!")))))))))
	(set!-handler 'set!)
	(set!-handler '#%set!)))

    ; --------------------------------------------------------------------

    (extend-parsed->raw class*/names-form?
      (lambda (expr p->r)
	`(class*/names
	   (,(p->r (class*/names-form-this expr))
	     ,(p->r (class*/names-form-super-init expr)))
	   ,(p->r (class*/names-form-super-expr expr))
	   ,(map p->r (class*/names-form-interfaces expr))
	   ,(p->r (class*/names-form-init-vars expr))
	   ,@(map (lambda (clause)
		    (cond
		      ((public-clause? clause)
			`(public
			   ,@(map (lambda (internal export expr)
				    `((,(p->r internal) ,(sexp->raw export))
				       ,(p->r expr)))
			       (public-clause-internals clause)
			       (public-clause-exports clause)
			       (public-clause-exprs clause))))
		      ((private-clause? clause)
			`(private
			   ,@(map (lambda (internal expr)
				    `(,(p->r internal) ,(p->r expr)))
			       (private-clause-internals clause)
			       (private-clause-exprs clause))))
		      ((inherit-clause? clause)
			`(inherit
			   ,@(map (lambda (internal inherited)
				    `(,(p->r internal) ,(sexp->raw inherited)))
			       (inherit-clause-internals clause)
			       (inherit-clause-imports clause))))
		      ((rename-clause? clause)
			`(rename
			   ,@(map (lambda (internal inherited)
				    `(,(p->r internal) ,(sexp->raw inherited)))
			       (rename-clause-internals clause)
			       (rename-clause-imports clause))))
		      ((sequence-clause? clause)
			`(sequence
			   ,@(map p->r (sequence-clause-exprs clause))))))
	       (class*/names-form-inst-clauses expr)))))

    (extend-parsed->raw interface-form?
      (lambda (expr p->r)
	`(interface ,(map p->r (interface-form-super-exprs expr))
	   ,@(map sexp->raw (interface-form-variables expr)))))

    ))
