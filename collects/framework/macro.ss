(module macro mzscheme
  (require (lib "class100.ss")
	   (lib "class.ss"))
  (require-for-syntax (lib "class100.ss")
		      (lib "class.ss")
		      (lib "stx.ss" "syntax"))
  
  (provide mixin)

  (define-syntax mixin
    (lambda (stx)
      (syntax-case stx ()
	[(_ (from ...) (to ...) args clauses ...)
	 (with-syntax ([(from-ids ...) (generate-temporaries (syntax (from ...)))]
		       [(to-ids ...) (generate-temporaries (syntax (to ...)))]
		       [(super-vars ...)
			(apply
			 append
			 (map (lambda (stx)
				(syntax->list
				 (syntax-case stx (inherit rename override)
				   [(inherit names ...) (syntax (names ...))]
				   [(rename [x names] ...) (syntax (names ...))]
				   [(override [names bodies] ...) (syntax (names ...))]
				   [else (syntax ())])))
			      (syntax->list (syntax (clauses ...)))))]

		       ;; new syntax system mumbo jumbo to bind super-init and this
		       [this (datum->syntax-object (stx-car stx) 'this stx)]
                       [super-init (datum->syntax-object (stx-car stx) 'super-init stx)])
	   (syntax
	    (let ([from-ids from] ...)
	      (let ([to-ids to] ...)

		(let ([all-from (list from-ids ...)])
		  (void)
		  (unless (interface? from-ids)
		    (error 'mixin
                           "expected interfaces for from, got: ~e, others ~e"
			   from-ids
			   all-from)) ...)

		(let ([all-to (list to-ids ...)])
		  (void)
		  (unless (interface? to-ids)
		    (error 'mixin
                           "expected interfaces for to, got: ~e, others ~e"
			   to-ids
			   all-to)) ...)

		(let ([ensure-interface-has?
		       (lambda (x)
			 (unless (or (method-in-interface? x from-ids) ...)
			   (error 'mixin
				  "method `~a' not in any of ~a, but was referenced in definition"
				  x (list from-ids ...))))])
		  (void)
		  (ensure-interface-has? (quote super-vars)) ...)

		(lambda (super%)
		  (unless (class? super%)
		    (error 'mixin "argument ~a not a class" super%))
		  (begin
		    (void)
		    (unless (implementation? super% from-ids)
		      (error 'mixin "argument ~s does not implement ~s" super% from-ids))
		    ...)

		  (class100*/names (this super-init) super% (to-ids ...) args
		    clauses ...))))))]))))