
;; Used by ../shared.ss, and also collects/lang/private/teach.ss
;; Besides the usual things, this code expects `undefined' and
;; `the-cons' to be bound.

(lambda (stx)
  (syntax-case stx ()
    [(_ ([name expr] ...) body1 body ...)
     (let ([names (syntax->list (syntax (name ...)))]
	   [exprs (syntax->list (syntax (expr ...)))])
       (for-each (lambda (name)
		   (unless (identifier? name)
		     (raise-syntax-error
		      'shared
		      "not an identifier"
		      stx
		      name)))
		 names)
       (let ([dup (check-duplicate-identifier names)])
	 (when dup
	   (raise-syntax-error
	    'shared
	    "duplicate identifier"
	    stx
	    dup)))
       (let ([exprs (map (lambda (expr)
			   (let ([e (local-expand
				     expr
				     'expression
				     (append
				      (kernel-form-identifier-list (quote-syntax here))
				      names))])
			     ;; Remove #%app if present...
			     (syntax-case e (#%app)
			       [(#%app . rest)
				(syntax rest)]
			       [_else e])))
			 exprs)])
	 (with-syntax ([(init-expr ...)
			(map (lambda (expr)
			       (define (bad n)
				 (raise-syntax-error
				  'shared
				  (format "illegal use of ~a" n)
				  stx
				  expr))
			       (syntax-case* expr (the-cons list box vector) module-or-top-identifier=?
				 [(the-cons a d)
				  (syntax (cons undefined undefined))]
				 [(the-cons . _)
				  (bad "cons")]
				 [(list e ...)
				  (with-syntax ([(e ...)
						 (map (lambda (x) (syntax undefined))
						      (syntax->list (syntax (e ...))))])
				    (syntax (list e ...)))]
				 [(list . _)
				  (bad "list")]
				 [(box v)
				  (syntax (box undefined))]
				 [(box . _)
				  (bad "box")]
				 [(vector e ...)
				  (with-syntax ([(e ...)
						 (map (lambda (x) (syntax undefined))
						      (syntax->list (syntax (e ...))))])
				    (syntax (vector e ...)))]
				 [(vector . _)
				  (bad "vector")]
				 [_else expr]))
			     exprs)]
		       [(finish-expr ...)
			(let ([gen-n (lambda (l)
				       (let loop ([l l][n 0])
					 (if (null? l)
					     null
					     (cons (datum->syntax-object (quote-syntax here) n #f)
						   (loop (cdr l) (add1 n))))))])
			  (map (lambda (name expr)
				 (with-syntax ([name name])
				   (syntax-case* expr (the-cons list box vector) module-or-top-identifier=?
				     [(the-cons a d)
				      (if (module-identifier=? (quote-syntax the-cons)
							       (quote-syntax cons))
					  ;; normal cons
					  (syntax (begin 
						    (set-car! name a)
						    (set-cdr! name d)))
					  ;; cons that wants list cdrs only:
					  (syntax (begin 
						    (let ([va a]
							  [vd d])
						      (unless (list? vd)
							(raise-type-error
							 'cons
							 "list"
							 1
							 va
							 vd))
						      (set-car! name va)
						      (set-cdr! name vd)))))]
				     [(list e ...)
				      (with-syntax ([(n ...) (gen-n (syntax->list (syntax (e ...))))])
					(syntax (let ([lst name])
						  (set-car! (list-tail lst n) e)
						  ...)))]
				     [(box v)
				      (syntax (set-box! name v))]
				     [(vector e ...)
				      (with-syntax ([(n ...) (gen-n (syntax->list (syntax (e ...))))])
					(syntax (let ([vec name])
						  (vector-set! vec n e)
						  ...)))]
				     [_else (syntax (void))])))
			       names exprs))])
	   (syntax
	    (letrec ([name init-expr] ...)
	      finish-expr
	      ...
	      body1
	      body
	      ...)))))]))
