
;; Used by ../shared.ss, and also collects/lang/private/teach.ss
;; Besides the usual things, this code expects `undefined' and
;; `the-cons' to be bound.

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
			     [(#%app e ...)
			      (syntax (e ...))]
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
			       [(make-x . _)
				(struct-decl-for (syntax make-x))
				(let ([decl (struct-decl-for (syntax make-x))]
				      [args (syntax->list (syntax _))])
				  expr)]
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
				    (syntax (begin 
					      (set-car! name a)
					      (set-cdr! name d)))]
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
			     names exprs))]
		     [(check-expr ...)
		      (if make-check-cdr
			  (map (lambda (name expr)
				 (syntax-case* expr (the-cons list box vector) 
				     module-or-top-identifier=?
				   [(the-cons a d)
				    (make-check-cdr name)]
				   [_else (syntax #t)]))
			       names exprs)
			  null)])
	 (syntax
	  (letrec ([name init-expr] ...)
	    finish-expr
	    ...
	    check-expr
	    ...
	    body1
	    body
	    ...)))))])
