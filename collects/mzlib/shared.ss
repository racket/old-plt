
(module shared mzscheme

  (export shared)

  (define undefined (letrec ([x x]) x))
  (export-indirect undefined)
  
  (define-syntax shared
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
	   (with-syntax ([(init-expr ...)
			  (map (lambda (expr)
				 (define (bad n)
				   (raise-syntax-error
				    'shared
				    (format "illegal use of ~a" n)
				    stx
				    expr))
				 (syntax-case expr (cons list box vector)
				   [(cons a d)
				    (syntax (cons undefined undefined))]
				   [(cons . _)
				    (bad "list")]
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
					       (cons (datum->syntax n #f (quote-syntax here))
						     (loop (cdr l) (add1 n))))))])
			    (map (lambda (name expr)
				   (with-syntax ([name name])
				     (syntax-case expr (cons list box vector)
				       [(cons a d)
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
				 names exprs))])
	     (syntax
	      (letrec ([name init-expr] ...)
		finish-expr
		...
		body1
		body
		...))))]))))

				    
	 