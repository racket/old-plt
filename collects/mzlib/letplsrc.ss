
(define-macro let+
  (lambda (bindings . bodies)
    (let* ([syn-error
	    (lambda (msg expr)
	      (raise-syntax-error 'let+ msg
				  `(let+ ,bindings ,@bodies)
				  expr))]
	   [expand-pattern
	    (lambda (x)
	      (match x
		[`(values ,(? symbol? x) ...) x]
		[(? symbol? x) `(,x)]
		[x (syn-error "invalid pattern" x)]))]
	   [single-binding
	    (lambda (sym binding E body)
	      `(,sym ([,(expand-pattern binding) ,E])
		     ,body))]
	   [multiple-bindings
	    (lambda (sym binding E body)
	      `(,sym ,(map list (map expand-pattern binding) E)
		     ,body))]	 
	   [translate-binding
	    (lambda (binding body)
	      (match binding
		[`(val ,B ,E) (single-binding 'let-values B E body)]
		[`(vals (,B ,E) ...) (multiple-bindings 'let-values B E body)]
		[`(rec ,B ,E) (single-binding 'letrec*-values B E body)]
		[`(recs (,B ,E) ...) (multiple-bindings 'letrec*-values B E body)]
		[`(_ ,E ...) `(begin ,@E ,body)]
		[x (syn-error "invalid binding" x)]))])
      (unless (and (list? bindings)
		   (andmap (lambda (x) (and (list? x)
					    (<= 2 (length x))
					    (symbol? (car x))))
			   bindings))
	(syn-error "invalid syntax" bindings))
      (let loop ([l bindings])
	(cond
	 [(null? l) `(begin ,@bodies)]
	 [else (translate-binding (car l) (loop (cdr l)))])))))

