;; only syntactic checking that should be
;; deferred to resultant class expression is
;; the well-formedness of the init-args.
(unit (import)
      (export class/d class/d* class/d*/names)
      
      ;; too lazy to import it....
      (define (filter p l)
	(let loop ([l l])
	  (cond
	   [(null? l) null]
	   [else (if (p (car l))
		     (cons (car l) (loop (cdr l)))
		     (loop (cdr l)))])))
      
      (define (validate-clauses clauses)
	(unless (and (list? clauses)
		     (andmap (lambda (x)
			       (and
				(list? x)
				(> (length x) 0)
				(or (eq? (car x) 'public)
				    (eq? (car x) 'rename)
				    (eq? (car x) 'inherit)
				    (eq? (car x) 'override))))
			     clauses))
	  (raise-syntax-error 'class/d "illformed clauses" clauses)))
      
      (define (extract-clause keyword well-formed-clause?)
	(lambda (clauses)
	  (let loop ([clauses clauses])
	    (cond
	     [(null? clauses) null]
	     [else
	      (let ([clause (car clauses)])
		(if (eq? (car clause) keyword)
		    (begin
		      (unless (well-formed-clause? clause)
			(raise-syntax-error 'class/d (format "malformed ~a clause: ~s" keyword clause)))
		      (append (cdr clause) (loop (cdr clauses))))
		    (loop (cdr clauses))))]))))
      
      (define extract-public-vars (extract-clause 'public (lambda (x) (andmap symbol? x))))
      (define extract-inherited-vars (extract-clause 'inherit (lambda (x) (andmap symbol? x))))
      (define extract-overriden-vars (extract-clause 'override (lambda (x) (andmap symbol? x))))
      (define extract-renamed-vars (extract-clause 'rename (lambda (x)
							     (andmap
							      (lambda (x)
								(and (list? x)
								     (= 2 (length x))
								     (symbol? (car x))
								     (symbol? (cadr x))))
							      (cdr x)))))
      
      (define (class/d* super interfaces init-args clauses . def/exps)
	(apply class/d*/names '(this super-init) super interfaces init-args clauses def/exps))

      (define (class/d super init-args clauses . def/exps)
	(apply class/d* super '() init-args clauses def/exps))

      (define (class/d*/names local-names super interfaces init-args clauses . def/exps)

	(unless (and (list? local-names)
		     (= 2 (length local-names))
		     (andmap symbol? local-names))
	  (raise-syntax-error 'class/d*/names "local names should be two variables" local-names))

	(unless (list? interfaces)
	  (raise-syntax-error 'class/d*/names "expected interfaces" interfaces))

	(validate-clauses clauses)


	(let ([class/d-super (gensym "class/d-super")]
	      [public-vars (extract-public-vars clauses)]
	      [overriden-vars (extract-overriden-vars clauses)]
	      [inherited-vars (extract-inherited-vars clauses)]
	      [renamed-vars (extract-renamed-vars clauses)])
	  (let-values ([(expanded-def/exps types)
			(let loop ([def/exps def/exps])
			  (cond
			   [(null? def/exps) (values null null)]
			   [else
			    (let-values ([(expanded-def/exp type) (local-expand-body-expression (car def/exps))]
					 [(def/exps types) (loop (cdr def/exps))])
			      (values (cons expanded-def/exp def/exps)
				      (cons type types)))]))])
	    (let* ([defined-vars (apply append (map (lambda (def/exp type)
						      (if (eq? type '#%define-values)
							  (cadr def/exp)
							  null))
						    expanded-def/exps types))]
		   [private-vars
		    (filter (lambda (x) (not (or (member x public-vars)
						 (member x overriden-vars))))
			    defined-vars)])

	      (for-each (lambda (pub-var)
			  (unless (member pub-var defined-vars)
			    (raise-syntax-error 'class/d (format "public var ~a not defined" pub-var))))
			public-vars)
	      (for-each (lambda (over-var)
			  (unless (member over-var defined-vars)
			    (raise-syntax-error 'class/d (format "overriden var ~a not defined" over-var))))
			overriden-vars)
	      (let ([clausess
		     (map (lambda (expanded-def/exp type)
			    (case type
			      [(#%define-values)
			       (let* ([vars (cadr expanded-def/exp)]
				      [gens (map gensym vars)])
				 (cons
				  `(private ,@gens)
				  (cons
				   `(sequence (set!-values ,gens ,@(cddr expanded-def/exp)))
				   (map (lambda (var gen)
					  (let ([clause-type
						 (cond
						  [(member var public-vars) 'public]
						  [(member var overriden-vars) 'override]
						  [else 'private])])
					    `(,clause-type [,var ,gen])))
					vars
					gens))))]
			      [else (list
				     `(sequence ,expanded-def/exp))]))
			  expanded-def/exps types)])
		
		`(class*/names ,local-names ,super ,interfaces ,init-args
			       (rename ,@renamed-vars)
			       (inherit ,@inherited-vars)

			       ,@(apply append clausess))))))))
