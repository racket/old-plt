(module spec/type mzscheme
  (provide provide/type require/type)
  (require-for-syntax mzscheme
		      (lib "stx.ss" "syntax"))
  
  (define (raise-error module-name fmt . args)
    (error 'provide/type
	   (string-append
	    (format "module ~e: " module-name)
	    (apply format fmt args))))

  (define-struct wrap (defn type))

  (define-syntax wrap
    (lambda (stx)
      (syntax-case stx ()
	[(_ type name stx-pos? module-name)
	 (let ([pos? (syntax-object->datum (syntax stx-pos?))])
	   (syntax-case (syntax type) (-> number union boolean interface)
	     [(dom -> rng)
	      (with-syntax ([stx-n-pos? (not (syntax-object->datum (syntax stx-pos?)))])
		(if pos?
		    (syntax
		     (if (procedure? name)
			 (lambda (in)
			   (let ([out (name (wrap dom in stx-n-pos? module-name))])
			     (wrap rng out stx-pos? module-name)))
			 (raise-error
			  (quote module-name)
			  "expected a procedure, got: ~e" name)))
		    (syntax
		     (lambda (in)
		       (let ([out (name (wrap dom in stx-n-pos? module-name))])
			 (wrap rng out stx-pos? module-name))))))]
	     [(interface i-e)
	      (if pos?
		  (syntax
		   (let ([interface i-e])
		     (if (is-a? name interface)
			 name
			 (raise-error
			  (quote module-name)
			  "expected an instance of ~e, got: ~e" name interface))))
		  (syntax name))]
	     [number
	      (if pos?
		  (syntax
		   (if (number? name)
		       name
		       (raise-error
			(quote module-name)
			"expected a number, got: ~e" name)))
		  (syntax name))]
	     [boolean
	      (if pos?
		  (syntax
		   (if (boolean? name)
		       name
		       (raise-error
			(quote module-name)
			"expected a boolean, got: ~e" name)))
		  (syntax name))]))])))

  (define-syntax provide/type
    (lambda (stx)
      (syntax-case stx ()
	[(_ module-name internal-name external-name type)
	 (with-syntax ([module-name (syntax-source stx)])
	   (syntax
	    (begin
	      (define external-name
		(make-wrap
		 (wrap type internal-name #t module-name)
		 (quote type)))
	      (provide external-name))))])))

  (define-syntax require/type
    (lambda (stx)
      (syntax-case stx ()
	[(_ orig-name wrap-name type)
	 (with-syntax ([module-name (syntax-source stx)])
	   (syntax
	    (define wrap-name
	      (if (equal? (quote type) (wrap-type orig-name))
		  (wrap type (wrap-defn orig-name) #f module-name)
		  (error 'require/type "expected types to match, but they don't: ~s ~s"
			 (quote type) (wrap-type orig-name))))))]))))