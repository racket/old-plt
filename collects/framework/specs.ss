(module tmp mzscheme
  (provide m)
  (define-syntax m
    (lambda (stx)
      (syntax-case stx ()
	[(_)
	 (let ([x 1])
	   (syntax x))]))))

(module tmp2 mzscheme
  (require tmp)
  (m))

(exit)

(module provide/type mzscheme
  (provide provide/type)

  (require-for-syntax (lib "stx.ss" "syntax"))

  (define-syntax provide/type
    (lambda (stx)
      (syntax-case stx ()
	[(_ module-name internal-name external-name type)
	 (let ([raise-error
		(lambda (fmt . args)
		  (error 'provide/type
			 (string-append
			  (format "~e" (syntax->datum (syntax module-name)))
			  (apply format fmt args))))])
	   (with-syntax ([new-defn
			  (let loop ([type (syntax type)]
				     [name (syntax internal-name)])
			    (syntax-case type (-> number union boolean interface)
			      [(dom -> rng)
			        (with-syntax ([in (gensym 'in)]
					      [out (gensym 'out)])
				  (with-syntax ([a-checker (loop (syntax dom)
								 (syntax in))]
						[b-checker (loop (syntax rng)
								 (syntax out))]
						[name name])
				    (syntax
				     (if (procedure? name)
					 (lambda (in)
					   (let ([out (name a-checker)])
					     b-checker))
					 (raise-error "expected a procedure, got: ~e" name)))))]
			      [(interface i-e)
			       (syntax
				(let ([interface i-e])
				  (if (is-a? name interface)
				      name
				      (raise-error "expected an instance of ~e, got: ~e" name interface))))]
			      [number
			       (with-syntax ([name name])
				 (syntax
				  (if (number? name)
				      name
				      (raise-error "expected a number, got: ~e" name))))]
			      [boolean
			       (with-syntax ([name name])
				 (syntax
				  (if (boolean? name)
				      name
				      (raise-error "expected a boolean, got: ~e" name))))]))])
	     (syntax
	      (begin (define external-name new-defn)
		     (provide external-name)))))]))))

(module a mzscheme
  (require provide/type)

  (define (in-f g) (g 0))
  (provide/type a in-f f ((number -> number) -> number)))

(module b mzscheme
  (require provide/type)

  (define (in-g x) (+ x 1))
  (provide/type b in-g g (number -> boolean)))

(module c mzscheme
  (require a b)

  (display (f g)) (newline))

(require (lib "pretty.ss"))

(pretty-print
 (syntax->datum
  (expand
   (syntax
     (module a mzscheme
       (require tmp)

       (define (in-f g)
	 (display g) (newline)
	 (g 0))
       (provide/type in-f f ((number -> number) -> number)))))))
