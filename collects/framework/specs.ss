(module spec/type mzscheme
  (provide provide/type)
  (require-for-syntax (lib "stx.ss" "syntax"))
  
  (define (raise-error module-name fmt . args)
    (error 'provide/type
	   (string-append
	    (format "module ~e: " module-name)
	    (apply format fmt args))))

  (define (wrap type name pos?)
    (syntax-case type (-> number union boolean interface)
      [(dom -> rng)
       (with-syntax ([in (gensym 'in)]
		     [out (gensym 'out)])
	 (with-syntax ([a-checker (wrap (syntax dom) (syntax in) (not pos?))]
		       [b-checker (wrap (syntax rng) (syntax out) pos?)]
		       [name name])
	   (if pos?
	       (syntax
		(if (procedure? name)
		    (lambda (in)
		      (let ([out (name a-checker)])
			b-checker))
		    (raise-error
		     (quote module-name)
		     "expected a procedure, got: ~e" name)))
	       (syntax
		(lambda (in)
		  (let ([out (name a-checker)])
		    b-checker))))))]
      [(interface i-e)
       (if pos?
	   (syntax
	    (let ([interface i-e])
	      (if (is-a? name interface)
		  name
		  (raise-error
		   (quote module-name)
		   "expected an instance of ~e, got: ~e" name interface))))
	   name)]
      [number
       (with-syntax ([name name])
	 (if pos?
	     (syntax
	      (if (number? name)
		  name
		  (raise-error
		   (quote module-name)
		   "expected a number, got: ~e" name)))
	     name))]
      [boolean
       (if name?
	   (with-syntax ([name name])
	     (syntax
	      (if (boolean? name)
		  name
		  (raise-error
		   (quote module-name)
		   "expected a boolean, got: ~e" name))))
	   name)]))

  (define-syntax provide/type
    (lambda (stx)
      (syntax-case stx ()
	[(_ module-name internal-name external-name type)
	 (with-syntax ([new-defn (wrap (syntax type) (syntax internal-name) #t)])
	   (syntax
	    (begin
	      (require provide/type-struct)
	      (define external-name (make-hidden new-defn (quote type)))
	      (provide external-name))))])))

  (define (type-equal? in out)
    (equal? in out))

  (define-syntax require/type
    (lambda (stx)
      (syntax-case stx ()
	[(_ module-name name type)
	 (syntax
	  (begin
	    (require spec/type-struct)
	    (require name)
	    (define name new-defn))))]))))

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

(require c)