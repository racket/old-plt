(module spec/type mzscheme
  (provide provide/type)
  (require-for-syntax mzscheme
		      (lib "stx.ss" "syntax")
		      "private/specs-helpers.ss")
  
  (define (raise-error module-name fmt . args)
    (error 'provide/type
	   (string-append
	    (format "module ~e: " module-name)
	    (apply format fmt args))))

  (define-struct wrap (defn type))

  (define-syntax provide/type
    (lambda (stx)
      (syntax-case stx ()
	[(_ module-name internal-name external-name type)
	 (with-syntax ([new-defn (build-wrapping
				  (syntax type)
				  (syntax internal-name)
				  #t
				  stx)])
	   (syntax
	    (begin
	      (define external-name (make-wrap new-defn (quote type)))
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
	    (define name new-defn)))]))))

(module a mzscheme
  (require spec/type)

  (define (in-f g) (g 0))
  (provide/type a in-f f ((number -> number) -> number)))

(module b mzscheme
  (require spec/type)

  (define (in-g x) (+ x 1))
  (provide/type b in-g g (number -> boolean)))

(module c mzscheme
  (require a b)

  (display (f g)) (newline))

(require c)