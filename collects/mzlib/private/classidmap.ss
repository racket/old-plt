
(module classidmap mzscheme

  (require (lib "stx.ss" "syntax"))

  (define (make-method-apply id this orig-args)
    (let loop ([args orig-args][accum null])
      (cond
       [(stx-null? args)
	(list* id this orig-args)]
       [(stx-pair? args)
	(loop (stx-cdr args) (cons (stx-car args) accum))]
       [else
	(list* 'apply id this (reverse (cons args accum)))])))

  (define (make-field-map this-id field-accessor field-mutator)
    (let ([set!-stx (datum->syntax-object this-id 'set!)])
      (make-set!-transformer
       (lambda (stx)
	 (syntax-case stx ()
	   [(set! id expr)
	    (module-identifier=? (syntax set!) set!-stx)
	    (datum->syntax-object 
	     this-id 
	     (list field-mutator this-id (syntax expr))
	     stx)]
	   [(id . args)
	    (datum->syntax-object 
	     this-id 
	     (cons (list field-accessor this-id) 
		   (syntax args))
	     stx)]
	   [_else
	    (datum->syntax-object 
	     this-id 
	     (list field-accessor this-id)
	     stx)])))))

  (define (make-method-map this-id method-accessor)
    (let ([set!-stx (datum->syntax-object this-id 'set!)])
      (make-set!-transformer
       (lambda (stx)
	 (syntax-case stx ()
	   [(set! id expr)
	    (module-identifier=? (syntax set!) set!-stx)
	    (raise-syntax-error 'class "cannot mutate method" stx)]
	   [(id . args)
	    (datum->syntax-object 
	     this-id 
	     (make-method-apply
	      (list method-accessor this-id) 
	      this-id
	      (syntax args))
	     stx)]
	   [_else
	    (raise-syntax-error 
	     'class 
	     "misuse of method (not in application)" 
	     stx)])))))

  ;; For methods that are dirrectly available via their names
  ;;  (e.g., private methods)
  (define (make-direct-method-map this-id new-name)
    (let ([set!-stx (datum->syntax-object this-id 'set!)])
      (make-set!-transformer
       (lambda (stx)
	 (syntax-case stx ()
	   [(set! id expr)
	    (module-identifier=? (syntax set!) set!-stx)
	    (raise-syntax-error 'class "cannot mutate method" stx)]
	   [(id . args)
	    (datum->syntax-object 
	     this-id 
	     (make-method-apply
	      new-name
	      this-id
	      (syntax args))
	     stx)]
	   [_else
	    (raise-syntax-error 
	     'class 
	     "misuse of method (not in application)" 
	     stx)])))))

  (define (make-rename-map rename-temp this-id)
    (let ([set!-stx (datum->syntax-object rename-temp 'set!)])
      (make-set!-transformer
       (lambda (stx)
	 (syntax-case stx ()
	   [(set! id expr)
	    (module-identifier=? (syntax set!) set!-stx)
	    (raise-syntax-error 'class "cannot mutate super method" stx)]
	   [(id . args)
	    (datum->syntax-object 
	     this-id
	     (make-method-apply rename-temp this-id (syntax args))
	     stx)]
	   [_else
	    (raise-syntax-error 
	     'class 
	     "misuse of super method (not in application)" 
	     stx)])))))

  (define init-error-map
    (make-set!-transformer
     (lambda (stx)
       (raise-syntax-error 
	'class
	"cannot use non-field init variable in a method"
	stx))))

  (define super-error-map
    (make-set!-transformer
     (lambda (stx)
       (raise-syntax-error 
	'class
	"cannot use superclass initialization form in a method"
	stx))))

  (define (flatten-args orig-args)
    (let loop ([args orig-args][accum null])
      (cond
       [(stx-null? args) orig-args]
       [(stx-pair? args)
	(loop (stx-cdr args) (cons (stx-car args) accum))]
       [else
	(reverse (cons args accum))])))

  (provide make-field-map make-method-map 
	   make-direct-method-map make-rename-map
	   init-error-map super-error-map flatten-args))

    
