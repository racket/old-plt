
(module classidmap mzscheme

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
	     (cons (list method-accessor this-id) 
		   (syntax args))
	     stx)]
	   [_else
	    (raise-syntax-error 
	     'class 
	     "misuse of method (not in application)" 
	     stx)])))))

  (define (make-rename-map rename-temp)
    (let ([set!-stx (datum->syntax-object rename-temp 'set!)])
      (make-set!-transformer
       (lambda (stx)
	 (syntax-case stx ()
	   [(set! id expr)
	    (module-identifier=? (syntax set!) set!-stx)
	    (raise-syntax-error 'class "cannot mutate super method" stx)]
	   [(id . args)
	    (datum->syntax-object 
	     rename-temp
	     (cons rename-temp (syntax args))
	     stx)]
	   [_else
	    (raise-syntax-error 
	     'class 
	     "misuse of super method (not in application)" 
	     stx)])))))

  (provide make-field-map make-method-map make-rename-map))

    
