
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

  (define (make-this-map the-obj)
    (let ([set!-stx (datum->syntax-object the-obj 'set!)])
      (make-set!-transformer
       (lambda (stx)
	 (syntax-case stx ()
	   [(set! id expr)
	    (module-identifier=? (syntax set!) set!-stx)
	    (raise-syntax-error 'class "cannot mutate object identifier" stx)]
	   [(id . args)
	    (datum->syntax-object 
	     stx
	     (cons the-obj (syntax args))
	     stx)]
	   [id the-obj])))))

  (define (make-field-map the-obj field-accessor field-mutator)
    (let ([set!-stx (datum->syntax-object the-obj 'set!)])
      (make-set!-transformer
       (lambda (stx)
	 (syntax-case stx ()
	   [(set! id expr)
	    (module-identifier=? (syntax set!) set!-stx)
	    (datum->syntax-object 
	     the-obj
	     (list field-mutator the-obj (syntax expr))
	     stx)]
	   [(id . args)
	    (datum->syntax-object 
	     the-obj
	     (cons (list field-accessor the-obj) (syntax args))
	     stx)]
	   [_else
	    (datum->syntax-object 
	     the-obj
	     (list field-accessor the-obj)
	     stx)])))))

  (define (make-method-map the-obj method-accessor)
    (let ([set!-stx (datum->syntax-object the-obj 'set!)])
      (make-set!-transformer
       (lambda (stx)
	 (syntax-case stx ()
	   [(set! id expr)
	    (module-identifier=? (syntax set!) set!-stx)
	    (raise-syntax-error 'class "cannot mutate method" stx)]
	   [(id . args)
	    (datum->syntax-object 
	     the-obj
	     (make-method-apply
	      (list method-accessor the-obj)
	      the-obj
	      (syntax args))
	     stx)]
	   [_else
	    (raise-syntax-error 
	     'class 
	     "misuse of method (not in application)" 
	     stx)])))))

  ;; For methods that are dirrectly available via their names
  ;;  (e.g., private methods)
  (define (make-direct-method-map the-obj new-name)
    (let ([set!-stx (datum->syntax-object the-obj 'set!)])
      (make-set!-transformer
       (lambda (stx)
	 (syntax-case stx ()
	   [(set! id expr)
	    (module-identifier=? (syntax set!) set!-stx)
	    (raise-syntax-error 'class "cannot mutate method" stx)]
	   [(id . args)
	    (datum->syntax-object 
	     the-obj
	     (make-method-apply new-name the-obj (syntax args))
	     stx)]
	   [_else
	    (raise-syntax-error 
	     'class 
	     "misuse of method (not in application)" 
	     stx)])))))

  (define (make-rename-map the-obj rename-temp)
    (let ([set!-stx (datum->syntax-object the-obj 'set!)])
      (make-set!-transformer
       (lambda (stx)
	 (syntax-case stx ()
	   [(set! id expr)
	    (module-identifier=? (syntax set!) set!-stx)
	    (raise-syntax-error 'class "cannot mutate super method" stx)]
	   [(id . args)
	    (datum->syntax-object 
	     the-obj
	     (make-method-apply rename-temp the-obj (syntax args))
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

  (provide make-this-map make-field-map make-method-map 
	   make-direct-method-map make-rename-map
	   init-error-map super-error-map flatten-args))

    
