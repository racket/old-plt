
(module stxparam mzscheme
  (require-for-syntax "private/stxparamkey.ss")

  (provide define-syntax-parameter
	   syntax-parameterize)

  (define-syntax (define-syntax-parameter stx)
    (syntax-case stx ()
      [(_ id init-val)
       (with-syntax ([gen-id (car (generate-temporaries (list #'id)))])
	 #'(begin
	     (define-syntax gen-id init-val)
	     (define-syntax id 
	       (make-set!-transformer
		(make-syntax-parameter 
		 (lambda (stx)
		   (let ([v (syntax-parameter-target-value #'gen-id)])
		     (cond
		      [(set!-transformer? v) ((set!-transformer-procedure v) stx)]
		      [(and (procedure? v)
			    (procedure-arity-includes? v 1))
		       (syntax-case stx (set!)
			 [(set! id _) (raise-syntax-error
				       #f
				       "cannot mutate syntax identifier"
				       stx
				       #'id)]
			 [else (v stx)])]
		      [else
		       (raise-syntax-error
			#f
			"bad syntax"
			stx
			#f)])))
		 #'gen-id)))))]))

  (define-syntax (syntax-parameterize stx)
    (syntax-case stx ()
      [(_ ([id val] ...) body0 body ...)
       (let ([ids (syntax->list #'(id ...))])
	 (with-syntax ([(gen-id ...)
			(map (lambda (id)
			       (unless (identifier? id)
				 (raise-syntax-error
				  #f
				  "not an identifier"
				  stx
				  id))
			       (let* ([rt (syntax-local-value id (lambda () #f))]
				      [sp (if (set!-transformer? rt)
					      (set!-transformer-procedure rt)
					      rt)])
				 (unless (syntax-parameter? sp)
				   (raise-syntax-error
				    #f
				    "not bound as a syntax parameter"
				    stx
				    id))
				 (syntax-local-get-shadower 
				  (syntax-local-introduce (syntax-parameter-target sp)))))
			     ids)])
	   (let ([dup (check-duplicate-identifier ids)])
	     (when dup
	       (raise-syntax-error
		#f
		"duplicate binding"
		stx
		dup)))
	   #'(let-syntax ([gen-id val] ...)
	       body0 body ...)))]))

  ;; ----------------------------------------

  (require "private/stxparamkey.ss")
  (provide syntax-parameter-value
	   make-parameter-rename-transformer)

  (define (make-parameter-rename-transformer id)
    (make-set!-transformer
     (lambda (stx)
       (let ([v (syntax-parameter-value (syntax-local-introduce id))])
	 (cond
	  [(set!-transformer? v) ((set!-transformer-procedure v) stx)]
	  [(and (procedure? v)
		(procedure-arity-includes? v 1))
	   (syntax-case stx (set!)
	     [(set! id _) (raise-syntax-error
			  #f
			  "cannot mutate syntax identifier"
			  stx
			  #'id)]
	     [else (v stx)])]
	  [else
	   (raise-syntax-error
	    #f
	    "bad syntax"
	    stx
	    #f)])))))

  (define (syntax-parameter-value id)
    (let* ([v (syntax-local-value id (lambda () #f))]
	   [v (if (set!-transformer? v)
		  (set!-transformer-procedure v)
		  v)])
      (unless (syntax-parameter? v)
	(raise-type-error 'syntax-parameter-value "syntax parameter" v))
      (let ([target (syntax-parameter-target v)])
	(syntax-parameter-target-value target)))))
