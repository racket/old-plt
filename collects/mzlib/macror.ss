
(unit
 (import)
 (export class-asi
	 class*-asi
	 opt-lambda)
	 
 (define class*-asi
  (lambda (super interfaces . body)
    (let ([args (gensym)]
	  [super-init 'super-init])
      `(class* ,super ,interfaces ,args
	  ,@body
	  (sequence
	    (apply ,super-init ,args))))))

 (define class-asi
  (lambda (super . rest)
    `(class*-asi ,super () ,@rest)))

 (define opt-lambda 
  (lambda (args . body)
    (let* ([mk-code (lambda () (list* 'opt-lambda args body))]
	   [f (gensym 'opt-lambda-procedure)]
	   [required
	    (let loop ([args args])
	      (if (and (pair? args)
		       (symbol? (car args)))
		  (cons (car args) (loop (cdr args)))
		  '()))]
	   [not-required-with-defaults
	    (let loop ([args args])
	      (if (and (pair? args)
		       (symbol? (car args)))
		  (loop (cdr args))
		  args))]
	   [not-required
	    (let loop ([args not-required-with-defaults])
	      (if (pair? args)
		  (if (pair? (car args))
		      (let ([name (caar args)])
			(if (symbol? name)
			    (cons name (loop (cdr args)))
			    (raise-syntax-error 
			     'opt-lambda
			     "all argument names must be symbols"
			     (mk-code))))
		      (raise-syntax-error 'opt-lambda
					  "all required args must come first"
					  (mk-code)))
		  (if (or (null? args) (symbol? args))
		      args
		      (raise-syntax-error 'opt-lambda 
					  "all argument names must be symbols"
					  (mk-code)))))]
	   [defaults
	     (let loop ([args not-required-with-defaults])
	       (if (pair? args)
		   (let ([v (cdar args)])
		     (if (and (pair? v) (null? (cdr v)))
			 (cons (car v) (loop (cdr args)))
			 (raise-syntax-error 
			  'opt-lambda
			  "only one default value allowed per argument"
			  (mk-code))))
		   ()))])
      `(letrec ([,f
		 (case-lambda
		  ,@(let loop ([required required]
			       [not-required not-required]
			       [defaults defaults])
		      (if (not (pair? not-required))
			  (list `(,(append required not-required) ,@body))
			  (cons `(,required
				  ,(cons f (append required 
						   (list (car defaults)))))
				(loop (append required (list (car not-required)))
				      (cdr not-required)
				      (cdr defaults))))))])
	 ,f)))))
