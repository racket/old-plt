
(unit
 (import)
 (export let-enumerate 
	 catch-errors 
	 class-asi
	 class*-asi
	 opt-lambda
	 evcase)
	 
 ;; let form where, instead of a list of name-value pairs, just
 ;; provide a list of names and they are bound to a sequence of
 ;; numbers (starting with 1)
 (define let-enumerate 
  (lambda (name-lists . body)
    (cons 'let
	  (cons
	   (let enum-all ((lists name-lists))
	     (if (null? lists)
		 '()
		 (append (let enum ((names (car lists)) (pos 1))
			   (if (null? names)
			       '()
			       (cons (list (car names) pos)
				     (enum (cdr names) (+ pos 1)))))
			 (enum-all (cdr lists)))))
	   body))))

 (define catch-errors 
  (lambda (displayer failure . body)
    (let ([orig-displayer (gensym)]
	  [orig-escaper (gensym)]
	  [err (gensym)]
	  [my-error (gensym)]
	  [this-displayer (gensym)])
      `(let* ([,orig-displayer (error-display-handler)]
	      [,orig-escaper (error-escape-handler)]
	      [,this-displayer ,displayer])
	 (dynamic-wind
	  (lambda () #f)
	  (lambda ()
	    (let/ec ,err
		   (if ,this-displayer
		       (error-display-handler ,this-displayer))
		   (error-escape-handler
		    (lambda () (,err (,failure))))
		   ,@body))
	  (lambda () 
	    (error-display-handler ,orig-displayer)
	    (error-escape-handler ,orig-escaper)))))))

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
	 ,f))))

 (define evcase 
   (lambda (v . tests-in)
     (let ([serror
	    (lambda (msg at)
	      (raise-syntax-error
	       'evcase
	       msg
	       (list* 'evcase v tests-in)
	       at))])
       (let ([gen (gensym "evcase")])
	 `(let ([,gen ,v])
	    (cond
	     ,@(let loop ([tests tests-in])
		 (cond
		  [(null? tests) `()]
		  [(pair? tests)
		   (let ([test-value (car tests)]
			 [rest (cdr tests)])
		     `(,(if (or (not (pair? test-value))
				(not (pair? (cdr test-value))))
			    (serror
			     "bad syntax (clause is not a test-value pair)"
			     test-value)
			    (let ([test (car test-value)]
				  [body (cdr test-value)])
			      (if (and (pair? body) (list? body))
				  #t
				  (serror
				   "bad syntax (improper clause body)"
				   body))
			      (let ([condition 
				     (cond
				      [(and (eq? test 'else)
					    (not (local-expansion-time-bound? 'else)))
				       (if (null? rest)
					   'else
					   (serror
					    "bad syntax (`else' clause must be last)"
					    test-value))]
				      [else `(eqv? ,gen ,test)])])
				`(,condition
				  (begin ,@body)))))
		       .
		       ,(loop rest)))]
		  [else (serror
			 "bad syntax (body must contain a list of pairs)"
			 tests)])))))))))
