
(reference-library "letplus.ss")
(reference-library "macrox.ss")

; let form where, instead of a list of name-value pairs, just
; provide a list of names and they are bound to a sequence of
; numbers (starting with 1)
(define-macro let-enumerate 
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

(define-macro catch-errors 
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

(define-macro class*-asi
  (lambda (super interfaces . body)
    (let ([args (gensym)]
	  [super-init 'super-init])
      `(class* ,super ,interfaces ,args
	  ,@body
	  (sequence
	    (apply ,super-init ,args))))))

(define-macro class-asi
  (lambda (super . rest)
    `(class*-asi ,super () ,@rest)))

(define-macro opt-lambda 
  (lambda (args . body)
    (let* ([mk-code (lambda () (list* 'opt-lambda args body))]
	   [f (gensym)]
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


