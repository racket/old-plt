(unit/sig ricedefs^
  (import [params : plt:userspace:params^])

  (define boolean=?
    (lambda (x y)
      (unless (boolean? x)
	(error 'boolean=? "expected boolean arguments, received ~e ~e"
	       x y))
      (#%eq? x y)))
  
  (define eq?
    (if (params:eq?-only-compares-symbols)
	(lambda (x y)
	  (unless (and (symbol? x)
		       (symbol? y))
	    (error 'eq? "expected symbols as arguments, received ~e and ~e" x y))
	  (#%eq? x y))
	(lambda (x y)
	  (#%eq? x y))))
  
  (define check-second 
    (lambda (prim-name a b)
      (unless (list? b)
	(error prim-name
	       "second argument must be of type <list>, given ~e and ~e"
	       a b))))
  
  (define check-last
    (lambda (prim-name args)
      (let loop ([l args])
	(cond
	 [(null? l) (void)]
	 [(null? (cdr l))
	  (let ([last (car l)])
	    (unless (list? last)
	      (error prim-name
		     "last argument must be of type <list>, given ~e; all args: ~a"
		     last
		     (map (lambda (x) (format "~e" x)) args))))]
	 [else (loop (cdr l))]))))

  (define =
    (if (params:<=-at-least-two-args)
	(lambda (x y . args)
	  (apply #%= x y args))
	#%=))

  (define -
    (if (params:<=-at-least-two-args)
	(lambda (x y . args)
	  (apply #%- x y args))
	#%-))

  (define +
    (if (params:<=-at-least-two-args)
	(lambda (x y . args)
	  (apply #%+ x y args))
	#%+))

  (define /
    (if (params:<=-at-least-two-args)
	(lambda (x y . args)
	  (apply #%/ x y args))
	#%/))

  (define *
    (if (params:<=-at-least-two-args)
	(lambda (x y . args)
	  (apply #%* x y args))
	#%*))

  (define >=
    (if (params:<=-at-least-two-args)
	(lambda (x y . args)
	  (apply #%>= x y args))
	#%>=))

  (define <
    (if (params:<=-at-least-two-args)
	(lambda (x y . args)
	  (apply #%< x  y args))
	#%<))

  (define >
    (if (params:<=-at-least-two-args)
	(lambda (x y . args)
	  (apply #%> x y args))
	#%>))

  (define <=
    (if (params:<=-at-least-two-args)
	(lambda (x y . args)
	  (apply #%<= x y args))
	#%<=))

  (define cons (if (params:allow-improper-lists)
		   #%cons
		   (lambda (a b)
		     (check-second 'cons a b)
		     (#%cons a b))))
  
  (define set-cdr! (if (params:allow-improper-lists)
		       #%set-cdr!
		       (lambda (a b)
			 (check-second 'set-cdr! a b)
			 (#%set-cdr! a b))))
  
  (define list* (if (params:allow-improper-lists)
		    #%list*
		    (lambda x
		      (check-last 'list* x)
		      (apply #%list* x))))
  
  (define append (if (params:allow-improper-lists)
		     #%append
		     (lambda x
		       (check-last 'append x)
		       (apply #%append x))))
  
  (define append! (if (params:allow-improper-lists)
		      #%append!
		      (lambda x
			(check-last 'append! x)
			(apply #%append! x)))))
