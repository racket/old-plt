(define-macro mixin
  (lambda (from to args . clauses)
    (let ([from-g (gensym "mixin:from")]
	  [to-g (gensym "mixin:to")]
	  [res-g (gensym "mixin:result")]
	  [super-g (gensym "mixin:super%")])
      `(let ([,from-g ,from]
	     [,to-g ,to])
	 (unless (or (class? ,from-g)
		     (interface? ,from-g))
	   (error 'mixin "expected interface or class for from type"))
	 (lambda (,super-g)
	   (unless ((cond
		     [(class? ,from-g) subclass?]
		     [(interface? ,from-g) implementation?])
		    ,super-g ,from-g)
	     (error 'mixin "argument ~a does not match ~a~n" ,super-g ,from-g))

	   (let ([interface-has? (lambda (x)
				   (printf "checking ~a in ~a~n" x ,from-g)
				   (ivar-in-interface? x ,from-g))])
	     (begin ,@(map (lambda (clause)
			     (let ([names 
				    (cond
				     [(and (pair? clause)
					   (eq? (car clause) 'inherit))
				      (cdr clause)]
				     [(and (pair? clause)
					   (eq? (car clause) 'rename))
				      (let loop ([output null]
						 [input (cdr clause)])
					(cond
					 [(null? input) output]
					 [(pair? input) (let ([pair (car input)])
							  (if (and (list? pair)
								   (= (length pair) 2)
								   (symbol? (cadr pair)))
							      (loop (cons (cadr pair) output)
								    (cdr input))
							      (loop output (cdr input))))]
					 [else output]))]
				     [else null])])
			       `(begin
				  (void)
				  ,@(map (lambda (id)
					   `(unless (interface-has? ',id)
					      (error 'mixin "ivar ~a not in ~a interface but was referenced in class~n") ,id ,from-g))
					 names))))
			   clauses)))

	   (class* ,super-g (,to-g) ,args
	     ,@clauses))))))
	     