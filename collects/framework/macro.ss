(define-macro trace-methods 
  (lambda methods
    (let ([super 
	   (lambda (method)
	     (string->symbol 
	      (string-append "super-"
			     (symbol->string method))))])
      `(lambda (%)
	 (class-asi %
	   (rename ,@(map (lambda (method)
			    `(,(super method) ,method))
			  methods))
	   (public
	     ,@(map (lambda (method)
		      `[,method
			(lambda args
			  (fprintf mred:constants:original-output-port
				   "trace:: ~a~n" (list* 'send this ',method args))
			  (apply ,(super method) args))])
		    methods)))))))

(define-macro mixin
  (lambda (from to args . clauses)
    (let ([from-g (gensym "mixin:from")]
	  [to-g (gensym "mixin:to")]
	  [res-g (gensym "mixin:result")]
	  [super-g (gensym "mixin:super%")])
      (unless (and (list? from) (list? to))
	(raise-syntax-error
	 'mixin
	 "expected a list of interfaces for from and to positions, got: ~a and ~a~n" from to))
      `(let ([,from-g (list ,@from)]
	     [,to-g (list ,@to)])
	 (unless (andmap interface? (append ,from-g ,to-g))
	   (error 'mixin "expected interfaces for from and to, got: ~a ~a~n" ,from-g ,to-g))

	 (let ([interface/class-has?
		(lambda (x)
		  (printf "checking ~a in ~a~n" x ,from-g)
		  (ormap (lambda (i) (ivar-in-interface? x i)) ,from-g))]
	   ,@(map (lambda (clause)
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
			 ,@(map (lambda (id)
				  `(unless (interface-has? ',id)
				     (error 'mixin "ivar ~a not in any of ~a interfaces but was referenced in definition~n")
				     ,id ,from-g))
				names))))
		  clauses))

	 (lambda (,super-g)
	   (unless (andmap (lambda (x) (implementation? ,super-g x)) ,from-g)
	     (error 'mixin "argument ~a does not match ~a~n" ,super-g ,from-g))

	   (class* ,super-g ,to-g ,args
	     ,@clauses))))))
	     