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
			  (printf "trace:: ~a~n" (list* 'send this ',method args))
			  (apply ,(super method) args))])
		    methods)))))))

(define-macro mixin
  (lambda (from to args . clauses)
    (unless (and (list? from) (list? to))
      (raise-syntax-error
       'mixin
       "expected a list of interfaces for from and to positions, got: ~a and ~a~n" from to))

    (let ([from-g (gensym "mixin:from")]
	  [to-g (gensym "mixin:to")]
	  [res-g (gensym "mixin:result")]
	  [super-g (gensym "mixin:super%")]
	  [to-gs (map (lambda (x) (gensym "mixin:tos")) to)])

      `(let (,(map (lambda (to-g to-exp) `[,to-g ,to-exp]) to-gs to))

	 (let ([,from-g (list ,@from)]
	       [,to-g (list ,@to-gs)])

	 (unless (and (andmap interface? ,from-g) (andmap interface? ,to-g))
	   (error 'mixin "expected interfaces for from and to, got: ~a ~a~n" ,from-g ,to-g))

	 'SUPER-IVAR-CHECKS-BEGIN-HERE

	 (let ([interface-has?
		(lambda (x)
		  (printf "checking ~a in ~a~n" x ,from-g)
		  (ormap (lambda (i) (ivar-in-interface? x i)) ,from-g))])
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
				  [(pair? input)
				   (let ([fail (lambda () (loop output (cdr input)))]
					 [pair (car input)])
				     (if (pair? pair)
					 (let ([sym (car pair)])
					   (if (symbol? pair)
					       (loop (cons (cadr pair) output)
						     (cdr input))
					       (fail)))
					 (fail)))]
				  [else output]))]
			     [else null])])
		      `(begin
			 ,@(map (lambda (id)
				  `(unless (interface-has? ',id)
				     (error
				      'mixin
				      "ivar ~a not in any of ~a interfaces but was referenced in definition~n")
				     ,id ,from-g))
				names))))
		  clauses))

	 'MIXIN-BEGINS-HERE

	 (lambda (,super-g)
	   (unless (andmap (lambda (x) (implementation? ,super-g x)) ,from-g)
	     (error 'mixin "argument ~a does not match ~a~n" ,super-g ,from-g))

	   (class* ,super-g ,to-gs ,args
	     ,@clauses)))))))
	     