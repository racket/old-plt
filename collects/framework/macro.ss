(require-library "match.ss")

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

      `(let ,(map (lambda (to-g to-exp) `[,to-g ,to-exp]) to-gs to)

	 (let ([,from-g (list ,@from)]
	       [,to-g (list ,@to-gs)])

	 (unless (and (andmap interface? ,from-g) (andmap interface? ,to-g))
	   (error 'mixin "expected interfaces for from and to, got: ~a ~a~n" ,from-g ,to-g))

	 'SUPER-IVAR-CHECKS-BEGIN-HERE

	 (let ([ensure-interface-has?
		(lambda (x)
		  (unless (ormap (lambda (i)
				   ((cond
				      [(interface? i) ivar-in-interface?]
				      [(class? i) ivar-in-class?])
				    x i))
				   ,from-g)
		    (error 'mixin
			   "ivar `~a' not in any of ~a, but was referenced in definition"
			   x ,from-g)))])
	   ,@(map (lambda (clause)
		    (let ([names
			   (match clause
			     [`(inherit ,(and (? symbol?) name) ...) name]
			     [`(rename [,(? symbol?) ,(and (? symbol?) name)] ...) name]
			     [`(override [,(and name (? symbol?)) ,body] ...) name]
			     [else null])])
		      `(begin
			 (void)
			 ,@(map (lambda (id) `(ensure-interface-has? ',id))
				names))))
		  clauses))

	 'MIXIN-BEGINS-HERE

	 (lambda (,super-g)
	   (unless (class? ,super-g)
	     (error 'mixin "argument ~a not a class matching interfaces: ~a" ,super-g ,from-g))
	   (unless (andmap (lambda (x) ((cond
					  [(interface? x) implementation?]
					  [(class? x) subclass?])
					,super-g x))
			   ,from-g)
	     (error 'mixin "argument ~s does not match ~s" ,super-g ,from-g))

	   (class* ,super-g ,to-gs ,args
	     ,@clauses)))))))

(define-macro dunit/sig
  (let ([debugging? #f])
    (if debugging?
	(begin
	  (require-library "loader.ss" "system")
	  (lambda (sig imports . args)
	    (let-values ([(before-args args)
			  (if (and (pair? args)
				   (pair? (car args))
				   (eq? (caar args) 'rename))
			      (values (list (car args))
				      (cdr args))
			      (values null args))])
	      
	      `(unit/sig ,sig ,imports
			 ,@(append before-args
				   (list `(fprintf (current-error-port) "invoking unit with ~a signature~n" ',sig))
				   args
				   (list `(fprintf (current-error-port) "invoked  unit with ~a signature~n" ',sig)))))))
	(lambda args
	  `(unit/sig ,@args)))))
  