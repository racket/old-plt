;(plt:require-library "ricedefs.ss")

  (unit/sig ricedefs^
    (import)
    
    (define allow-improper-lists (make-parameter #t))
    (define eq?-only-compares-symbols? (make-parameter #f))

    (define boolean=?
      (lambda (x y)
	(unless (boolean? x)
	  (error 'boolean=? "expected boolean arguments, received ~a ~a"
		 x y))
	(if x
	    y
	    (not y))))

    (define eq?
      (lambda (x y)
	(when (and (eq?-only-compares-symbols?)
		   (not (and (symbol? x)
			     (symbol? y))))
	  (error 'eq? "expected symbols as arguments, received ~a ~a" x y))
	(#%eq? x y)))

    (define last
      (lambda (l)
        (cond
          [(null? l) (error 'last "received an empty list")]
          [(null? (cdr l)) (car l)]
          [else (last (cdr l))])))

    (define make-last-checked
      (lambda (prim prim-name)
	(if (allow-improper-lists)
	    prim
	    (case-lambda
	     [() (prim)]
	     [args (let ([l (last args)])
		     (if (list? l)
			 (apply prim args)
			 (error prim-name
				"last argument must be of type <proper list>, given ~a; all args: ~a"
				l
				args)))]))))

    (define make-second-checked 
      (lambda (prim prim-name)
	(lambda (a b)
	  (if (allow-improper-lists)
	      (prim a b)
	      (if (list? b)
		  (prim a b)
		  (error prim-name
			 "second argument must be of type <proper list>, given ~a and ~a"
			 a b))))))

    (define cons (make-second-checked #%cons 'cons))
    (define set-cdr! (make-second-checked #%set-cdr! 'set-cdr!))
    (define list* (make-last-checked #%list* 'list*))
    (define append (make-last-checked #%append 'append))
    (define append! (make-last-checked #%append! 'append!)))
