(plt:require-library "ricedefs.ss")

(define ricedefs@
  (unit/sig ricedefs^
    (import)
    
    (define allow-improper-lists (make-parameter #t))

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
	(if (allow-improper-lists)
	    prim
	    (lambda (a b)
	      (if (list? b)
		  (prim a b)
		  (error prim-name
			 "second argument must be of type <proper list>, given ~a and ~a"
			 a b))))))

    (define cons (make-second-checked #%cons 'cons))
    (define set-cdr! (make-second-checked #%set-cdr! 'set-cdr!))
    (define list* (make-last-checked #%list* 'list*))
    (define append (make-last-checked #%append 'append))
    (define append! (make-last-checked #%append! 'append!))))
