(require-library "functio.ss")

(define (l) (load "gen-tests2.ss"))

(define *max-funs* 1600)
(define *max-args* 1)
(define *counter* 0)
(define *arg* 0)

(define *arity-table*
  (make-hash-table))

(define (go)
  (gen-app '() 0))

(define (fun-name n)
  (string->symbol 
   (string-append "fun-" (number->string n))))

(define (create-fun)
  (let* ([numargs 1] ;[numargs (random *max-args*)]
	 [formals (build-list numargs
			      (lambda _ (gensym)))]
	 [prev-fun (fun-name *counter*)]
	 [_ (set! *counter* (add1 *counter*))]
	 [curr-fun (fun-name *counter*)]
	 [_ (hash-table-put! *arity-table* curr-fun numargs)]
	 [prev-arity (hash-table-get *arity-table* prev-fun)]
	 [prev-args
	  (let loop ([argnum 0]
		     [formals formals])
	    (if (>= argnum prev-arity)
		'()
		(if (null? formals)
		    (begin
		     (set! *arg* (add1 *arg*))
		     (cons *arg* 
			   (loop (add1 argnum)
				 formals)))
		    (cons (car formals)
			  (loop (add1 argnum)
				(cdr formals))))))])
     `(define ,curr-fun
	(lambda ,formals
	  (,prev-fun ,@prev-args)))))

(define (go)
  (hash-table-put! *arity-table* (fun-name *counter*) 1) ; 5)

  (printf "~a~n" 
	  `(define ,(fun-name *counter*)
	     ; (lambda (a b c d e) 42)))
	     (lambda (a) 42)))
		      
  (let loop 
      ([n *max-funs*])
    (unless (<= n 0)
	    (printf "~a~n~n" (create-fun))
	    (loop (sub1 n))))
  (let ([numargs (hash-table-get *arity-table* 
				 (fun-name *counter*))])
  `(,(fun-name *counter*)
    ,@(build-list numargs (lambda (x) x)))))

    



      
    