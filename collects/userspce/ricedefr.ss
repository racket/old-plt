(plt:require-library "ricedefs.ss")
(plt:require-library "sparams.ss")

(define ricedefs@
  (unit/sig ricedefs^
    (import [params : plt:parameters^])
    
      ; return random lower case character
    (define random-char
      (lambda ()
	(integer->char (+ 97 (random 26)))))
    
    (define call/cc0
      (lambda (f)
	(call/cc (lambda (k) (f (lambda () (k (void))))))))
    
    ; 2dim Vectors
    (define 2vector
      ; list of m lists of length n; return m x n matrix
      (lambda mn
	(apply vector (map list->vector mn))))
    
    (define 2make-vector
      ; m, n > 0; return m x n matrix, containing ?? in each cell
      (lambda (m n)
	(apply vector
	       (recur list-of ([m m])
		 (if (zero? m)
		     '()
		     (cons (make-vector n) (list-of (sub1 m))))))))
    
    (define 2vector-ref
      ; vector lookup in mn matrix; return mn[i,j]
      (lambda (mn i j)
	; 0 <= i < m, 0 <= j < n
	(vector-ref (vector-ref mn i) j)))
    
    (define 2vector-set!
      ; vector set! in mn matrix; change mn[i,j] to v; return void
      (lambda (mn i j v)
	;  0 <= i < m, 0 <= j < n, v is new value
	(vector-set! (vector-ref mn i) j v)))
    
    (define foreach!
      ; vector-mapc for vector
      (lambda (mvec pt)
	; pt is unary function
	(recur ! ([le (sub1 (vector-length mvec))])
	  (vector-set! mvec le (pt (vector-ref mvec le) le))
	  (if (zero? le)
	      mvec
	      (! (sub1 le))))))
    
    (define 2foreach!
      ; vector-mapc for mn matrix
      (lambda (mn pt)
	; pt is ternary function
	(foreach! mn
		  (lambda (vec row) (foreach! vec (lambda (x col) (pt x row col)))))))
    
    (define 2vector-init
      (lambda (m n init)
	(let ([M (2make-vector m n)])
	  (2foreach! M (lambda (_ i j) (init i j)))
	  M)))
    
    (define 2vector-print
      ; print each row in m x n vector via vector print; return void
      (lambda (mn vector-print)
	(map vector-print (vector->list mn))))
    
    ;; (build-vector n f) returns a vector 0..n-1 where the ith element is (f i).
    ;; The eval order is guaranteed to be: 0, 1, 2, ..., n-1.
    ;; eg: (build-vector 4 (lambda (i) i)) ==> #4(0 1 2 3)
    
    (define  build-vector
      (lambda  (n  fcn)
	(unless  (and (integer? n) (exact? n) (>= n 0))
	  (error  'build-vector  "~s must be an exact integer >= 0"  n))
	(unless  (procedure? fcn)
	  (error  'build-vector  "~s must be a procedure"  fcn))
	(let  ((vec  (make-vector n)))
	  (recur  loop  ((i  0))
	    (if  (= i n)  vec
		 (begin
		   (vector-set!  vec  i  (fcn i))
		   (loop  (add1 i))))))))
    
    (define  build-string
      (lambda  (n  fcn)
	(unless  (and (integer? n) (exact? n) (>= n 0))
	  (error  'build-string  "~s must be an exact integer >= 0"  n))
	(unless  (procedure? fcn)
	  (error  'build-string  "~s must be a procedure"  fcn))
	(let  ((str  (make-string n)))
	  (recur  loop  ((i  0))
	    (if  (= i n)  str
		 (begin
		   (string-set!  str  i  (fcn i))
		   (loop  (add1 i))))))))
    
    (define  build-list
      (lambda  (n  fcn)
	(unless  (and (integer? n) (exact? n) (>= n 0))
	  (error  'build-list  "~s must be an exact integer >= 0"  n))
	(unless  (procedure? fcn)
	  (error  'build-list  "~s must be a procedure"  fcn))
	(if  (zero? n)  '()
	     (let  ([head  (list (fcn 0))])
	       (recur  loop  ([i 1]  [p head])
		 (if  (= i n)  head
		      (begin
			(set-cdr!  p  (list (fcn i)))
			(loop  (add1 i)  (cdr p)))))))))
    
    (define tabulate build-vector)
    
    ;; The Little Lisper lists
    
    (define atom?
      (let ((not not) (pair? pair?) (null? null?))
	(lambda (a) (not (or (pair? a) (null? a))))))
    
    (define cons? pair?)
    
    (define last
      (lambda (l)
	(cond
	  [(null? l) (error 'last "received an empty list")]
	  [(null? (cdr l)) (car l)]
	  [else (last (cdr l))])))

    (define list? (if params:allow-improper-lists?
		      #%list?
		      (lambda (l) (or (cons? l) (null? l)))))

    (define make-last-checked
      (lambda (prim prim-name)
	(if params:allow-improper-lists?
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
	(if params:allow-improper-lists?
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
