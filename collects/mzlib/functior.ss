(unit/sig
   mzlib:function^
   (import)

   (define identity (polymorphic (lambda (x) x)))

   (define compose
     (polymorphic
      (case-lambda 
       [(f) f]
       [(f g)
	(if (eqv? 1 (arity f)) ; optimize: don't use call-w-values
	    (if (eqv? 1 (arity g)) ; optimize: single arity everywhere
		(lambda (x) (f (g x)))
		(lambda args (f (apply g args))))
	    (lambda args
	      (call-with-values
	       (lambda () (apply g args))
	       f)))]
       [(f . more)
	(let ([m (apply compose more)])
	  (compose f m))])))

   (define quicksort
     (polymorphic
      (lambda (l less-than)
	(let* ([v (list->vector l)]
	       [count (vector-length v)])
	  (let loop ([min 0][max count])
	    (if (< min (sub1 max))
		(let ([pval (vector-ref v min)])
		  (let pivot-loop ([pivot min]
				   [pos (add1 min)])
		    (if (< pos max)
			(let ([cval (vector-ref v pos)])
			  (if (less-than cval pval)
			      (begin
				(vector-set! v pos (vector-ref v pivot))
				(vector-set! v pivot cval)
				(pivot-loop (add1 pivot) (add1 pos)))
			      (pivot-loop pivot (add1 pos))))
			(if (= min pivot)
			    (loop (add1 pivot) max)
			    (begin
			      (loop min pivot)
			      (loop pivot max))))))))
	  (vector->list v)))))

   (define ignore-errors
     (polymorphic
      (lambda (thunk)
	(let/ec escape
	  (with-handlers ([void (lambda (x) (escape (void)))])
	    (thunk))))))

   (define remove
     (polymorphic
      (letrec ([rm (case-lambda 
		    [(item list) (rm item list equal?)]
		    [(item list equal?)
		     (let loop ([list list])
		       (cond
			[(null? list) ()]
			[(equal? item (car list)) (cdr list)]
			[else (cons (car list)
				    (loop (cdr list)))]))])])
	rm)))

   (define remq
     (polymorphic
      (lambda (item list)
	(remove item list eq?))))

   (define remv
     (polymorphic
      (lambda (item list)
	(remove item list eqv?))))

   (define dynamic-disable-break
     (polymorphic
      (lambda (thunk)
	(parameterize ([break-enabled #f])
	  (thunk)))))

   (define dynamic-wind/protect-break
     (polymorphic
      (lambda (a b c)
	(let ([enabled? (break-enabled)])
	  (dynamic-disable-break
	   (lambda ()
	     (dynamic-wind 
	      a
	      (if enabled?
		  (lambda () (dynamic-enable-break b))
		  b)
	      c)))))))
   
   (define make-single-threader
     (polymorphic
      (lambda ()
	(let ([sema (make-semaphore 1)])
	  (lambda (thunk)
	    (dynamic-wind
	     (lambda () (semaphore-wait sema))
	     thunk
	     (lambda () (semaphore-post sema))))))))

   ;; fold : ((A -> B) B (listof A) -> B)
   ;; fold : ((A1 ... An -> B) B (listof A1) ... (listof An) -> B)

   ;; foldl builds "B" from the beginning of the list to the end of the
   ;; list and foldr builds the "B" from the end of the list to the
   ;; beginning of the list.

   (define mapadd
     (polymorphic
      (lambda (f l last)
	(letrec ((helper
		  (lambda (l)
		    (cond
		     [(null? l) (list last)]
		     [else (cons (f (car l)) (helper (cdr l)))]))))
	  (helper l)))))

   (define foldl
     (polymorphic
      (letrec ((fold-one
		(lambda (f init l)
		  (letrec ((helper
			    (lambda (init l)
			      (cond
			       [(null? l) init]
			       [else (helper (f (car l) init) (cdr l))]))))
		    (helper init l))))
	       (fold-n
		(lambda (f init  l)
		  (cond
		   [(ormap null? l)
		    (if (andmap null? l) 
			init
			(error 'foldl "received non-equal length input lists"))]
		   [else (fold-n
			  f
			  (apply f (mapadd car l init))
			  (map cdr l))]))))
	(case-lambda
	 [(f init l) (fold-one f init l)]
	 [(f init . ls) (fold-n f init ls)]))))
   
   (define foldr
     (polymorphic
      (letrec ((fold-one
		(lambda (f init l)
		  (letrec ((helper
			    (lambda (init l)
			      (cond
			       [(null? l) init]
			       [else (f (car l) (helper init (cdr l)))]))))
		    (helper init l))))
	       (fold-n
		(lambda (f init l)
		  (cond
		   [(ormap null? l)
		    (if (andmap null? l)
			init
			(error 'foldr "received non-equal length input lists"))]
		   [else (apply f
				(mapadd car l
					(fold-n f init (map cdr l))))]))))
	(case-lambda
	 [(f init l) (fold-one f init l)]
	 [(f init . ls) (fold-n f init ls)]))))
   
   (define first (polymorphic car))
   (define second (polymorphic cadr))
   (define third (polymorphic caddr))
   (define fourth (polymorphic cadddr))
   (define fifth (polymorphic (compose fourth cdr)))
   (define sixth (polymorphic (compose fourth cddr)))
   (define seventh (polymorphic (compose fourth cdddr)))
   (define eighth (polymorphic (compose fourth cddddr)))

   (define rest (polymorphic cdr))

   (define  build-string
     (lambda  (n  fcn)
       (unless  (and (integer? n) (exact? n) (>= n 0))
	 (error  'build-string  "~s must be an exact integer >= 0"  n))
       (unless  (procedure? fcn)
	 (error  'build-string  "~s must be a procedure"  fcn))
       (let  ((str  (make-string n)))
	 (let  loop  ((i  0))
	   (if (= i n)  
	       str
	       (begin
		 (string-set!  str  i  (fcn i))
		 (loop  (add1 i))))))))
    
    ;; (build-vector n f) returns a vector 0..n-1 where the ith element is (f i).
    ;; The eval order is guaranteed to be: 0, 1, 2, ..., n-1.
    ;; eg: (build-vector 4 (lambda (i) i)) ==> #4(0 1 2 3)
    
    (define  build-vector
      (polymorphic
       (lambda  (n  fcn)
	 (unless  (and (integer? n) (exact? n) (>= n 0))
		  (error  'build-vector  "~s must be an exact integer >= 0"  n))
	 (unless  (procedure? fcn)
		  (error  'build-vector  "~s must be a procedure"  fcn))
	 (let  ((vec  (make-vector n)))
	   (let  loop  ((i  0))
	     (if  (= i n)  vec
		  (begin
		    (vector-set!  vec  i  (fcn i))
		    (loop  (add1 i)))))))))
    
    (define  build-list
      (polymorphic
       (lambda  (n  fcn)
	 (unless  (and (integer? n) (exact? n) (>= n 0))
		  (error  'build-list  "~s must be an exact integer >= 0"  n))
	 (unless  (procedure? fcn)
		  (error  'build-list  "~s must be a procedure"  fcn))
	 (if  (zero? n)  '()
	      (let  ([head  (list (fcn 0))])
		(let  loop  ([i 1]  [p head])
		  (if  (= i n)  head
		       (begin
			 (set-cdr!  p  (list (fcn i)))
			 (loop  (add1 i)  (cdr p))))))))))

    (define loop-until
      (polymorphic
       (lambda (start done? next body)
	 (let loop ([i start])
	   (unless (done? i)
		   (body i)
		   (loop (next i)))))))

    (define last-pair
      (polymorphic
       (lambda (l)
	 (if (pair? l)
	     (if (pair? (cdr l))
		 (last-pair (cdr l))
		 l)
	     (error 'last-pair "argument not a pair")))))

    (define cons? pair?)
  )
