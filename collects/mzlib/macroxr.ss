
(unit
 (import)
 (export send*
	 local
	 recur
	 rec
	 signature->symbols)
 
 (define send*
  (lambda (x . rest)
    (let ([g (gensym "send*")])
      `(let ([,g ,x])
	 ,@(map (lambda (x) `(send ,g ,@x))
		rest)))))

 ;; Another let-like form.
 (define local 
  (lambda (defines expr1 . body)
    (unless (list? defines)
	    (raise-syntax-error
		       'local
		       "bad definition sequence"
		       (list* 'local defines expr1 body)
		       defines))
    `(let () 
       ,@(map
	  (lambda (def)
	    (let-values ([(d kind) (local-expand-body-expression def)])
	      (unless (and (eq? kind '#%define-values)
			   (list? d)
			   (= 3 (length d)))
		      (raise-syntax-error
		       'local
		       "bad definition"
		       (list* 'local defines expr1 body)
		       def))
	      d))
	  defines)
       0 (let () ,expr1 ,@body))))

 ;; recur is another name for 'let' in a named let
 (define recur 
  (lambda (name args . body) `(let ,name ,args ,@body)))

 ;; define a recursive value
 (define rec
  (lambda (x rest)
    (if (symbol? x)
	`(letrec ([,x ,rest])
	   ,x)
	(raise-syntax-error 'rec "identifier must be a symbol" 
			    (list 'rec x rest) x))))


 (define signature->symbols
   (lambda (name)
     (unless (symbol? name)
	     (raise-syntax-error 'signature->symbols
				 "not an identifier"
				 (list 'signature->symbols name)
				 name))
     (let ([v (global-expansion-time-value name)])
       (letrec ([sig? (lambda (v)
			(and (vector? v)
			     (andmap
			      (lambda (s)
				(or (and (pair? s)
					 (symbol? (car s))
					 (sig? (cdr s)))
				    (symbol? s)))
			      (vector->list v))))])
	 (unless (sig? v)
		 (raise-syntax-error 'signature->symbols
				     "expansion-time value is not a signature"
				     (list 'signature->symbols name)
				     name))
	 v)))))
