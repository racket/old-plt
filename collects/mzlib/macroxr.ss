
(unit
 (import)
 (export send*
	 local
	 recur
	 rec)
 
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
			    (list 'rec x rest) x)))))

