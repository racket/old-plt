
(define-macro send*
  (lambda (x . rest)
    (let ([g (gensym "send*")])
      `(let ([,g ,x])
	 ,@(map (lambda (x) `(send ,g ,@x))
		rest)))))

; Another let-like form.
(define-macro local 
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
	    (let ([d (local-expand-defmacro def)])
	      (unless (and (pair? d) (eq? (car d) '#%define-values))
		      (raise-syntax-error
		       'local
		       "bad definition"
		       (list* 'local defines expr1 body)
		       def))
	      d))
	  defines)
       0 (let () ,expr1 ,@body))))

; recur is another name for 'let' in a named let
(define-macro recur 
  (lambda (name args . body) `(let ,name ,args ,@body)))

; define a recursive value
(define-macro rec
  (lambda (x rest)
    (if (symbol? x)
	`(letrec ([,x ,rest])
	   ,x)
	(syntax-error 'rec "identifier must be a symbol" 
		      (list 'rec x rest) x))))
