
(define-macro send*
  (lambda (x . rest)
    (let ([g (gensym "send*")])
      `(let ([,g ,x])
	 ,@(map (lambda (x) `(send ,g ,@x))
		rest)))))

; Another let-like form. We perform no checking.
(define-macro local 
  (lambda (defines . body)
    `(let () ,@defines ,@body)))

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
