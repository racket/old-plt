
(define-macro split 
  (lambda Es
    `(splitfn (lambda () (begin ,@Es)))))

(define-macro split* 
  (lambda Es 
    `(split*fn (list ,@(map (lambda (x) `(lambda () ,x)) Es)))))

(define-macro tprompt
  (lambda Es
    `(fluid-let ([Turtles Turtles]
		 [Cache Cache])
		,@Es)))

(define-macro repeat
  (match-lambda* 
   [((var n) . Es) (let ((loop (gensym 'repeat)))
		     `(let ,loop ((,var 1))
			(when (<= ,var ,n)
			  ,@Es
			  (,loop (add1 ,var)))))]
   [_ (error 'repeat "bad syntax")]))
