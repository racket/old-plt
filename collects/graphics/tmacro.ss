(define-macro split 
  (lambda (e . es)
    `(splitfn (lambda () (begin ,e ,@es)))))

(define-macro split* 
  (lambda es 
    `(split*fn (list ,@(map (lambda (x) `(lambda () ,x)) es)))))

(define-macro tprompt
  (lambda es
    `(fluid-let ([turtles-state turtles-state]
		 [turtles-cache turtles-cache])
		,@es)))
