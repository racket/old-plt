(define-macro split 
  (lambda (e . es)
    `(splitfn (lambda () (begin ,e ,@es)))))

(define-macro split* 
  (lambda (e . es)
    `(split*fn (list ,@(map (lambda (x) `(lambda () ,x)) (cons e es))))))

(define-macro tprompt
  (lambda es
    `(tpromptfn
      (lambda ()
	,@es))))
