
; Map "(constant x)" to "(constant-symbol 'x)" if constants are
;  disabled
(define-macro constant 
  (lambda (symbol)
    `(constant-name (quote ,symbol))))

(define-macro constant-signature-content
  (lambda (sig)
    (let ([get-signature
	   (lambda (name)
	     (let ([e (eval `(let-id-macro 
			      x 
			      `',(global-expansion-time-value ',name) 
			      x))])
	       e))])
      `(begin
	 ,@(map (lambda (x) `(constant ,x))
		(vector->list (get-signature sig)))))))

