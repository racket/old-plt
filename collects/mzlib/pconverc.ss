(require-library "constant.ss")
(require-library "pconvert.ss")

(constant mzlib:print-convert^)
(constant mzlib:print-convert-hooks^)
(constant mzlib:print-convert@)
(constant mzlib:print-convert-hooks@)

(begin-elaboration-time
 (let ([get-signature
	(lambda (name)
	  (let ([e (eval `(let-id-macro 
			   x 
			   `',(global-expansion-time-value ',name) 
			   x))])
	    e))])
   `(begin
      ,@(map (lambda (x) `(constant ,x))
	     (vector->list (get-signature 'mzlib:print-convert^))))))

