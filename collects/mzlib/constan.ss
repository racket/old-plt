
; Map "(constant x)" to "(constant-symbol 'x)" if constants are
;  disabled
(define-macro constant 
  (lambda (symbol)
    `(constant-name (quote ,symbol))))

(define-macro constant-signature-content
  (lambda (file . collection)
    (let* ([c (if (null? collection)
		  '("mzlib")
		  collection)]
	   [dir (apply collection-path c)]
	   [file (build-path dir file)])
      (with-input-from-file file
	(lambda ()
	  (let ([sig (read)])
	    `(begin
	       ,@(map (lambda (x) `(constant ,x))
		      (caddr sig)))))))))

