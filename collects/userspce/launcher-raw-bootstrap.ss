(when (getenv "MREDDEBUG")
  (require-library "errortrace.ss" "errortrace") (error-print-width 200)
  (current-load (let ([ol (current-load)]) (lambda (x) (printf "~a~n" x) (ol x)))))

(lambda (filename)
  (call-with-input-file filename
    (lambda (port)
      (let loop ()
	(let ([exp (read port)])
	  (if (eof-object? exp)
	      (void)
	      (begin
		(call-with-values
		 (lambda () (eval exp))
		 (lambda x (for-each (lambda (x) (unless (void? x) (print x) (newline)))
				     x)))
		(loop))))))))
