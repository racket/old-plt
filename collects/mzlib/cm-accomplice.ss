(module cm-accomplice mzscheme
  (provide register-external-file)

  (define (register-external-file f)
    (unless (and (string? f)
		 (complete-path? f))
      (raise-type-error 'register-external-file "complete-path string" f))
    (let ([param (lambda () void)])
      (thread-wait 
       (thread (lambda ()
		 (set! param
		       (dynamic-require '(lib "cm-ctime.ss" "mzlib" "private")
					'current-external-file-registrar)))))
      ((param) f))))


