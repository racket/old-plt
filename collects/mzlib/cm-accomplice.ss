(module cm-accomplice mzscheme
  (provide register-external-file)

  (define (register-external-file f)
    (unless (and (string? f)
		 (complete-path? f))
      (raise-type-error 'register-external-file "complete-path string" f))
    (let ([param (lambda () void)])
      ;; Load the code in a separate thread, so that the dynamic
      ;; extent of this one (likely a pase-sensitive macro expansion)
      ;; doesn't pollute the load:
      (thread-wait 
       (thread (lambda ()
		 (set! param
		       (dynamic-require '(lib "cm-ctime.ss" "mzlib" "private")
					'current-external-file-registrar)))))
      ((param) f))))


