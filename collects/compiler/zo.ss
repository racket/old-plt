
(require-library "compile.ss")
(require-library "file.ss" "mzscheme" "dynext")

(define (compile-to-zo src dest namespace)
  (let ([cwd (current-directory)])
    (parameterize ([current-namespace namespace]) 
      (with-handlers ([void (lambda (exn)
			      (delete-file (path->complete-path dest cwd))
			      (raise exn))])
        (compile-file src dest
		      '(use-current-namespace
			ignore-macro-definitions
			ignore-require-library))
	(printf " [output to \"~a\"]~n" dest)))))

