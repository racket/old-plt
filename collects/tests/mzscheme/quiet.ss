
(with-handlers ([not-break-exn?
		 (lambda (exn)
		   (namespace-variable-binding
		    'quiet-load
		    "all.ss"))])
  (namespace-variable-binding 'quiet-load))

(let ([p (make-output-port void void)])
  (parameterize ([current-output-port p])
      (load-relative quiet-load))
  (report-errs))

