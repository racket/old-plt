
; Tests macro expansion by setting the eval handler and
;  running all tests

(load-relative "loadtest.ss")

(with-handlers ([not-break-exn?
		 (lambda (exn)
		   (namespace-variable-binding
		    'expand-load
		    "quiet.ss"))])
  (namespace-variable-binding 'expand-load))

(let ([orig (current-eval)])
  (dynamic-wind
   (lambda ()
     (current-eval
      (lambda (x)
	(set! mz-test-syntax-errors-allowed? #t)
	(let ([x (if (compiled-expression? x)
		     x
		     (parameterize ([current-module-name-prefix #f])
		       (expand
			(expand
			 (expand-once
			  (expand-once x))))))])
	  (set! mz-test-syntax-errors-allowed? #f)
	  (orig x)))))
   (lambda ()
     (load-relative expand-load))
   (lambda ()
     (current-eval orig))))
