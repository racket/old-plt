
; Tests macro expansion by setting the eval handler and
;  running all tests

(load-relative "loadtest.ss")

(namespace-variable-value 
 'expand-load
 #f
 (lambda ()
   (namespace-set-variable-value! 'expand-load "quiet.ss")))

(let ([orig (current-eval)])
  (dynamic-wind
   (lambda ()
     (current-eval
      (lambda (x)
	(set! mz-test-syntax-errors-allowed? #t)
	(let ([x (if (or (compiled-expression? x)
			 (and (syntax? x) (compiled-expression? (syntax-e x))))
		     x
		     (parameterize ([current-module-name-prefix #f])
		       (expand
			(expand x))))])
	  (set! mz-test-syntax-errors-allowed? #f)
	  (orig x)))))
   (lambda ()
     (load-relative expand-load))
   (lambda ()
     (current-eval orig))))
