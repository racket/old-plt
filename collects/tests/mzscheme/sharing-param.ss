(if (not (defined? 'SECTION))
    (load-relative "testing.ss")
    (void))

(let* ([parameters (list (vector (make-parameter #f) (box #f))
			 (vector current-exception-handler (lambda x x))
			 (vector current-prompt-read (lambda x x))
			 (vector current-eval (lambda x x))
			 (vector current-namespace (make-namespace))
			 (vector current-print (lambda x x))
			 (vector current-load (lambda x x))
			 (vector current-load-extension (lambda x x))
			 (vector current-load-relative-directory (car (filesystem-root-list)))
			 (vector debug-info-handler (lambda x x))
			 (vector error-display-handler (lambda x x))
			 (vector error-print-width (expt 2 65))
			 (vector error-value->string-handler (lambda x x))
			 (vector break-enabled (box #f))
			 (vector exception-break-enabled (box #f))
			 (vector user-break-poll-handler (lambda x x))
			 (vector current-input-port (make-input-port void void void))
			 (vector current-output-port (make-output-port void void))
			 (vector current-error-port (make-output-port void void))
			 (vector global-port-print-handler (lambda x x))
			 (vector current-custodian (make-custodian))
			 (vector current-will-executor (make-will-executor))
			 (vector read-case-sensitive (box #f))
			 (vector read-square-bracket-as-paren (box #f))
			 (vector read-curly-brace-as-paren (box #f))
			 (vector read-accept-box (box #f))
			 (vector read-accept-type-symbol (box #f))
			 (vector read-accept-compiled (box #f))
			 (vector read-accept-bar-quote (box #f))
			 (vector read-accept-graph (box #f))
			 (vector print-graph (box #f))
			 (vector print-struct  (box #f))
			 (vector print-box (box #f))
			 (vector exit-handler (lambda x x))
			 (vector compile-allow-cond-fallthrough (box #f))
			 (vector compile-allow-set!-undefined (box #f))
			 (vector parameterization-branch-handler (lambda x x))
			 (vector current-library-collection-paths (list "aa"))
			 (vector require-library-use-compiled (box #f))
			 (vector current-require-relative-collection (list "a")))]
			 
       [sharing-parameterizations?
	(lambda (sharing? param1 param2)
	  (for-each (lambda (param-vector)
		      (test
		       sharing?
		       (lambda (param value)
			 (let* ([p1 (in-parameterization param1 param)]
				[p2 (in-parameterization param2 param)]
				[orig (p1)])
			   (p1 value)
			   (begin0 (eq? (p2) value)
				   (p1 orig))))
		       (vector-ref param-vector 0)
		       (vector-ref param-vector 1)))
		    parameters))])

  (let ([has-wx? (defined? 'wx@)]
	[evt-param (and has-wx? 
			(wx:eventspace-parameterization (wx:make-eventspace)))]
	[new-param (make-parameterization)]
	[cur-param (current-parameterization)])

    (test #f eq? cur-param new-param)
    (when has-wx?
      (test #f eq? evt-param new-param)
      (test #f eq? evt-param cur-param))

    (sharing-parameterizations? #f new-param cur-param)
    (when has-wx?
      (sharing-parameterizations? #f evt-param cur-param)
      (sharing-parameterizations? #f evt-param new-param))))

(report-errs)
