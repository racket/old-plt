(unit/sig drscheme:init^
  (import [mred : mred^])
  
  (define primitive-eval (current-eval))
  (define primitive-load (current-load))

  (define top-parameterization (current-parameterization))
  (define system-parameterization (make-parameterization top-parameterization))
  (define eval-thread-parameterization
    (make-parameterization-with-sharing 
     system-parameterization
     system-parameterization
     (list current-exception-handler)
     (lambda (x) #f)))
  (define system-custodian (current-custodian))

  (current-parameterization system-parameterization)

  (parameterization-branch-handler
   (lambda ()
     (make-parameterization-with-sharing 
      system-parameterization
      system-parameterization
      (list current-exception-handler)
      (lambda (x) #f))))

  (define system-eventspace (mred:make-eventspace))
  (mred:current-eventspace system-eventspace)

  (print-struct #t)
  (break-enabled #f)
  ((in-parameterization eval-thread-parameterization break-enabled) #f)
  ((in-parameterization eval-thread-parameterization print-struct) #t)

  (error-display-handler
   (lambda (msg)
     (with-parameterization system-parameterization
       (lambda ()
	 (display msg)
	 (newline)
	 (mred:message-box (format "Internal Error: ~a" msg)
			   "Internal Error"))))))
  
