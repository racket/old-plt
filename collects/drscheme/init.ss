(unit/sig drscheme:init^
  (import [mred : mred-interfaces^])
  
  (define original-output-port (current-output-port))
  (define original-error-port (current-error-port))

  (define primitive-eval (current-eval))
  (define primitive-load (current-load))

  (print-struct #t)
  (break-enabled #f)

  (Define system-custodian (current-custodian))
  (Define system-eventspace (current-eventspace))

  (error-display-handler
   (lambda (msg)
     (with-parameterization system-parameterization
       (lambda ()
	 (display msg)
	 (newline)
	 (mred:message-box (format "Internal Error: ~a" msg)
			   "Internal Error"))))))
  
