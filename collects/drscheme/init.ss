(unit/sig drscheme:init^
  (import [mred : mred^])
  
  (define original-output-port (current-output-port))
  (define original-error-port (current-error-port))

  (define primitive-eval (current-eval))
  (define primitive-load (current-load))

  (print-struct #t)
  (break-enabled #f)

  (define system-custodian (current-custodian))
  (define system-eventspace (mred:current-eventspace))
  (define first-dir (current-directory))

  (error-display-handler
   (lambda (msg)
     (parameterize ([mred:current-eventspace system-eventspace]
		    [current-custodian system-custodian])
       (display msg)
       (newline)
       (mred:message-box "Internal Error"
			 msg)))))
  
