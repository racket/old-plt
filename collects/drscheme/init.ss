(module init mzscheme
  (require "mred-wrap.ss")
  (provide original-output-port
           original-error-port
           primitive-eval
           primitive-load
           system-custodian
           system-eventspace
           first-dir)
  
  (define original-output-port (current-output-port))
  (define original-error-port (current-error-port))

  (define primitive-eval (current-eval))
  (define primitive-load (current-load))

  (print-struct #t)
  (break-enabled #f)

  (define system-custodian (current-custodian))
  (define system-eventspace (mred:current-eventspace))
  (define system-thread (current-thread))
  (define first-dir (current-directory))

  (error-display-handler
   (lambda (msg)
     (display msg)
     (newline)
     (if (eq? (mred:current-eventspace) system-eventspace)
	 (mred:message-box "DrScheme Internal Error" msg)
	 (parameterize ([mred:current-eventspace system-eventspace]
			[current-custodian system-custodian])
	   (mred:queue-callback
	    (lambda ()
	      (mred:message-box "DrScheme Internal Error" msg))))))))
