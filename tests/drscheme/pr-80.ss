;;; pr-80.ss

(load-relative "drscheme-test-util.ss")

(letrec* ([drscheme-frame (wait-for-drscheme-frame)]
	  [build-frame/push-button
	   (lambda (cb-code)
	     (let ([code
		    (format "(let* ([f (make-object mred:frame% null \"MyFrame\" 0 0 50 90)]~n[p (make-object mred:vertical-panel% f)]~n[b (make-object mred:button% p~n(lambda Q ~a)~n\"b\")])~n(send f show #t))"
			    cb-code)])
	       (clear-definitions drscheme-frame)
	       (parameterize ([mred:test:current-get-eventspaces (lambda ()
								   (list
								    (get-user-eventspace)
								    (wx:current-eventspace)))])
		 (type-in-definitions drscheme-frame code)
		 (push-button-and-wait (ivar drscheme-frame execute-button))

		 (printf "frame: ~a~n" (mred:test:get-active-frame)))))])
  (build-frame/push-button "(car 4)"))
