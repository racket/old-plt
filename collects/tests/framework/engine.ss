(define all-tests (map symbol->string (load-relative "README")))

(define (run-tests . tests)
  (let ([tests (if (null? test) all-tests tests)])
    (restart-mred)
    (for-each run-test tests)))

;; old, hopefully unnecessary
'(case (system-type)
   [(macos) 

    (when running?
      (let ([tmp-file (build-path (find-system-path 'temp-dir)
				  "frameworkempty.ss")])
	(call-with-output-file tmp-file
	  (lambda (port)
	    (newline port))
	  'truncate)
	(send-event "MrEd" "aevt" "quit")
	(let loop ()
	  (sleep 1)
	  (with-handlers ([(lambda (x) #t) void])
	    (printf "looping~n")
	    (send-event "MrEd" "aevt" "odoc" (vector 'file tmp-file))
	    (loop)))))
    (printf "macos: mred no longer running~n")])

(define-values (restart-mred send-sexp-to-mred)
  (let ([listener #f]
	[in-port #f]
	[out-port #f])
    (values
     (lambda ()
       (when listener
	 (tcp-close listener)
	 (set! listener #f))
       (set! listener (tcp-listen (load-relative "receive-sexps-port.ss")))
       (let-values ([(base _1 _2) (split-path program)])
	 (system* (mred-program-launcher-path "Framework Test Engine")))
       (let-values ([(in out) (tcp-accept listener)])
	 (set! in-port in)
	 (set! out-port out)))
     (lambda (sexp)
       (unless (and in-port out-port listener)
	 (error 'send-sexp-to-mred "mred not running"))
       (write sexp out-port)
       (newline out-port)
       (read in-port)))))

(restart-mred)
(restart-mred)
(send-sexp-to-mred `(begin (message-box "test" "test") (+ 1 2)))
