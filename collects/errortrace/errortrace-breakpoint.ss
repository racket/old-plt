(module errortrace-breakpoint mzscheme
  
  (require (lib "contract.ss")
	   (lib "mred.ss" "mred"))
  
  (provide/contract [break (-> any)])
  
  (define (break)
    (let* ([break-semaphore (make-semaphore)])
      (queue-callback
       (lambda ()
	 ((error-display-handler) "a breakpoint has occurred" (make-exn:break "a breakpoint has occurred" (current-continuation-marks) (lambda (x) (semaphore-post break-semaphore))))))
      (semaphore-wait break-semaphore))))

