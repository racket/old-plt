;;
;; $Id$
;;
;; Run fake events in single or multiple threads.
;; run-single/multiple returns value from last action.
;;

(unit/sig mred:test:run^
  
  (import [mred:test : mred:test:global^])
  
  (define run-error error)
  
  (define run-single
    (let ([tag  'run-single])
      (lambda (l)
	(let ([done  (make-semaphore 0)]
	      [ans   (void)])
	  (thread
	   (lambda ()
	     (let loop ([l l])
	       (cond
		 [(null? l)  (semaphore-post done)]
		 [(pair? l)
		  (let ([event  (car l)])
		    (if (mred:test:event? event)
			(begin
			  (set! ans ((mred:test:event-thunk event)))
			  (loop (cdr l)))
			(run-error tag "list contains non-event object")))]
		 [else  (run-error tag "input is not proper list")]))))
	  (semaphore-wait done)
	  ans))))
  
  (define run-multiple
    (let ([tag  'run-multiple])
      (lambda (l)
	(let ([ans  (void)])
	  (let loop ([l l])
	    (cond
	      [(null? l)  ans]
	      [(pair? l)
	       (let ([event  (car l)])
		 (if (mred:test:event? event)
		     (let ([done  (make-semaphore 0)])
		       (thread
			(lambda ()
			  (set! ans ((mred:test:event-thunk event)))
			  (semaphore-post done)))
		       (semaphore-wait done)
		       (loop (cdr l)))))]
	      [else  (run-error tag "input is not proper list")]))))))

  )
