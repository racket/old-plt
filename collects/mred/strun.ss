;;
;; $Id: strun.ss,v 1.5 1997/08/08 20:36:37 krentel Exp krentel $
;;
;; Run fake actions in the real handler thread.
;;
;; (mred:test:run list) runs (initiates) the actions from list,
;; where list is a list of mred:test:event structs (created by
;; primitives like mred:test:button-push, etc).
;;
;; (mred:test:run-interval [msec]) is parameterization for the
;; interval (in milliseconds) between starting actions in list.
;;
;; run (currently) does not return a useful value, but does
;; reraise errors from the actions.
;;

(unit/sig mred:test:run^
  
  (import 
    [mred:test : mred:test:struct^])
  
  (define initial-run-interval 100)  ;; milliseconds
  
  (define run-error error)  ;; naive error handling (for now).
  
  ;;
  ;; The time between initiating actions from the run queue.
  ;; This is just the interval between their start times.
  ;; You can't control how long the first action takes.
  ;; make-parameter doesn't do what we need across threads.
  ;;
  
  (define run-interval
    (let ([tag     'mred:test:run-interval]
	  [current  initial-run-interval])
      (case-lambda
        [()   current]
	[(x)  (if (and (integer? x) (<= 0 x))
		  (set! current x)
		  (error tag "expects non-negative integer, given: ~s" x))])))
  
  ;;
  ;; This is how we get into the handler thread,
  ;; and put fake actions on the real event queue.
  ;;
  
  (define timer-callback%
    (class wx:timer% (thunk)
      (public [notify  thunk])
      (sequence (super-init))))
  
  (define install-timer
    (lambda (msec thunk)
      (let ([timer  (make-object timer-callback% thunk)])
	(send timer start msec #t))))
  
  ;;
  ;; Run list of fake actions.
  ;; The actions run in a handler thread (from wx:timer%).
  ;; Catch errors and reraise them in main thread.
  ;;
  ;; Can only guarantee to start the actions, not finish them.
  ;; But any action that never yields will run to completion.
  ;;
  ;; Put queue, etc inside scope of run so that successive
  ;; calls to run won't conflict.
  ;; 
  
  (define run
    (lambda (l)
      (let 
	  ([tag        'mred:test:run]
	   [main-sem   (make-semaphore 0)]
	   [queue      l]
	   [is-error   #f]
	   [the-error  #f])
	
	(letrec
	    ([get-head
	      (lambda () 
		(begin0 (car queue) (set! queue (cdr queue))))]
	     
	     ;; Catch errors in thunk and pass them to main thread.
	     ;; This only catches the first error (then run returns).
	     ;; This even handles (raise #f). 
	     
	     [pass-errors-out
	      (lambda (thunk)
		(parameterize
		    ((current-exception-handler
		      (lambda (exn)
			(unless is-error
			  (set! queue null)
			  (set! is-error #t)
			  (set! the-error exn)
			  (semaphore-post main-sem))
			((error-escape-handler)))))
		  (thunk)))]
	     
	     ;; Start running the next event from the queue.
	     ;; But first, install timer for the rest of queue.
	     ;; There is never more than one un-notified timer,
	     ;; so (wx:yield) can only yield to real events
	     ;; (eg, update), and is usually a no-op.
	     
	     [process-queue
	      (lambda ()
		(wx:yield)  ;; flush out real events.
		(cond
		  [(null? queue)  (semaphore-post main-sem)]
		  [(pair? queue)
		   (let ([event  (get-head)])
		     (cond
		       [(mred:test:event? event)
			(let ([thunk  (mred:test:event-thunk event)]
			      [msec   (if (mred:test:sleep? event)
					  (mred:test:sleep-msec event)
					  (run-interval))])
			  (install-timer msec process-queue)
			  (pass-errors-out thunk))]
		       [else 
			(pass-errors-out
			 (lambda ()
			   (run-error tag "input contains non-event object")))]))]
		  [else
		   (pass-errors-out
		    (lambda ()
		      (run-error tag "input is not proper list")))]))]
	     )
	  
	  (install-timer 0 process-queue)
	  (semaphore-wait main-sem)
	  (if is-error (raise the-error) (void))))))
    
  )
