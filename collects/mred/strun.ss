;;
;; $Id: strun.ss,v 1.6 1997/08/11 21:01:43 krentel Exp krentel $
;;
;; Run one fake action in the real handler thread.
;; This should be the only file that needs to worry about thread
;; issues, semaphores, etc.
;;
;; (mred:test:run-one thunk) is internal-only, not for export,
;; called directly from mred:test:primitives.
;;
;; (mred:test:run-interval [msec]) is parameterization for the
;; interval (in milliseconds) between starting actions.
;;
;; run-one (currently) does not return a useful value, but does
;; reraise errors from the actions.
;;

(unit/sig mred:test:run^
  
  (import)
  
  (define initial-run-interval 100)  ;; milliseconds
  
  (define run-error error)  ;; naive error handling (for now).
  
  ;;
  ;; The minimum time an action is allowed to run before returning from
  ;; mred:test:action.  Controls the rate at which actions are started, 
  ;; and gives some slack time for real events to complete (eg, update).
  ;; Make-parameter doesn't do what we need across threads.
  ;; Probably don't need semaphores here (set! is atomic).
  ;; Units are in milliseconds (as in wx:timer%).
  ;;
  
  (define run-interval
    (let ([tag  'mred:test:run-interval]
	  [msec  initial-run-interval])
      (case-lambda
        [()   msec]
	[(x)  (if (and (integer? x) (exact? x) (<= 0 x))
		  (set! msec x)
		  (run-error tag "expects exact, non-negative integer, given: ~s" x))])))
  
  ;;
  ;; How we get into the handler thread, and put fake actions 
  ;; on the real event queue.
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
  ;; Simple error catching/reporting.
  ;; Errors may occur long after mred:test:... has yielded and returned.
  ;; Store the first error and reraise it in main thread at first opportunity.
  ;; Getting/reraising error flushes the buffer (so can catch future errors).
  ;; the-error = #f (no error), or box containing exn value.
  ;; (Just so (raise #f) is correctly handled.)
  ;; Put-exn is called from handler thread, get-exn-box from main thread.
  ;; Do need semaphores because get-exn-box is not atomic.
  ;; Put-exn takes exn struct (not box), get-exn-box returns the box (or #f).
  ;;
  
  (define-values (put-exn get-exn-box is-exn?)
    (let ([sem  (make-semaphore 1)]
	  [the-error  #f])
      (letrec
	  ([put-exn
	    (lambda (exn)
	      (semaphore-wait sem)
	      (unless the-error
		(set! the-error (box exn)))
	      (semaphore-post sem))]
	   
	   [get-exn-box
	    (lambda ()
	      (semaphore-wait sem)
	      (let ([my-copy  the-error])
		(set! the-error #f)
		(semaphore-post sem)
		my-copy))]
	   
	   [is-exn?
	    (lambda ()
	      (semaphore-wait sem)
	      (let ([my-copy  the-error])
		(semaphore-post sem)
		(if my-copy #t #f)))])
	
	(values put-exn get-exn-box is-exn?))))
  
  ;;
  ;; Start running thunk in handler thread.
  ;; Don't return until run-interval expires, and thunk finishes, 
  ;; raises error, or yields (ie, at event boundary).
  ;; Reraise error, if exists, even from previous action.
  ;; Note: never more than one timer (of ours) on real event queue.
  ;; 
  
  (define run-one
    (lambda (thunk)
      (let ([sem  (make-semaphore 0)])
	(letrec
	    ([start
	      (lambda ()
		(wx:yield)  ;; flush out real events.
		(install-timer (run-interval) return)
		(unless (is-exn?)
		  (pass-errors-out thunk)))]
	     
	     [pass-errors-out
	      (lambda (thunk)
		(parameterize
		    ([current-exception-handler
		      (lambda (exn)
			(put-exn exn)
			((error-escape-handler)))])
		  (thunk)))]
	     
	     [return (lambda () (semaphore-post sem))])
	  
	  (install-timer 0 start)
	  (semaphore-wait sem)
	  (let ([exn-box  (get-exn-box)])
	    (if exn-box (raise (unbox exn-box)) (void)))))))
  
  )
