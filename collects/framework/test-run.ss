;;
;; $Id: test-run.ss,v 1.1 1998/11/19 17:23:42 robby Exp $
;;
;; Run one fake action in the real handler thread.
;; This should be the only file that needs to worry about multiple
;; threads, semaphores, etc.
;;
;; (mred:test:run-interval [msec]) is parameterization for the
;; interval (in milliseconds) between starting actions.
;;
;; (mred:test:number-pending-actions) => number of unfinished 
;; actions, counting a pending error as an unfinished action.
;; So, zero really means zero.
;;
;; (mred:test:reraise-error) reraises a pending error, if there
;; is one, else returns void.
;;
;; (mred:test:run-one thunk) is internal-only, not for export, called
;; directly from mred:test:primitives.  Currently does not return a 
;; useful value, but does reraise errors from the actions.
;;

(unit/sig framework:test:run^
  
  (import [mred : mred-interfaces^])

  (define initial-run-interval 100)  ;; milliseconds
  
  (define run-error error)  ;; naive error handling (for now).
  
  ;;
  ;; The minimum time an action is allowed to run before returning from
  ;; mred:test:action.  Controls the rate at which actions are started, 
  ;; and gives some slack time for real events to complete (eg, update).
  ;; Make-parameter doesn't do what we need across threads.
  ;; Probably don't need semaphores here (set! is atomic).
  ;; Units are in milliseconds (as in mred:timer%).
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
    (class mred:timer% (thunk)
      (override [notify thunk])
      (sequence (super-init))))
  
  (define install-timer
    (lambda (msec thunk)
      (let ([timer  (make-object timer-callback% thunk)])
	(send timer start msec #t))))
  
  ;;
  ;; Simple accounting of actions and errors.
  ;;
  ;; Keep number of unfinished actions.  An error in the buffer
  ;; (caught but not-yet-reraised) counts as an unfinished action.
  ;; (but kept in the-error, not count).
  ;;
  ;; Keep buffer of one error, and reraise at first opportunity.
  ;; Keep just first error, any others are thrown on the floor.
  ;; Reraising the error flushes the buffer.
  ;; Store exn in box, so can correctly catch (raise #f).
  ;; 
  ;; These values are set in handler thread and read in main thread,
  ;; so certainly need semaphores here.
  ;;
  
  (define-values (begin-action  end-action  end-action-with-error
		  get-exn-box   is-exn?     num-actions)
    (let 
	([sem    (make-semaphore 1)]
	 [count      0]     ;; number unfinished actions.
	 [the-error  #f])   ;; boxed exn struct, or else #f.
      (letrec
	  ([begin-action
	    (lambda ()
	      (semaphore-wait sem)
	      (set! count (add1 count))
	      (semaphore-post sem))]
	   
	   [end-action
	    (lambda ()
	      (semaphore-wait sem)
	      (set! count (sub1 count))
	      (semaphore-post sem))]
	   
	   [end-action-with-error
	    (lambda (exn)
	      (semaphore-wait sem)
	      (set! count (sub1 count))
	      (unless the-error
		(set! the-error (box exn)))
	      (semaphore-post sem))]
	   
	   [get-exn-box
	    (lambda ()
	      (semaphore-wait sem)
	      (let ([ans  the-error])
		(set! the-error #f)
		(semaphore-post sem)
		ans))]
	   
	   [is-exn?
	    (lambda ()
	      (semaphore-wait sem)
	      (let ([ans  (if the-error #t #f)])
		(semaphore-post sem)
		ans))]
 
	   [num-actions
	    (lambda ()
	      (semaphore-wait sem)
	      (let ([ans  (+ count (if the-error 1 0))])
		(semaphore-post sem)
		ans))])
	
	(values  begin-action  end-action  end-action-with-error
		 get-exn-box   is-exn?     num-actions))))
  
  ;; Functions to export, always in main thread.
  
  (define number-pending-actions num-actions)
  
  (define reraise-error
    (lambda ()
      (let ([exn-box  (get-exn-box)])
	(if exn-box (raise (unbox exn-box)) (void)))))
  
  ;;
  ;; Start running thunk in handler thread.
  ;; Don't return until run-interval expires, and thunk finishes, 
  ;; raises error, or yields (ie, at event boundary).
  ;; Reraise error (if exists) even from previous action.
  ;; Note: never more than one timer (of ours) on real event queue.
  ;; 
  
  (define run-one
    (lambda (thunk)
      (let ([sem  (make-semaphore 0)])
	(letrec
	    ([start
	      (lambda ()
		(mred:yield)  ;; flush out real events.
		(install-timer (run-interval) return)
		(unless (is-exn?)
		  (begin-action)
		  (pass-errors-out thunk)
		  (end-action)))]
	     
	     [pass-errors-out
	      (lambda (thunk)
		(parameterize
		    ([current-exception-handler
		      (lambda (exn)
			(end-action-with-error exn)
			((error-escape-handler)))])
		  (thunk)))]
	     
	     [return (lambda () (semaphore-post sem))])
	  
	  (install-timer 0 start)
	  (semaphore-wait sem)
	  (reraise-error))))))
