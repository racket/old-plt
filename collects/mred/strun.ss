;;
;; $Id: strun.ss,v 1.3 1997/07/22 18:41:32 krentel Exp krentel $
;;
;; Run fake events in single or multiple threads.
;;
;; Run-single uses a single, separate thread, persistent across calls.
;; Run-multiple creates a new thread for each event.
;; Both return value from last action (like begin).
;;
;; (mred:test:run list [style]) runs list of events in default/explicit style.
;; (mred:test:default-run-style [style]) queries/resets default style.
;; (mred:test:new-single-thread) kills/recreates single thread.
;; style = 'single or 'multiple.
;;

(unit/sig mred:test:run^
  
  (import 
    [mred:test : mred:test:struct^]
    [mred:test : mred:test:globals^])
  
  (define run-error error)  ;; naive error handling (for now).
  
  ;;
  ;; (run list [style]) invokes run-single/multiple.
  ;; (default-run-style [style]) is parameterization for style.
  ;; style = 'single or 'multiple.
  ;;  
    
  (define current-run-style 'single)
  
  (define style->proc
    (lambda (style tag)
      (cond
	[(eq? style 'single)    run-single]
	[(eq? style 'multiple)  run-multiple]
	[else  (run-error tag "style is not 'single or 'multiple")])))
  
  (define default-run-style
    (let ([tag  'mred:test:default-run-style])
      (case-lambda
        [()  current-run-style]
	[(style)
	 (style->proc style tag)
	 (set! current-run-style style)])))
  
  (define run
    (let ([tag  'mred:test:run])
      (opt-lambda (l [style current-run-style])
	((style->proc style tag) l))))
  
  ;;
  ;; Run Single.
  ;; The global variables are for communication between the main thread
  ;; (running run-single) and the test thread that runs the events.
  ;; The Main Invariant here is that these two threads are mutually
  ;; exclusive, ie, cooperative scheduling.
  ;;
  
  (define main-sem (make-semaphore 0))
  (define test-sem (make-semaphore 0))
  (define test-thread  #f)
  
  (define queue null)
  (define ans  (void))
  (define ans-is-error #f)
  
  (define run-single
    (lambda (l)
      (unless (and (thread? test-thread) (thread-running? test-thread))
	(set! test-thread (make-single-thread)))
      (set! queue l)
      (semaphore-post test-sem)
      (semaphore-wait main-sem)
      (if ans-is-error (raise ans) ans)))
  
  (define new-single-thread
    (lambda ()
      (when (thread? test-thread)
	(kill-thread test-thread))))
  
  (define make-single-thread
    (let ([tag  'mred:test:run-single])
      (lambda ()
	(thread
	  (lambda ()
	    (letrec 
		([start
		  (lambda ()
		    (set! ans (void))
		    (set! ans-is-error #f)
		    (loop queue))]
		 [return
		  (lambda ()
		    (semaphore-post main-sem)
		    (semaphore-wait test-sem)
		    (start))]
		 [loop
		  (lambda (l)
		    (cond
		      [(null? l)  (return)]
		      [(pair? l)
		       (let ([event  (car l)])
			 (if (mred:test:event? event)
			     (begin
			       (with-handlers
				   (((lambda (x) #t)
				     (lambda (x) 
				       (set! ans x)
				       (set! ans-is-error #t))))
				 (set! ans ((mred:test:event-thunk event))))
			       (if ans-is-error (return) (loop (cdr l))))
			     (run-error tag "list contains non-event object")))]
		      [else  (run-error tag "input is not proper list")]))])
	      (semaphore-wait test-sem)
	      (start)))))))
  
  ;;
  ;; The thread communication in run-multiple is much simpler.
  ;;
  
  (define run-multiple
    (let ([tag  'mred:test:run-multiple])
      (lambda (l)
	(let ([ans  (void)]
	      [ans-is-error #f])
	  (let loop ([l l])
	    (cond
	      [(null? l)  ans]
	      [(pair? l)
	       (let ([event  (car l)])
		 (if (mred:test:event? event)
		     (begin
		       (thread-wait
			 (thread
			   (lambda ()
			     (with-handlers
				 (((lambda (x) #t) 
				   (lambda (x) 
				     (set! ans x)
				     (set! ans-is-error #t))))
			       (set! ans ((mred:test:event-thunk event)))))))
		       (if ans-is-error (raise ans) (loop (cdr l))))
		     (run-error tag "list contains non-event object")))]
	      [else  (run-error tag "input is not proper list")]))))))
  
  )
