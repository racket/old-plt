

(load-relative "loadtest.ss")

(SECTION 'synchronization)

(define SYNC-SLEEP-DELAY 0.1)

;; ----------------------------------------
;; Waitable sets

(err/rt-test (waitables->waitable-set 7))
(err/rt-test (waitables->waitable-set (make-semaphore) 7))

(arity-test waitables->waitable-set 0 -1)

(test #f object-wait-multiple SYNC-SLEEP-DELAY (waitables->waitable-set))
(test #f object-wait-multiple SYNC-SLEEP-DELAY (waitables->waitable-set) (waitables->waitable-set))
(test #f object-wait-multiple SYNC-SLEEP-DELAY (waitables->waitable-set (waitables->waitable-set) (waitables->waitable-set)))

(let ([s1 (make-semaphore)]
      [s2 (make-semaphore)]
      [s3 (make-semaphore)])
  (test #f object-wait-multiple SYNC-SLEEP-DELAY (waitables->waitable-set s1 s2 s3))
  (semaphore-post s2)
  (test s2 object-wait-multiple SYNC-SLEEP-DELAY (waitables->waitable-set s1 s2 s3))
  (test #f object-wait-multiple SYNC-SLEEP-DELAY (waitables->waitable-set s1 s2 s3))
  (let ([set (waitables->waitable-set s1 s2 s3)])
    (test #f object-wait-multiple SYNC-SLEEP-DELAY set)
    (semaphore-post s2)
    (test s2 object-wait-multiple SYNC-SLEEP-DELAY set)
    (test #f object-wait-multiple SYNC-SLEEP-DELAY set))
  (thread (lambda () (sleep) (semaphore-post s3)))
  (test s3 object-wait-multiple SYNC-SLEEP-DELAY (waitables->waitable-set s1 s2 s3))
  (test #f object-wait-multiple SYNC-SLEEP-DELAY (waitables->waitable-set s1 s2 s3))
  (semaphore-post s3)
  (test s3 object-wait-multiple SYNC-SLEEP-DELAY s1 (waitables->waitable-set s2 s3))
  (test #f object-wait-multiple SYNC-SLEEP-DELAY s1 (waitables->waitable-set s2 s3))
  (semaphore-post s3)
  (test s3 object-wait-multiple SYNC-SLEEP-DELAY (waitables->waitable-set s1 s2) s3)
  (test #f object-wait-multiple SYNC-SLEEP-DELAY (waitables->waitable-set s1 s2) s3)
  (let ([set (waitables->waitable-set s1 s2)])
    (test #f object-wait-multiple SYNC-SLEEP-DELAY s1 set s3)
    (semaphore-post s2)
    (test s2 object-wait-multiple SYNC-SLEEP-DELAY set s3)
    (test #f object-wait-multiple SYNC-SLEEP-DELAY set s3))
  (test #f object-wait-multiple SYNC-SLEEP-DELAY (waitables->waitable-set s1 (waitables->waitable-set s2 s3)))
  (semaphore-post s3)
  (test s3 object-wait-multiple SYNC-SLEEP-DELAY (waitables->waitable-set s1 (waitables->waitable-set s2 s3)))
  (test #f object-wait-multiple SYNC-SLEEP-DELAY (waitables->waitable-set s1 (waitables->waitable-set s2 s3)))
  (semaphore-post s3)
  (test s3 object-wait-multiple SYNC-SLEEP-DELAY (waitables->waitable-set (waitables->waitable-set s1 s2) s3))
  (test #f object-wait-multiple SYNC-SLEEP-DELAY (waitables->waitable-set (waitables->waitable-set s1 s2) s3))
  (let ([set (waitables->waitable-set s1 (waitables->waitable-set s2 s3))])
    (test #f object-wait-multiple SYNC-SLEEP-DELAY set)
    (semaphore-post s3)
    (test s3 object-wait-multiple SYNC-SLEEP-DELAY set)
    (test #f object-wait-multiple SYNC-SLEEP-DELAY set)))

;; ----------------------------------------
;; Structures as waitables

;; Bad property value:
(err/rt-test (make-struct-type 'wt #f 2 0 #f (list (cons prop:waitable -1))) exn:application:mismatch?)
;; slot index 1 not immutable:
(err/rt-test (make-struct-type 'wt #f 2 0 #f (list (cons prop:waitable 1))) exn:application:mismatch?)

(define-values (struct:wt make-wt wt? wt-ref wt-set!)
  (make-struct-type 'wt #f 2 0 #f (list (cons prop:waitable 1)) (make-inspector) #f '(1)))

(let ([always-ready (make-wt #f (lambda () #t))]
      [always-stuck (make-wt 1 2)])
  (test always-ready object-wait-multiple #f always-ready)
  (test always-ready object-wait-multiple 0 always-ready)
  (test #f object-wait-multiple 0 always-stuck)
  (test #f object-wait-multiple SYNC-SLEEP-DELAY always-stuck))

;; Check whether something that takes at least SYNC-SLEEP-DELAY
;;  seconds in fact takes that roughyl much CPU time. We
;;  expect non-busy-wait takes to take a very small fraction
;;  of the time.
;; This test only works well if there are no other
;;  threads running and the underlying OS is not loaded.
(define (check-busy-wait go busy?)
  (collect-garbage) ; reduces false-positives in detecting busy wait
  (let ([msecs (current-process-milliseconds)]
	[real-msecs (current-milliseconds)])
    (go)
    (let ([took (/ (abs (- (current-process-milliseconds) msecs)) 1000.0)]
	  [real-took (/ (abs (- (current-milliseconds) real-msecs)) 1000.0)]
	  [boundary (/ SYNC-SLEEP-DELAY 6)])
      (test busy? (lambda (a b c d) (> b c)) 'busy-wait? took boundary real-took))))

(define (test-good-waitable wrap-sema)
  (let* ([sema (make-semaphore)]
	 [sema-ready (make-wt 1 (wrap-sema sema))])
    (test #f 'initial-sema (object-wait-multiple 0 sema-ready))
    (semaphore-post sema)
    (test sema-ready object-wait-multiple 0 sema-ready)
    (test #t semaphore-try-wait? sema)
    (test #f object-wait-multiple 0 sema-ready)
    (let ()
      (define (non-busy-wait waitable)
	(check-busy-wait
	 (lambda ()
	   (thread (lambda ()
		     (sleep SYNC-SLEEP-DELAY)
		     (semaphore-post sema)))
	   (test waitable object-wait-multiple #f waitable))
	 #f)
	(test waitable object-wait-multiple #f waitable)
	(test waitable object-wait-multiple #f waitable)
	(test #t semaphore-try-wait? sema)
	(test #f object-wait-multiple 0 waitable))
      (non-busy-wait sema-ready)
      (semaphore-post sema)
      (let ([wrapped (make-wt 3 (wrap-sema (make-wt 2 sema-ready)))])
	(non-busy-wait wrapped)))))

(test-good-waitable values)
(test-good-waitable (lambda (x) (waitables->waitable-set
				 x
				 (make-wt 99 (lambda () (make-semaphore))))))
(test-good-waitable (lambda (x) (lambda () 
				  (if (object-wait-multiple 0 x)
				      (begin
					(when (semaphore? x)
					  (semaphore-post x))
					#t)
				      x))))

(check-busy-wait
 (let* ([s (make-semaphore 1)]
	[wt (make-wt 1 (lambda () s))])
   (lambda ()
     (test #f object-wait-multiple SYNC-SLEEP-DELAY wt)))
 #t)

;; Check a (bad!) waitable that spin-blocks until its test function
;;  is called 1000 times.
(let* ([c 0]
       [s (make-semaphore 1)]
       [wt (make-wt 7 (lambda ()
			(if (c . < . 1000)
			    (begin
			      (set! c (add1 c))
			      s)
			    #t)))])
  (test wt object-wait-multiple #f wt))

;; ----------------------------------------

(define (test-stuck-port ready-waitable make-waitable-unready make-waitable-ready)
  (let* ([go? #f]
	 [bad-stuck-port (make-custom-input-port
			  ready-waitable
			  (lambda (str)
			    (if go?
				(begin
				  (string-set! str 0 #\x)
				  1)
				0))
			  #f
			  void)])
    (test #f char-ready? bad-stuck-port)
    (test #f object-wait-multiple SYNC-SLEEP-DELAY bad-stuck-port)
    (test #f object-wait-multiple 0 bad-stuck-port)
    (test 0 read-string-avail!* (make-string 10) bad-stuck-port)
    (make-waitable-unready ready-waitable)
    (test #f char-ready? bad-stuck-port)
    (test #f object-wait-multiple SYNC-SLEEP-DELAY bad-stuck-port)
    (test 0 read-string-avail!* (make-string 10) bad-stuck-port)
    (make-waitable-ready ready-waitable)
    (test #f char-ready? bad-stuck-port)
    (test #f object-wait-multiple SYNC-SLEEP-DELAY bad-stuck-port)
    (test 0 read-string-avail!* (make-string 10) bad-stuck-port)
    (set! go? #t)
    (test #t char-ready? bad-stuck-port)
    (test bad-stuck-port object-wait-multiple SYNC-SLEEP-DELAY bad-stuck-port)
    (test #t positive? (read-string-avail!* (make-string 10) bad-stuck-port))
    (set! go? #f)
    (test #f char-ready? bad-stuck-port)
    (test #f object-wait-multiple SYNC-SLEEP-DELAY bad-stuck-port)
    (test 0 read-string-avail!* (make-string 10) bad-stuck-port)
    (check-busy-wait
     (lambda ()
       (thread (lambda ()
		 (sleep SYNC-SLEEP-DELAY)
		 (set! go? #t)))
       (test bad-stuck-port object-wait-multiple (* 3 SYNC-SLEEP-DELAY) bad-stuck-port))
     #t)))

(test-stuck-port (make-semaphore 1) semaphore-wait semaphore-post)
(let ([s (make-semaphore 1)])
  (test-stuck-port (make-wt 77 s)
		   (lambda (wt) (semaphore-wait s))
		   (lambda (wt) (semaphore-post s))))
(let ([s (make-semaphore 1)])
  (test-stuck-port (make-wt 77 (lambda ()
				 (if (semaphore-try-wait? s)
				     (begin
				       (semaphore-post s)
				       #t)
				     s)))
		   (lambda (wt) (semaphore-wait s))
		   (lambda (wt) (semaphore-post s))))

;; ----------------------------------------

;; In the current implemenation, a depth of 10 for 
;;  waitable chains is a magic number; it causes the scheduler to
;;  swap a thread in to check whether it can run, instead of
;;  checking in the thread. (For a well-behaved chain, this 
;;  swap in will lead to a more friendly semaphore wait, for
;;  example.)

(letrec ([stack-em (lambda (n s)
		     ;; This needs to be tail-recursive to
		     ;; the find-depth check below (to check
		     ;; blocking depth, not precdure depth)
		     (if (zero? n)
			 s
			 (stack-em (sub1 n) (make-wt n s))))])
  (let* ([s1 (make-semaphore 1)]
	 [s20 (make-semaphore 1)]
	 [wt1 (stack-em 1 s1)]
	 [wt20 (stack-em 20 s20)])
    (test wt1 object-wait-multiple 0 wt1)
    (test wt20 object-wait-multiple 0 wt20)
    (test #t semaphore-try-wait? s1)
    (test #t semaphore-try-wait? s20)
    (let ([t20
	   (thread (lambda ()
		     (test wt20 object-wait-multiple #f wt20)))])
      (let loop ([n 20])
	(unless (zero? n)
	  (sleep)
	  (loop (sub1 n))))
      (semaphore-post s20)
      (test (void) thread-wait t20))
    ;; Induce stack overflow:
    (printf "overflow depth: ~a~n"
	    (find-depth 
	     (lambda (n)
	       (let* ([s (make-semaphore 1)]
		      [wt (stack-em n s)])
		 (test #t `(deep-wait . ,n) (and (object-wait-multiple 0 wt) #t))
		 (semaphore-wait s)
		 (test #f `(deep-wait . ,n) (object-wait-multiple 0 wt))))))))


;; ----------------------------------------
;; Transactions

(define-struct atomic (val))

(err/rt-test (struct-transact atomic-val (make-atomic 1) (make-semaphore) (make-semaphore) (make-semaphore) void))
(err/rt-test (struct-transact set-atomic-val! (make-atomic 1) 7 (make-semaphore) (make-semaphore) void))
(err/rt-test (struct-transact set-atomic-val! (make-atomic 1) (make-semaphore) 7 (make-semaphore) void))
(err/rt-test (struct-transact set-atomic-val! (make-atomic 1) (make-semaphore) (current-input-port) (make-semaphore) void))
(err/rt-test (struct-transact set-atomic-val! (make-atomic 1) (make-semaphore) (make-semaphore) 7 void))
(err/rt-test (struct-transact set-atomic-val! (make-atomic 1) (make-semaphore) (make-semaphore) (make-semaphore) (lambda () 10)))
(err/rt-test (struct-transact set-atomic-val! (make-atomic 1) (make-semaphore) (make-semaphore) (make-semaphore) (lambda (x y) 10)))
(err/rt-test (struct-transact set-atomic-val! 7 (make-semaphore) (make-semaphore) (make-semaphore) void) exn:application:mismatch?)

(arity-test struct-transact 6 6)
(arity-test struct-transact/enable-break 6 6)

(define (test-transactions struct-transact)
  (let* ([a (make-atomic 5)]
	 [s (make-semaphore 1)]
	 [fail (make-semaphore)]
	 [both (waitables->waitable-set s fail)])
    (test (void) struct-transact set-atomic-val! a both s fail (lambda (v)
								 (test s values v)
								 10))
    (test #t semaphore-try-wait? s)
    (semaphore-post s)
    (test #f semaphore-try-wait? fail)
    (test 10 atomic-val a)
    
    (let ([t (thread
	      (lambda ()
		(struct-transact set-atomic-val! a both s fail (lambda (v)
								 (test s values v)
								 (kill-thread (current-thread)) 7))))])
      (thread-wait t)
      (test #f semaphore-try-wait? s)
      (test #t semaphore-try-wait? fail)
      (test 10 atomic-val a))
    (semaphore-post s) ; reset a

    (let ([ts
	   (let contend ([n 10])
	     (if (zero? n)
		 null
		 (cons
		  (thread (lambda ()
			    (struct-transact set-atomic-val! a both s fail 
					     (lambda (v) 
					       (printf "running ~a~n" n)
					       (test #t 'ok-v (and (memq v (list s fail)) #t))
					       (if (= 0 (modulo n 3))
						   (kill-thread (current-thread))
						   (add1 (begin0
							  (atomic-val a)
							  (sleep))))))))
		  (contend (sub1 n)))))])
      (for-each thread-wait ts)

      (test 17 atomic-val a))))

(test-transactions struct-transact)
(test-transactions struct-transact/enable-break)

(let ([a (make-atomic 1)])
  (let ([t  (parameterize ([break-enabled #f])
	      (thread
	       (lambda ()
		 (struct-transact/enable-break
		  set-atomic-val! a
		  (make-semaphore) (make-semaphore) (make-semaphore)
		  (lambda (v)
		    'done)))))])
    (sleep)
    (test (void) break-thread t)
    (test (void) thread-wait t)
    (test 1 atomic-val a))
  (let* ([s (make-semaphore)]
	 [t  (parameterize ([break-enabled #f])
	       (thread
		(lambda ()
		  (struct-transact/enable-break
		   set-atomic-val! a
		   s (make-semaphore) (make-semaphore)
		   (lambda (v)
		     ;; if we get here, it's too late to break
		     (sleep SYNC-SLEEP-DELAY)
		     'done)))))])
    (sleep)
    (semaphore-post s)
    (sleep)
    (test (void) break-thread t)
    (test (void) thread-wait t)
    (test 'done atomic-val a)))

(report-errs)
