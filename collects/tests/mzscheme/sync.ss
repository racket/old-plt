

(load-relative "loadtest.ss")

(SECTION 'synchronization)

(define SYNC-SLEEP-DELAY 0.025)
(define SYNC-BUSY-DELAY 0.1) ; go a little slower to check busy waits

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
;; Wrapped waitables

(arity-test make-wrapped-waitable 2 2)

(err/rt-test (make-wrapped-waitable 1 void))
(err/rt-test (make-wrapped-waitable (make-semaphore) 10))
(err/rt-test (make-wrapped-waitable (make-semaphore) (lambda () 10)))

(test 17 object-wait-multiple #f (make-wrapped-waitable (make-semaphore 1) (lambda (sema) 17)))
(test 17 object-wait-multiple #f (waitables->waitable-set
				  (make-semaphore)
				  (make-wrapped-waitable (make-semaphore 1) (lambda (sema) 17))))
(test #t object-wait-multiple #f (make-wrapped-waitable (make-semaphore 1) semaphore?))
(test 18 'sync
      (let ([n 17]
	    [s (make-semaphore)])
	(thread (lambda () (sleep SYNC-SLEEP-DELAY) (semaphore-post s)))
	(object-wait-multiple #f 
			      (make-wrapped-waitable 
			       s 
			       (lambda (sema) (set! n (add1 n)) n))
			      (make-wrapped-waitable 
			       s 
			       (lambda (sema) (set! n (add1 n)) n)))))


;; ----------------------------------------
;; Nack waitables

(arity-test make-nack-waitable 1 1)
(arity-test make-guard-waitable 1 1)

(err/rt-test (make-nack-waitable 10))
(err/rt-test (make-nack-waitable (lambda () 10)))
(err/rt-test (make-guard-waitable 10))
(err/rt-test (make-guard-waitable (lambda (x) 10)))

(let ([s (make-semaphore 1)])
  (test s object-wait-multiple #f (make-nack-waitable (lambda (nack) s)))
  (test #f semaphore-try-wait? s)
  (semaphore-post s)
  (let ([v #f])
    (test #f object-wait-multiple 0
	  (make-nack-waitable (lambda (nack) 
				(set! v nack)
				(make-semaphore))))
    (test #t semaphore-try-wait? v)
    (set! v #f)
    (test #f object-wait-multiple SYNC-SLEEP-DELAY
	  (make-nack-waitable (lambda (nack) 
				(set! v nack)
				(make-semaphore))))
    (test #t semaphore-try-wait? v)
    (set! v #f)
    (test #f object-wait-multiple 0
	  (make-nack-waitable (lambda (nack) 
				(set! v nack)
				(make-semaphore)))
	  (make-nack-waitable (lambda (nack) 
				(set! v nack)
				(make-semaphore))))
    (test #t semaphore-try-wait? v)
    (set! v #f)
    (test #f object-wait-multiple SYNC-SLEEP-DELAY
	  (make-nack-waitable (lambda (nack) 
				(set! v nack)
				(make-semaphore)))
	  (make-nack-waitable (lambda (nack) 
				(set! v nack)
				(make-semaphore))))
    (test #t semaphore-try-wait? v)
    (set! v #f)
    (test #f object-wait-multiple SYNC-SLEEP-DELAY
	  (waitables->waitable-set 
	   (make-nack-waitable (lambda (nack) 
				 (set! v nack)
				 (make-semaphore)))
	   (make-nack-waitable (lambda (nack) 
				 (set! v nack)
				 (make-semaphore)))))
    (test #t semaphore-try-wait? v)
    (set! v #f)
    (test s object-wait-multiple 0
	  (make-nack-waitable (lambda (nack) 
				(set! v nack)
				s)))
    (test #f semaphore-try-wait? v) ; ... but not an exception!
    (semaphore-post s)
    (set! v #f)
    (test s object-wait-multiple 0
	  (make-nack-waitable (lambda (nack) 
				(set! v nack)
				(make-semaphore)))
	  s)
    (test #t semaphore-try-wait? v)
    (set! v #f)
    (err/rt-test (object-wait-multiple 0
				      (make-nack-waitable (lambda (nack) 
							    (set! v nack)
							    (make-semaphore)))
				      (make-nack-waitable (lambda (nack)
							    (/ 1 0))))
		 exn:application:divide-by-zero?)
    (test #t semaphore-try-wait? v)
    (set! v #f)
    (err/rt-test (object-wait-multiple 0
				      (make-nack-waitable (lambda (nack)
							    (/ 1 0)))
				      (make-nack-waitable (lambda (nack) 
							    (set! v nack)
							    (make-semaphore))))
		 exn:application:divide-by-zero?)
    (test #t not v)
    (set! v null)
    (test #f object-wait-multiple 0
	  (make-nack-waitable (lambda (nack) 
				(set! v (cons nack v))
				(make-semaphore)))
	  (make-nack-waitable (lambda (nack) 
				(set! v (cons nack v))
				(make-semaphore))))
    (test '(#t #t) map semaphore-try-wait? v)))

;; ----------------------------------------
;; Structures as waitables

;; Bad property value:
(err/rt-test (make-struct-type 'wt #f 2 0 #f (list (cons prop:waitable -1))) exn:application:mismatch?)
;; slot index 1 not immutable:
(err/rt-test (make-struct-type 'wt #f 2 0 #f (list (cons prop:waitable 1))) exn:application:mismatch?)

(define-values (struct:wt make-wt wt? wt-ref wt-set!)
  (make-struct-type 'wt #f 2 0 #f (list (cons prop:waitable 1)) (make-inspector) #f '(1)))

(let ([always-ready (make-wt #f (lambda (self) #t))]
      [always-stuck (make-wt 1 2)])
  (test always-ready object-wait-multiple #f always-ready)
  (test always-ready object-wait-multiple 0 always-ready)
  (test #f object-wait-multiple 0 always-stuck)
  (test #f object-wait-multiple SYNC-SLEEP-DELAY always-stuck))

;; Check whether something that takes at least SYNC-SLEEP-DELAY
;;  seconds in fact takes roughly that much CPU time. We
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
  (let ([sema (make-semaphore)])
    (letrec-values ([(sema-ready-part get-sema-result) (wrap-sema sema sema (lambda () sema-ready))]
		    [(sema-ready) (make-wt 1 sema-ready-part)])
      (test #f 'initial-sema (object-wait-multiple 0 sema-ready))
      (semaphore-post sema)
      (test (get-sema-result) object-wait-multiple 0 sema-ready)
      (test #f semaphore-try-wait? sema)
      (test #f object-wait-multiple 0 sema-ready)
      (semaphore-post sema)
      (let ()
	(define (non-busy-wait waitable get-result)
	  (check-busy-wait
	   (lambda ()
	     (thread (lambda ()
		       (sleep SYNC-BUSY-DELAY)
		       (semaphore-post sema)))
	     (test (get-result) object-wait-multiple #f waitable))
	   #f)
	  (test #f object-wait-multiple 0 waitable)
	  (semaphore-post sema)
	  (test (get-result) object-wait-multiple #f waitable)
	  (test #f object-wait-multiple 0 waitable)
	  (semaphore-post sema)
	  (test (get-result) object-wait-multiple #f waitable)
	  (test #f semaphore-try-wait? sema)
	  (test #f object-wait-multiple 0 waitable))
	(non-busy-wait sema-ready get-sema-result)
	(semaphore-post sema)
	(letrec-values ([(wrapped-part get-wrapped-result)
			 (wrap-sema (make-wt 2 (lambda (self) sema-ready))
				    (get-sema-result)
				    (lambda () sema-ready))]
			[(wrapped) (make-wt 3 wrapped-part)])
	  (non-busy-wait (get-wrapped-result) get-wrapped-result))))))

(test-good-waitable (lambda (x x-result get-self)
		      (values x (lambda () x-result))))
(test-good-waitable (lambda (x x-result get-self)
		      (let ([ws (waitables->waitable-set
				 x
				 (make-wt 99 (lambda (self) (make-semaphore))))])
			(values ws (lambda () x-result)))))

(check-busy-wait
 (letrec ([s (make-semaphore)]
	  [wt (make-wt 1 (lambda (self) (unless (or (eq? wt s)
						    (eq? self wt) )
					  (error 'wt "yikes: ~s != ~s" self wt)) 
				 wt))])
   (thread (lambda () (sleep (/ SYNC-BUSY-DELAY 2)) (set! wt s)))
   (lambda ()
     (test #f object-wait-multiple SYNC-BUSY-DELAY wt)))
 #t)

;; ----------------------------------------

(define (test-stuck-port ready-waitable make-waitable-unready make-waitable-ready)
  (let* ([go? #f]
	 [bad-stuck-port (make-custom-input-port
			  (lambda (str)
			    (if go?
				(begin
				  (string-set! str 0 #\x)
				  1)
				(if (zero? (random 2))
				    0
				    ready-waitable)))
			  #f
			  void)])
    (make-waitable-unready ready-waitable)
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
    (set! ready-waitable 0)
    (test #f object-wait-multiple 0 bad-stuck-port)
    (test #f object-wait-multiple 0 bad-stuck-port)
    (check-busy-wait
     (lambda ()
       (test #f object-wait-multiple SYNC-BUSY-DELAY bad-stuck-port))
     #t)
    (check-busy-wait
     (lambda ()
       (thread (lambda ()
		 (sleep SYNC-BUSY-DELAY)
		 (set! go? #t)))
       (test bad-stuck-port object-wait-multiple (* 3 SYNC-BUSY-DELAY) bad-stuck-port))
     #t)))

(test-stuck-port (make-semaphore 1) semaphore-try-wait? semaphore-post)
(let ([ready? #t])
  (test-stuck-port (make-wt 77 (lambda (self)
				 (if ready?
				     #t
				     (make-semaphore))))
		   (lambda (wt) (set! ready? #f))
		   (lambda (wt) (set! ready? #t))))
(let ([s (make-semaphore 1)])
  (test-stuck-port (make-wt 77 s)
		   (lambda (wt) (semaphore-try-wait? s))
		   (lambda (wt) (semaphore-post s))))
(let ([s (make-semaphore 1)])
  (test-stuck-port (make-wt 177 (lambda (self) s))
		   (lambda (wt) (semaphore-try-wait? s))
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
    (test s1 object-wait-multiple 0 wt1)
    (test s20 object-wait-multiple 0 wt20)
    (test #f semaphore-try-wait? s1)
    (test #f semaphore-try-wait? s20)
    (let ([t20
	   (thread (lambda ()
		     (test s20 object-wait-multiple 1.0 wt20)))])
      (let loop ([n 20])
	(unless (zero? n)
	  (sleep)
	  (loop (sub1 n))))
      (semaphore-post s20)
      (test (void) thread-wait t20))))


;; ----------------------------------------

(report-errs)
