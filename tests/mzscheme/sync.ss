

(load-relative "loadtest.ss")

(SECTION 'synchronization)

(define SYNC-SLEEP-DELAY 0.025)
(define SYNC-BUSY-DELAY 0.1) ; go a little slower to check busy waits

;; ----------------------------------------
;;  Semaphore peeks

(let* ([s (make-semaphore)]
       [p (make-semaphore-peek s)]
       [ch (make-channel)])
  (test #f object-wait-multiple 0 s p)
  (test #f object-wait-multiple 0 s p)
  (semaphore-post s)
  (test p object-wait-multiple 0 p)
  (test p object-wait-multiple #f p)
  (test s object-wait-multiple #f s)
  (test #f object-wait-multiple 0 p)
  (thread (lambda () (sleep SYNC-SLEEP-DELAY) (semaphore-post s)))
  (test p object-wait-multiple #f p)
  (test p object-wait-multiple #f p)
  (test s object-wait-multiple #f s)
  (test #f object-wait-multiple 0 p)
  (thread (lambda () (object-wait-multiple 0 p) (channel-put ch 7)))
  (thread (lambda () (object-wait-multiple 0 p) (channel-put ch 7)))
  (thread (lambda () (object-wait-multiple 0 p) (channel-put ch 7)))
  (semaphore-post s)
  (test 7 channel-get ch)
  (test 7 channel-get ch)
  (test 7 channel-get ch)
  (test #f channel-try-get ch)
  (thread (lambda () (channel-put ch 9)))
  (sleep SYNC-SLEEP-DELAY)
  (test 9 channel-try-get ch)
  (test #f channel-try-get ch))

(arity-test make-semaphore-peek 1 1)
(err/rt-test (make-semaphore-peek #f))
(err/rt-test (make-semaphore-peek (make-semaphore-peek (make-semaphore))))

;; ----------------------------------------
;; Channels

(arity-test make-channel 0 0)

(let ([c (make-channel)]
      [v 'nope])
  (test #f object-wait-multiple 0 c)
  (thread (lambda () (sleep SYNC-SLEEP-DELAY) (set! v (channel-get c))))
  (test (void) channel-put c 10)
  (sleep)
  (test 10 'thread-v v)
  (thread (lambda () (sleep SYNC-SLEEP-DELAY) (channel-put c 11)))
  (test #f object-wait-multiple 0 c)
  (test 11 object-wait-multiple #f c)
  (let ([p (make-channel-put-waitable c 45)])
    (thread (lambda () (sleep SYNC-SLEEP-DELAY) (set! v (object-wait-multiple #f c))))
    (test #f object-wait-multiple 0 p)
    (test p object-wait-multiple #f p)
    (test #f object-wait-multiple 0 p)
    (sleep)
    (test 45 'thread-v v))
  ;;;;; Make sure break/kill before action => break/kill only
  ;; get:
  (let ([try (lambda (break-thread)
	       (let ([t (thread (lambda ()
				  (set! v (channel-get c))))])
		 (test #t thread-running? t)
		 (sleep)
		 (test #t thread-running? t)
		 (test (void) break-thread t)
		 (test #f object-wait-multiple 0 (make-channel-put-waitable c 32))
		 (sleep)
		 (test #f thread-running? t)
		 (test 45 'old-v v)))])
    (try break-thread)
    (try kill-thread))
  ;; put:
  (let ([try (lambda (break-thread)
	       (let ([t (thread (lambda () (channel-put c 17)))])
		 (test #t thread-running? t)
		 (sleep)
		 (test #t thread-running? t)
		 (test (void) break-thread t)
		 (test #f object-wait-multiple 0 c)
		 (sleep)
		 (test #f thread-running? t)))])
    (try break-thread)
    (try kill-thread))
  ;; put in main thread:
  (let ([t (current-thread)])
    (thread (lambda () 
	      (sleep SYNC-SLEEP-DELAY) 
	      (break-thread t) 
	      (set! v (channel-get c)))))
  (test 77
	'broken
	(with-handlers ([exn:break? (lambda (x) 77)])
	  (object-wait-multiple #f (make-channel-put-waitable c 32))))
  (test 45 'old-v v)
  (channel-put c 89)
  (sleep)
  (test 89 'new-v v)
  ;; get in main thread:
  (let ([t (current-thread)])
    (thread (lambda () 
	      (sleep SYNC-SLEEP-DELAY) 
	      (break-thread t) 
	      (channel-put c 66))))
  (test 99
	'broken
	(with-handlers ([exn:break? (lambda (x) 99)])
	  (object-wait-multiple #f c)))
  (test 66 object-wait-multiple 0 c)

  ;;; Can't sync with self!
  (test #f object-wait-multiple 0 c (make-channel-put-waitable c 100))
  ;; Test cross sync:
  (let ([c2 (make-channel)]
	[ok-result? (lambda (r)
		      (or (eq? r 100) (object-waitable? r)))])
    (thread (lambda () (channel-put c2 (object-wait-multiple #f c (make-channel-put-waitable c 100)))))
    (thread (lambda () (channel-put c2 (object-wait-multiple #f c (make-channel-put-waitable c 100)))))
    (test #t ok-result? (channel-get c2))
    (test #t ok-result? (channel-get c2))))

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
    (test #f object-wait-multiple SYNC-SLEEP-DELAY set))
  
  (let* ([c (make-channel)]
	 [set (waitables->waitable-set s1 s2 c)])
    (test #f object-wait-multiple SYNC-SLEEP-DELAY set)
    (thread (lambda () (channel-put c 12)))
    (test 12 object-wait-multiple SYNC-SLEEP-DELAY set)
    (test #f object-wait-multiple SYNC-SLEEP-DELAY set)
    (let* ([p (make-channel-put-waitable c 85)]
	   [set (waitables->waitable-set s1 s2 p)])
      (test #f object-wait-multiple SYNC-SLEEP-DELAY set)
      (thread (lambda () (channel-get c)))
      (test p object-wait-multiple SYNC-SLEEP-DELAY set)
      (test #f object-wait-multiple SYNC-SLEEP-DELAY set))))

(test 77 object-wait-multiple 
      #f 
      (make-wrapped-waitable (make-semaphore) void) 
      (make-guard-waitable 
       (lambda () 
	 (waitables->waitable-set 
	  (make-semaphore) (make-semaphore) (make-semaphore) (make-semaphore) 
	  (make-semaphore) (make-semaphore) (make-semaphore) (make-semaphore) 
	  (let ([sema (make-semaphore 1)])
	    (make-wrapped-waitable sema (lambda (x)
					  (test sema values x)
					  77)))))))

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

(let ([c (make-channel)])
  (thread (lambda () (channel-put c 76)))
  (test 77 object-wait-multiple #f (make-wrapped-waitable c add1)))

(test 78 object-wait-multiple #f 
      (make-wrapped-waitable (waitables->waitable-set (make-semaphore 1) (make-semaphore 1))
			     (lambda (x) 78)))

;; ----------------------------------------
;; Nack waitables

(arity-test make-nack-guard-waitable 1 1)
(arity-test make-guard-waitable 1 1)

(err/rt-test (make-nack-guard-waitable 10))
(err/rt-test (make-nack-guard-waitable (lambda () 10)))
(err/rt-test (make-guard-waitable 10))
(err/rt-test (make-guard-waitable (lambda (x) 10)))

(let ([s (make-semaphore 1)]
      [nack-try-wait? (lambda (n)
			(let ([v (object-wait-multiple 0 n)])
			  (when v
			    (test #t void? v)
			    (test (void) object-wait-multiple #f n))
			  (and v #t)))])
  (test s object-wait-multiple #f (make-nack-guard-waitable (lambda (nack) s)))
  (test #f semaphore-try-wait? s)
  (semaphore-post s)
  (let ([v #f])
    (test #f object-wait-multiple 0
	  (make-nack-guard-waitable (lambda (nack) 
				      (set! v nack)
				      (make-semaphore))))
    (test #t nack-try-wait? v)
    (set! v #f)
    (test #f object-wait-multiple SYNC-SLEEP-DELAY
	  (make-nack-guard-waitable (lambda (nack) 
				      (set! v nack)
				      (make-semaphore))))
    (test #t nack-try-wait? v)
    (set! v #f)
    (test #f object-wait-multiple 0
	  (make-nack-guard-waitable (lambda (nack) 
				      (set! v nack)
				      (make-semaphore)))
	  (make-nack-guard-waitable (lambda (nack) 
				      (set! v nack)
				      (make-semaphore))))
    (test #t nack-try-wait? v)
    (set! v #f)
    (test #f object-wait-multiple SYNC-SLEEP-DELAY
	  (make-nack-guard-waitable (lambda (nack) 
				      (set! v nack)
				      (make-semaphore)))
	  (make-nack-guard-waitable (lambda (nack) 
				      (set! v nack)
				      (make-semaphore))))
    (test #t nack-try-wait? v)
    (set! v #f)
    (test #f object-wait-multiple SYNC-SLEEP-DELAY
	  (waitables->waitable-set 
	   (make-nack-guard-waitable (lambda (nack) 
				       (set! v nack)
				       (make-semaphore)))
	   (make-nack-guard-waitable (lambda (nack) 
				       (set! v nack)
				       (make-semaphore)))))
    (test #t nack-try-wait? v)
    (set! v #f)
    (test s object-wait-multiple 0
	  (make-nack-guard-waitable (lambda (nack) 
				      (set! v nack)
				      s)))
    (test #f nack-try-wait? v) ; ... but not an exception!
    (semaphore-post s)
    (set! v #f)
    (let loop ()
      (test s object-wait-multiple 0
	    (make-nack-guard-waitable (lambda (nack) 
					(set! v nack)
					(make-semaphore)))
	    s)
      (if v
	  (test #t nack-try-wait? v)
	  (begin  ; tried the 2nd first, so do test again
	    (semaphore-post s)
	    (loop))))
    (set! v #f)
    (let loop ()
      (err/rt-test (object-wait-multiple 0
					 (make-nack-guard-waitable (lambda (nack) 
								     (set! v nack)
								     (make-semaphore)))
					 (make-nack-guard-waitable (lambda (nack)
								     (/ 1 0))))
		   exn:application:divide-by-zero?)
      (if v
	  (test #t nack-try-wait? v)
	  (loop)))
    (set! v #f)
    (let loop ()
      (err/rt-test (object-wait-multiple 0
					 (make-nack-guard-waitable (lambda (nack)
								     (/ 10 0)))
					 (make-nack-guard-waitable (lambda (nack) 
								     (set! v nack)
								     (make-semaphore))))
		   exn:application:divide-by-zero?)
      (if v
	  (begin
	    (set! v #f)
	    (loop))
	  (test #t not v)))
    (set! v null)
    (test #f object-wait-multiple 0
	  (make-nack-guard-waitable (lambda (nack) 
				(set! v (cons nack v))
				(make-semaphore)))
	  (make-nack-guard-waitable (lambda (nack) 
				(set! v (cons nack v))
				(make-semaphore))))
    (test '(#t #t) map nack-try-wait? v)

    ;; Check that thread kill also implies nack:
    (set! v #f)
    (let* ([ready (make-semaphore)]
	   [t (thread (lambda ()
			(object-wait-multiple 
			 #f
			 (make-nack-guard-waitable
			  (lambda (nack)
			    (set! v nack)
			    (semaphore-post ready)
			    (make-semaphore))))))])
      (semaphore-wait ready)
      (kill-thread t)
      (test #t nack-try-wait? v))))
		       

(let ([s (make-semaphore 1)])
  (test s object-wait-multiple 0 (make-guard-waitable (lambda () s))))

(let ([v #f])
  (test #f object-wait-multiple 0
	(make-nack-guard-waitable
	 (lambda (nack)
	   (set! v nack)
	   (waitables->waitable-set (make-semaphore) (make-semaphore)))))
  (test (void) object-wait-multiple 0 v))

;; ----------------------------------------
;; Poll waitables

(arity-test make-poll-guard-waitable 1 1)

(err/rt-test (make-poll-guard-waitable 10))
(err/rt-test (make-poll-guard-waitable (lambda () 10)))

(let ([s (make-semaphore-peek (make-semaphore 1))])
  (test s object-wait-multiple 0 (make-poll-guard-waitable (lambda (poll?)
							     (test #t values poll?)
							     s)))
  (test s object-wait-multiple #f (make-poll-guard-waitable (lambda (poll?)
							      (test #f values poll?)
							      s)))
  (test s object-wait-multiple 0 (waitables->waitable-set
				  (make-poll-guard-waitable (lambda (poll?)
							      (test #t values poll?)
							      s))
				  (make-semaphore)))
  (test s object-wait-multiple #f (waitables->waitable-set
				   (make-poll-guard-waitable (lambda (poll?)
							       (test #f values poll?)
							       s))
				   (make-semaphore))))

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
	[gc-msecs (current-gc-milliseconds)]
	[real-msecs (current-milliseconds)])
    (go)
    (let ([took (/ (abs (- (current-process-milliseconds) msecs
			   (abs (- (current-gc-milliseconds) gc-msecs))))
		   1000.0)]
	  [real-took (/ (abs (- (current-milliseconds) real-msecs)) 1000.0)]
	  [boundary (/ SYNC-SLEEP-DELAY 6)])
      ;; Hack.
      ;; The following test isn't reliable, so only Matthew should see it.
      (when (regexp-match #rx"(mflatt)|(matthewf)" (find-system-path 'home-dir))
	(test busy? (lambda (a b c d) (> b c)) 'busy-wait? took boundary real-took)))))

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
;;  Thread suspend, resume, and dead waitables

(let ([d (thread-dead-waitable (thread void))])
  (test d object-wait-multiple #f d))

(let* ([sema (make-semaphore)]
       [t (thread (lambda () (semaphore-wait sema)))]
       [r (thread-resume-waitable t)]
       [s (thread-suspend-waitable t)])
  (test #f object-wait-multiple 0 t s)
  (test t object-wait-multiple 0 t s r)
  (test t object-wait-multiple 0 r)
  (thread-suspend t)
  (test t object-wait-multiple 0 r)
  (test t object-wait-multiple 0 s)
  (let* ([r (thread-resume-waitable t)]
	 [s (thread-suspend-waitable t)])
    (test #f object-wait-multiple 0 t r)
    (test t object-wait-multiple 0 t s r)
    (test t object-wait-multiple 0 s)
    (thread-resume t)
    (test t object-wait-multiple 0 s)
    (test t object-wait-multiple 0 r)
    (let* ([s (thread-suspend-waitable t)])
      (thread (lambda () (sleep SYNC-SLEEP-DELAY) (thread-suspend t)))
      (test #f object-wait-multiple 0 s)
      (test t object-wait-multiple #f s)
      (let* ([r (thread-resume-waitable t)]
	     [d (thread-dead-waitable t)])
	(thread (lambda () (sleep SYNC-SLEEP-DELAY) (thread-resume t)))
	(test #f object-wait-multiple 0 r)
	(test t object-wait-multiple #f r)

	(test #f object-wait-multiple 0 d)
	(semaphore-post sema)
	(test d object-wait-multiple #f d)
	(test t object-wait-multiple #f r)
	(test t object-wait-multiple #f s)
	(test #f object-wait-multiple 0 (thread-resume-waitable t))
	(test #f object-wait-multiple 0 (thread-suspend-waitable t))
	(test d thread-dead-waitable t)))))

;; ----------------------------------------
;;  Garbage collection

(define (check-threads-gcable label blocking-thunk)
  (let ([l (let loop ([n 20][die? #f])
	     (if (zero? n)
		 null
		 (cons (make-weak-box (thread (if die? void blocking-thunk)))
		       (loop (if die? n (sub1 n)) (not die?)))))]
	[sl (lambda ()
	      (let loop ([n 20])
		(unless (zero? n) (sleep) (loop (sub1 n)))))]
	[ok-done? (lambda (r) (< (car r) 10))])
    (test #t
	  ok-done?
	  (let loop ([tries 0][n 100])
	    (if (or (= tries 3) (< n 10))
		(list tries n label)
		(begin
		  (sl) (collect-garbage)
		  (loop (add1 tries)
			(apply + (map (lambda (b) (if (weak-box-value b) 1 0)) l)))))))))

(check-threads-gcable 'sema (lambda () (semaphore-wait (make-semaphore))))
(define (check/combine c)
  (check-threads-gcable 'semaw (lambda () (object-wait-multiple #f (c (make-semaphore)))))
  (check-threads-gcable 'semap (lambda () (object-wait-multiple #f (c (make-semaphore-peek (make-semaphore))))))
  (check-threads-gcable 'ch (lambda () (object-wait-multiple #f (c (make-channel)))))
  (check-threads-gcable 'chput (lambda () (object-wait-multiple #f (c (make-channel-put-waitable (make-channel) 10)))))
  (check-threads-gcable 'wrapped (lambda () (object-wait-multiple #f (c (make-wrapped-waitable (make-semaphore) void)))))
  (check-threads-gcable 'guard (lambda () (object-wait-multiple #f (c (make-guard-waitable (lambda () (make-semaphore)))))))
  (check-threads-gcable 'nack (lambda () (object-wait-multiple #f (c (make-nack-guard-waitable (lambda (nack) (make-semaphore)))))))
  (check-threads-gcable 'poll (lambda () (object-wait-multiple #f (c (make-poll-guard-waitable (lambda (poll?) (make-semaphore))))))))
(check/combine values)
(check/combine (lambda (x) (waitables->waitable-set x (make-semaphore))))
(check/combine (lambda (x) (waitables->waitable-set (make-semaphore) x)))
(check/combine (lambda (x) (waitables->waitable-set (make-semaphore) x)))

(check-threads-gcable 'nested (lambda () (call-in-nested-thread (lambda () (semaphore-wait (make-semaphore))))))
(check-threads-gcable 'suspended (lambda () (thread-suspend (current-thread))))
(check-threads-gcable 'nested-suspend (lambda () (call-in-nested-thread (lambda () (thread-suspend (current-thread))))))

(check-threads-gcable 'resume (lambda () (let ([t (thread (lambda () (sleep 10)))])
					   (thread-suspend t)
					   (object-wait-multiple #f (thread-resume-waitable t)))))
(check-threads-gcable 'suspend (lambda () (let ([t (thread (lambda () (semaphore-wait (make-semaphore))))])
					    (object-wait-multiple #f (thread-suspend-waitable t)))))
(check-threads-gcable 'suspend-self (lambda () (object-wait-multiple #f (thread-suspend-waitable (current-thread)))))

;; ----------------------------------------
;;  Fairness in wait selection

(let ([try (lambda (t1 t2 r min max)
	     (test #t 
		   < 
		   min
		   (let loop ([n 100][r-n 0])
		     (if (zero? n)
			 r-n
			 (loop (sub1 n) (+ r-n
					   (if (eq? r (object-wait-multiple #f t1 t2))
					       1
					       0)))))
		   max))])
  (let ([t1 (make-semaphore-peek (make-semaphore 1))]
	[t2 (make-semaphore-peek (make-semaphore 1))])
    (let-values ([(r w) (make-pipe)])
      (fprintf w "Hi!~n")
      ;; Between 20% and 80% is fair, and surely < 20% or > 80% is unlikely
      (try t1 t2 t1 20 80)
      (try t1 t2 t2 20 80)
      (try t1 w w 20 80)
      (try w t1 w 20 80)
      (try t1 (waitables->waitable-set t2 w) t1 10 50)
      (try t1 (waitables->waitable-set t2 w) w 10 50)
      (try (waitables->waitable-set t2 w) t1 w 10 50))))

;; ----------------------------------------
;;  No starvation, despite hack to increase throughput for
;;  semaphore-protected data structures:

(let ([s1 (make-semaphore)])
  (define t1
    (thread (lambda ()
	      (semaphore-wait s1)
	      (semaphore-post s1))))
  (let loop ()
    (sleep)
    (semaphore-post s1)
    (semaphore-wait s1)
    (when (thread-running? t1)
      (loop)))
  (test #t string? "No starvation - good!"))

(let ([s1 (make-semaphore)]
      [s2 (make-semaphore)])
  (define t1
    (thread (lambda ()
	      (semaphore-post (object-wait-multiple #f s1 s2)))))
  (define t2
    (thread (lambda ()
	      (semaphore-post (object-wait-multiple #f s1 s2)))))
  (let loop ()
    (sleep)
    (semaphore-post s1)
    (semaphore-wait s1)
    (semaphore-post s2)
    (semaphore-wait s2)
    (when (or (thread-running? t1)
	      (thread-running? t2))
      (loop)))
  (test #t string? "No starvation - good!"))

;; ----------------------------------------

(report-errs)
