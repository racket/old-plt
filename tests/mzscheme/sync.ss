

(load-relative "loadtest.ss")

(SECTION 'synchronization)

(define SYNC-SLEEP-DELAY 0.1)

(let ([s1 (make-semaphore)]
      [s2 (make-semaphore)]
      [s3 (make-semaphore)])
  (test #f object-wait-multiple SYNC-SLEEP-DELAY (make-waitable-set s1 s2 s3))
  (semaphore-post s2)
  (test s2 object-wait-multiple SYNC-SLEEP-DELAY (make-waitable-set s1 s2 s3))
  (test #f object-wait-multiple SYNC-SLEEP-DELAY (make-waitable-set s1 s2 s3))
  (let ([set (make-waitable-set s1 s2 s3)])
    (test #f object-wait-multiple SYNC-SLEEP-DELAY set)
    (semaphore-post s2)
    (test s2 object-wait-multiple SYNC-SLEEP-DELAY set)
    (test #f object-wait-multiple SYNC-SLEEP-DELAY set))
  (thread (lambda () (sleep) (semaphore-post s3)))
  (test s3 object-wait-multiple SYNC-SLEEP-DELAY (make-waitable-set s1 s2 s3))
  (test #f object-wait-multiple SYNC-SLEEP-DELAY (make-waitable-set s1 s2 s3))
  (semaphore-post s3)
  (test s3 object-wait-multiple SYNC-SLEEP-DELAY s1 (make-waitable-set s2 s3))
  (test #f object-wait-multiple SYNC-SLEEP-DELAY s1 (make-waitable-set s2 s3))
  (semaphore-post s3)
  (test s3 object-wait-multiple SYNC-SLEEP-DELAY (make-waitable-set s1 s2) s3)
  (test #f object-wait-multiple SYNC-SLEEP-DELAY (make-waitable-set s1 s2) s3)
  (let ([set (make-waitable-set s1 s2)])
    (test #f object-wait-multiple SYNC-SLEEP-DELAY s1 set s3)
    (semaphore-post s2)
    (test s2 object-wait-multiple SYNC-SLEEP-DELAY set s3)
    (test #f object-wait-multiple SYNC-SLEEP-DELAY set s3))
  (test #f object-wait-multiple SYNC-SLEEP-DELAY (make-waitable-set s1 (make-waitable-set s2 s3)))
  (semaphore-post s3)
  (test s3 object-wait-multiple SYNC-SLEEP-DELAY (make-waitable-set s1 (make-waitable-set s2 s3)))
  (test #f object-wait-multiple SYNC-SLEEP-DELAY (make-waitable-set s1 (make-waitable-set s2 s3)))
  (semaphore-post s3)
  (test s3 object-wait-multiple SYNC-SLEEP-DELAY (make-waitable-set (make-waitable-set s1 s2) s3))
  (test #f object-wait-multiple SYNC-SLEEP-DELAY (make-waitable-set (make-waitable-set s1 s2) s3))
  (let ([set (make-waitable-set s1 (make-waitable-set s2 s3))])
    (test #f object-wait-multiple SYNC-SLEEP-DELAY set)
    (semaphore-post s3)
    (test s3 object-wait-multiple SYNC-SLEEP-DELAY set)
    (test #f object-wait-multiple SYNC-SLEEP-DELAY set)))

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

;; This test only works well if there are no other
;;  threads running and the underlying OS is not loaded.
(define (check-busy-wait go busy?)
  (collect-garbage) ; reduces false-positives in detecting busy wait
  (let ([msecs (current-process-milliseconds)])
    (go)
    (test busy? 'is-busy-wait (> (/ (abs (- (current-process-milliseconds) msecs)) 1000.0)
				 (/ SYNC-SLEEP-DELAY 2)))))

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
(test-good-waitable (lambda (x) (make-waitable-set
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

(report-errs)
