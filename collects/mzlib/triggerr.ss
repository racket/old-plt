
; Old-style MzScheme triggers implemented with semaphores

  (unit/sig
   mzlib:trigger^
   (import)
   (rename (make-trigger-x make-trigger))

   (define-struct trigger (ever? sem wait-sem num-waiting))
   
   (define (make-trigger-x)
     (make-trigger #f (make-semaphore 1) (make-semaphore 0) 0))

   (define (do-trigger-hit t)
     (let ([s (trigger-wait-sem t)])
       (set-trigger-ever?! t #t)
       (let loop ([n (trigger-num-waiting t)])
	 (unless (zero? n)
		 (semaphore-post s)
		 (loop (sub1 n))))
       (set-trigger-num-waiting! t 0)))

   (define (trigger-waiter t)
     (set-trigger-num-waiting! t (add1 (trigger-num-waiting t)))
     (trigger-wait-sem t))

   (define (trigger-hit t)
     (let ([s (trigger-sem t)])
       (semaphore-wait s)
       (do-trigger-hit t)
       (semaphore-post s)))

   (define (trigger-hit? t)
     (let ([s (trigger-sem t)])
       (semaphore-wait s)
       (begin0
	(trigger-ever? t)
	(semaphore-post s))))

   (define (trigger-test-and-hit? t)
     (let ([s (trigger-sem t)])
       (semaphore-wait s)
       (begin0
	(if (trigger-ever? t)
	    #f
	    (begin
	      (do-trigger-hit t)
	      #t))
	(semaphore-post s))))

   (define trigger-block 
     (case-lambda
      [(t) (trigger-block t #f)]
      [(t ever?)
       (let ([s (trigger-sem t)])
	 (semaphore-wait s)
	 (if (and ever? (trigger-ever? t))
	     (semaphore-post s)
	     (let ([ws (trigger-waiter t)]
		   [error? #t])
	       (semaphore-post s)
	       (dynamic-wind
		void
		(lambda ()
		  (semaphore-wait ws)
		  (set! error? #f))
		(lambda ()
		  ; If control escapes via break, we're not waiting on the
		  ; semaphore anymore
		  (when error?
			(semaphore-wait s)
			(set-trigger-num-waiting! 
			 t (sub1 (trigger-num-waiting t)))
			(semaphore-post s)))))))]))

   (define (trigger-callback t cb)
     (let ([s (trigger-sem t)])
       (semaphore-wait s)
       (semaphore-callback (trigger-waiter t) cb)
       (semaphore-post s)))

   (define (input-port-trigger p t)
     (let ([s (trigger-sem t)])
       (semaphore-wait s)
       (let ([s2 (make-semaphore 0)])
	 (input-port-post-semaphore p s2)
	 (semaphore-callback s2 (lambda () (trigger-hit t))))
       (semaphore-post s)))
   )
