

(load-relative "loadtest.ss")

(SECTION 'threads)

(define SLEEP-TIME 0.1)

(define t (thread (lambda () 8)))
(test #t thread? t)

(arity-test thread 1 1)
(err/rt-test (thread 5) type?)
(err/rt-test (thread (lambda (x) 8)) type?)
(arity-test thread? 1 1)

; Should be able to make an arbitrarily deep chain of custodians
; if only the first & last are accssible:
(test #t custodian?
      (let loop ([n 1000][c (current-custodian)])
	(if (zero? n)
	    c
	    (loop (sub1 n) (make-custodian c)))))

(define result 0)
(define th1 0)
(define set-ready
  (let ([s (make-semaphore 1)]
	[r #f])
    (lambda (v)
      (semaphore-wait s)
      (begin0
       r
       (set! r v)
       (semaphore-post s)))))
(define cm (make-custodian))
(define th2 (parameterize ([current-custodian cm])
              (thread 
	       (lambda ()
		 (let ([cm2 (make-custodian cm)])
		   (parameterize ([current-custodian cm2])
		      (set! th1 (thread 
				 (lambda ()
				   (let loop ()
				     (let ([r (set-ready #f)])
				       (sleep SLEEP-TIME)
				       (set! result (add1 result))
				       (when r (semaphore-post r)))
				     (loop)))))))))))
(define start result)
(let ([r (make-semaphore)])
  (set-ready r)
  (semaphore-wait r))
(test #f eq? start result)
(kill-thread th2)
(set! start result)
(let ([r (make-semaphore)])
  (set-ready r)
  (semaphore-wait r))
(test #f eq? start result)
(test #t thread-running? th1)
(custodian-shutdown-all cm)
(thread-wait th1)
(set! start result)
(test #f thread-running? th1)
(sleep SLEEP-TIME)
(test #t eq? start result)

(let ([kept-going? #f])
  (let ([c (make-custodian)])
    (parameterize ([current-custodian c])
     (thread-wait
      (thread
       (lambda ()
	 (custodian-shutdown-all c)
	 (set! kept-going? #t))))))
  (test #f 'kept-going-after-shutdown? kept-going?))

(err/rt-test (parameterize ([current-custodian cm]) (kill-thread (current-thread)))
	     exn:misc?)

(test #t custodian? cm)
(test #f custodian? 1)
(arity-test custodian? 1 1)

(arity-test custodian-shutdown-all 1 1)

(arity-test make-custodian 0 1)
(err/rt-test (make-custodian 0))

(test (void) kill-thread t)
(arity-test kill-thread 1 1)
(err/rt-test (kill-thread 5) type?)

(arity-test break-thread 1 1)
(err/rt-test (break-thread 5) type?)

(arity-test thread-wait 1 1)
(err/rt-test (thread-wait 5) type?)

(test #t thread-running? (current-thread))
(arity-test thread-running? 1 1)
(err/rt-test (thread-running? 5) type?)

(arity-test sleep 0 1)
(err/rt-test (sleep 'a) type?)
(err/rt-test (sleep 1+3i) type?)
(err/rt-test (sleep -1.0) type?)

(define s (make-semaphore 1))

(test #t semaphore? s)

(arity-test make-semaphore 0 1)
(err/rt-test (make-semaphore "a") type?)
(err/rt-test (make-semaphore -1) type?)
(err/rt-test (make-semaphore 1.0) type?)
(err/rt-test (make-semaphore (expt 2 64)) exn:application:mismatch?)
(arity-test semaphore? 1 1)

(define test-block
  (lambda (block? thunk)
    (let* ([hit? #f]
	   [t (parameterize ([current-custodian (make-custodian)])
		(thread (lambda () (thunk) (set! hit? #t))))])
      (sleep SLEEP-TIME)
      (begin0 (test block? 'nondeterministic-block-test (not hit?))
	      (kill-thread t)))))

(test #t semaphore-try-wait? s) 
(test #f semaphore-try-wait? s) 
(semaphore-post s) 
(test #t semaphore-try-wait? s) 
(test #f semaphore-try-wait? s) 
(semaphore-post s) 
(test-block #f (lambda () (semaphore-wait s)))
(test-block #t (lambda () (semaphore-wait s)))
(semaphore-post s) 
(test-block #f (lambda () (semaphore-wait/enable-break s)))
(test-block #t (lambda () (semaphore-wait/enable-break s)))

(arity-test semaphore-try-wait? 1 1)
(arity-test semaphore-wait 1 1)
(arity-test semaphore-post 1 1)

(define s (make-semaphore))
(define result 0)
(define t-loop
  (lambda (n m)
    (lambda ()
      (if (zero? n)
	  (begin
	    (set! result m)
	    (semaphore-post s))
	  (thread (t-loop (sub1 n) (add1 m)))))))
(thread (t-loop 25 1))
(semaphore-wait s)
(test 26 'thread-loop result)

; Make sure you can break a semaphore-wait:
(test 'ok
      'break-semaphore-wait
      (let* ([s1 (make-semaphore 0)]
	     [s2 (make-semaphore 0)]
	     [t (thread (lambda ()
			  (semaphore-post s1)
			  (with-handlers ([exn:break? (lambda (x) (semaphore-post s2))])
			    (semaphore-wait (make-semaphore 0)))))])
	(semaphore-wait s1)
	(sleep SLEEP-TIME)
	(break-thread t)
	(semaphore-wait s2)
	'ok))

; Make sure two waiters can be released
(test 'ok
      'double-semaphore-wait
      (let* ([s1 (make-semaphore 0)]
	     [s2 (make-semaphore 0)]
	     [go (lambda ()
		   (semaphore-post s2)
		   (semaphore-wait s1)
		   (semaphore-post s2))])
	(thread go) (thread go)
	(semaphore-wait s2) (semaphore-wait s2)
	(semaphore-post s1) (semaphore-post s1)
	(semaphore-wait s2) (semaphore-wait s2)
	'ok))

; Tests inspired by a question from David Tillman
(define (read-line/expire1 port expiration)
  (with-handlers ([exn:break? (lambda (exn) #f)])
    (let ([timer (thread (let ([id (current-thread)])
			   (lambda () 
			     (sleep expiration)
			     (break-thread id))))])
      (dynamic-wind
       void
       (lambda () (read-line port))
       (lambda () (kill-thread timer))))))
(define (read-line/expire2 port expiration)
  (let ([done (make-semaphore 0)]
	[result #f])
    (let ([t1 (thread (lambda () 
			(set! result (read-line port))
			(semaphore-post done)))]
	  [t2 (thread (lambda () 
			(sleep expiration)
			(semaphore-post done)))])
      (semaphore-wait done)
      (kill-thread t1)
      (kill-thread t2)
      result)))

(define (go read-line/expire)
  (define p (let ([c 0]
		  [nl-sleep? #f]
		  [nl? #f])
	      (make-input-port (lambda () 
				 (when nl-sleep?
				       (sleep 0.4)
				       (set! nl-sleep? #f))
				 (if nl?
				     (begin
				       (set! nl? #f)
				       #\newline)
				     (begin
				       (set! nl? #t)
				       (set! nl-sleep? #t)
				       (set! c (add1 c))
				       (integer->char c))))
			       (lambda ()
				 (when nl-sleep?
				       (sleep 0.4)
				       (set! nl-sleep? #f))
				 #t)
			       void)))
  (test #f read-line/expire p 0.2) ; should get char but not newline
  (test "" read-line/expire p 0.6)) ; picks up newline

(go read-line/expire1)
(go read-line/expire2)

;; Make sure queueing works, and check kill/wait interaction:
(let* ([s (make-semaphore)]
       [l null]
       [wait (lambda (who)
	       (thread
		(lambda ()
		  (semaphore-wait s)
		  (set! l (cons who l)))))]
       [pause (lambda () (sleep 0.01))])
  (wait 0) (pause)
  (wait 1) (pause)
  (wait 2)
  (pause)
  (test null 'queue l)
  (semaphore-post s) (pause)
  (test '(0) 'queue l)
  (semaphore-post s) (pause)
  (test '(1 0) 'queue l)
  (semaphore-post s) (pause)
  (test '(2 1 0) 'queue l)
  
  (set! l null)
  (wait 0) (pause)
  (let ([t (wait 1)])
    (pause)
    (wait 2)
    (pause)
    (test null 'queue l)
    (kill-thread t)
    (semaphore-post s) (pause)
    (test '(0) 'queue l)
    (semaphore-post s) (pause)
    (test '(2 0) 'queue l)
    (semaphore-post s) (pause)
    (test '(2 0) 'queue l)
    (wait 3) (pause)
    (test '(3 2 0) 'queue l)))

;; Nested threads
(test 5 call-in-nested-thread (lambda () 5))

(err/rt-test (call-in-nested-thread (lambda () (kill-thread (current-thread)))) exn:thread?)
(err/rt-test (call-in-nested-thread (lambda () ((error-escape-handler)))) exn:thread?)
(err/rt-test (call-in-nested-thread (lambda () (raise (box 5)))) box?)

(define c1 (make-custodian))
(define c2 (make-custodian))
(define c3 (make-custodian))
(define output-stream null)
(define (output v)
  (set! output-stream 
	(append output-stream (list v))))
(define (test-stream v)
  (test v 'output-stream output-stream))

(define (chain c)
  (set! output-stream null)
  
  (output 'os)
  (with-handlers ([void (lambda (x) x)])
    (call-in-nested-thread
     (lambda ()
       (output 'ms)
       (begin0
	(dynamic-wind
	 (lambda () (output 'mpre))
	 (lambda ()
	   (let ([t1 (current-thread)])
	     (call-in-nested-thread
	      (lambda ()
		(output 'is)
		(with-handlers ([void (lambda (x) 
					(if (exn:break? x)
					    (output 'ibreak)
					    (output 'iother))
					(raise x))])
		  (if (procedure? c)
		      (c t1)
		      (custodian-shutdown-all c)))
		(output 'ie)
		'inner-result)
	      c2)))
	 (lambda () (output 'mpost)))
	(output 'me)))
     c1)))

(test 'inner-result chain c3)
(test-stream '(os ms mpre is ie mpost me))

(test #t exn:thread? (chain c1))
(test-stream '(os ms mpre is ibreak))

(parameterize ([break-enabled #f])
  (test #t exn:thread? (chain c1))
  (test-stream '(os ms mpre is ie))
  (test (void) 'discard-break
	(with-handlers ([void void])
	  (break-enabled #t)
	  (sleep)
	  'not-void)))

(test #t exn:thread? (chain c2))
(test-stream '(os ms mpre is mpost))

(test #t exn:thread? (chain (lambda (t1) (kill-thread (current-thread)))))
(test-stream '(os ms mpre is mpost))

(test #t exn:application? (chain 'wrong))
(test-stream '(os ms mpre is iother mpost))

(test #t exn:break? (chain (let ([t (current-thread)]) (lambda (t1) (break-thread t)))))
(test-stream '(os ms mpre is ibreak mpost))

(test #t exn:thread? (chain (lambda (t1) (kill-thread t1))))
(test-stream '(os ms mpre is ibreak))

(parameterize ([break-enabled #f])
  (test #t exn:thread? (let ([t (current-thread)])
			 (chain (lambda (t1)
				  (custodian-shutdown-all c1)
				  (test #t thread-running? (current-thread))
				  (test #t thread-running? t)
				  (test #f thread-running? t1)))))
  (test-stream '(os ms mpre is ie))
  (test (void) 'discard-break
	(with-handlers ([void void])
	  (break-enabled #t)
	  (sleep)
	  'not-void)))

(err/rt-test (let/cc k (call-in-nested-thread (lambda () (k)))) exn:application:continuation?)
(err/rt-test (let/ec k (call-in-nested-thread (lambda () (k)))) exn:application:continuation?)
(err/rt-test ((call-in-nested-thread (lambda () (let/cc k k)))) exn:application:continuation?)
(err/rt-test ((call-in-nested-thread (lambda () (let/ec k k)))) exn:application:continuation?)

(err/rt-test (call-in-nested-thread 5))
(err/rt-test (call-in-nested-thread (lambda (x) 10)))
(err/rt-test (call-in-nested-thread (lambda () 10) 5))

(arity-test call-in-nested-thread 1 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test wait-multiple:

(let ([s (make-semaphore 1)]
      [s2 (make-semaphore 1)])
  (let ([w (list
	    (object-wait-multiple #f s s2)
	    (object-wait-multiple #f s s2))])
    (test #t 'both (or (equal? w (list s s2))
		       (equal? w (list s2 s))))
    (test #f semaphore-try-wait? s)
    (test #f semaphore-try-wait? s2)))

(let ([s (make-semaphore)]
      [s-t (make-semaphore)]
      [portnum (+ 40000 (random 100))]) ; so parallel tests work ok
  (let ([t (thread
	    (lambda ()
	      (object-wait-multiple #f s-t)))]
	[l (tcp-listen portnum 5 #t)]
	[orig-thread (current-thread)])
    (let-values ([(r w) (make-pipe)])
      
      (define (try-all-blocked* wait)
	(let ([v #f])
	  (let ([bt (thread
		     (lambda ()
		       (with-handlers ([exn:break? (lambda (x) (set! v 'break))])
			 (set! v (wait #f s t l r)))))])
	    (sleep 0.05) ;;;     <---------- race condition (that's unlikely to fail)
	    (break-thread bt)
	    (sleep 0.05) ;;;     <----------
	    )
	  (test 'break 'broken-wait v)))

      (define (try-all-blocked)
	(test #f object-wait-multiple 0.05 s t l r))

      (try-all-blocked* object-wait-multiple)
      (try-all-blocked* object-wait-multiple/enable-break)
      (parameterize ([break-enabled #f])
	(try-all-blocked* object-wait-multiple/enable-break))

      (display #\x w)
      (test r object-wait-multiple #f s t l r)
      (test r object-wait-multiple #f s t l r)
      (peek-char r)
      (test r object-wait-multiple #f s t l r)
      (read-char r)
      (try-all-blocked)
	  
      (semaphore-post s)
      (test s object-wait-multiple #f s t l r)
      (try-all-blocked)

      (semaphore-post s-t)
      (test t object-wait-multiple #f s t l r)
      (test t object-wait-multiple #f s t l r)

      (set! t (thread (lambda () (semaphore-wait (make-semaphore)))))

      (let-values ([(cr cw) (tcp-connect "localhost" portnum)])
	(test l object-wait-multiple #f s t l r)
	(test l object-wait-multiple #f s t l r)

	(let-values ([(sr sw) (tcp-accept l)])
	  (try-all-blocked)

	  (close-output-port w)
	  (test r object-wait-multiple #f s t l r)
	  (test r object-wait-multiple #f s t l r)

	  (set! r cr)
	  (try-all-blocked)

	  (display #\y sw)
	  (test cr object-wait-multiple #f s t l sr cr)
	  (read-char cr)
	  (try-all-blocked)
	  
	  (display #\z cw)
	  (test sr object-wait-multiple #f s t l sr cr)
	  (read-char sr)
	  (try-all-blocked)

	  (close-output-port sw)
	  (test cr object-wait-multiple #f s t l sr cr)
	  (test cr object-wait-multiple #f s t l sr cr)

	  (close-output-port cw)
	  (test sr object-wait-multiple #f s t l sr))))
    (tcp-close l)))

(report-errs)
