
(unit/sig mzlib:thread^
  (import)

  #|
  t accepts a function, f, and creates a thread. It returns the thread and a
  function, g. When g is applied it passes it's argument to f, and evaluates
  the call of f in the time of the thread that was created. Calls to g do not
  block.
  |#
  
  (define consumer-thread
    (lambda (f)
      (unless (procedure? f) (raise-type-error 'consumer-thread "procedure" f))
      (let ([sema (make-semaphore 0)]
	    [protect (make-semaphore 1)]
	    [front-state null]
	    [back-state null])
	(values 
	 (thread
	  (letrec ([loop
		    (lambda ()
		      (semaphore-wait sema)
		      (let ([local-state
			     (begin
			       (semaphore-wait protect)
			       (if (null? back-state)
				   (let ([new-front (reverse front-state)])
				     (set! back-state (cdr new-front))
				     (set! front-state null)
				     (semaphore-post protect)
				     (car new-front))
				   (begin0
				    (car back-state)
				    (set! back-state (cdr back-state))
				    (semaphore-post protect))))])
			(apply f local-state))
		      (loop))])
	    loop))
	 (lambda new-state
	   (let ([num (length new-state)])
	     (unless (procedure-arity-includes? f num) 
		     (raise 
		      (make-exn:application:arity
		       (format "<procedure-from-consumer-thread>: consumer procedure arity is ~e; provided ~s argument~a"
			       (arity f) num (if (= 1 num) "" "s"))
		       ((debug-info-handler))
		       num
		       (arity f)))))
	   (semaphore-wait protect)
	   (set! front-state (cons new-state front-state))
	   (semaphore-post protect)
	   (semaphore-post sema))))))

  (define with-semaphore
    (lambda (s f)
      (semaphore-wait s)
      (f)
      (semaphore-post s)))

  (define semaphore-wait-multiple 
   (case-lambda
    [(semaphores) (semaphore-wait-multiple semaphores #f #f)]
    [(semaphores timeout) (semaphore-wait-multiple semaphores timeout #f)]
    [(semaphores timeout allow-break?)
     (let ([break-enabled? (or allow-break? (break-enabled))])
       (parameterize 
	([break-enabled #f])
	(for-each
	 (lambda (s)
	   (or (semaphore? s) 
	       (raise-type-error 'semaphore-wait-multiple "list of semaphores" semaphores)))
	 semaphores)
	(or (not timeout) (real? timeout) (>= timeout 0)
	    (raise-type-error 'semaphore-wait-multiple "positive real number" timeout))
	(let* ([result-l null]
	       [ok? #f]
	       [set-l (make-semaphore 1)]
	       [one-done (make-semaphore)]
	       [branch-handler (lambda ()
				 (let ([p (make-parameterization)])
				   (with-parameterization 
				    p
				    (lambda () 
				      (user-break-poll-handler (lambda () #f))
				      (break-enabled #f)))
				   p))]
	       [threads (parameterize ([parameterization-branch-handler
					branch-handler])
				      (let loop ([l semaphores])
					(if (null? l)
					    null
					    (cons (let ([s (car l)])
						    (thread (lambda ()
							      (let/ec 
							       k
							       (current-exception-handler k)
							       (semaphore-wait/enable-break s)
							       (with-semaphore 
								set-l
								(lambda () (set! result-l 
										 (cons s result-l))))
							       (semaphore-post one-done)))))
						  (loop (cdr l))))))]
	       [timer-thread (if timeout
				 (parameterize ([parameterization-branch-handler
						 branch-handler])
					       (thread (lambda () (sleep timeout) (semaphore-post one-done))))
				 #f)])
	  (dynamic-wind
	   void
	   (lambda ()
	     ; wait until someone is done
	     ((if break-enabled? semaphore-wait/enable-break semaphore-wait) one-done)
	     (set! ok? #t))
	   (lambda ()
	     ; tell everyone to stop
	     (for-each (lambda (th) (break-thread th)) threads)
	     (when timer-thread (break-thread timer-thread))
	     ; wait until everyone's done
	     (for-each thread-wait threads)
	     ; If more that too manay suceeded, repost to the extras
	     (let ([extras (if ok? 
			       (if (null? result-l) 
				   null 
				   (cdr result-l))
			       result-l)])
	       (for-each (lambda (s) (semaphore-post s)) extras))))
	  (if (null? result-l)
	      #f 
	      (car result-l)))))]))

)

