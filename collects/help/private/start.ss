(module start mzscheme

  (provide post-start-semaphore
           wait-start-semaphore
           browser-timeout)

  (define browser-timeout 12)

  (define start-semaphore (make-semaphore 0))
  
  (define (post-start-semaphore) (semaphore-post start-semaphore))
  (define (wait-start-semaphore) (semaphore-wait start-semaphore)))

	