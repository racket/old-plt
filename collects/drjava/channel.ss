(unit/sig channel^
  (import queue^)
  
  ;; Channel(a) = (make-canal Queue(a) Semaphore)
  ;; where the count on the semephore + n <= the length of the queue,
  ;;   with n being the # of threads in channel-get between semaphore-wait and deq!
  ;;   Also, mutex is held when q is updated.
  (define-struct canal (q sem mutex))
  
  ;; make-channel : -> Channel(a)
  (define (make-channel)
    (make-canal (mtq) (make-semaphore) (make-semaphore 1)))
  
  ;; channel-get : Channel(a) -> a
  (define (channel-get channel)
    (semaphore-wait (canal-sem channel))
    (with-mutex (canal-mutex channel) (lambda () (deq! (canal-q channel)))))
  
  ;; channel-put : Channel(a) a -> Void
  (define (channel-put channel value)
    (with-mutex (canal-mutex channel) (lambda () (enq! value (canal-q channel))))
    (semaphore-post (canal-sem channel)))
  
  ;; with-mutex : Semaphore (-> a) -> a
  (define (with-mutex mutex thunk)
    (semaphore-wait mutex)
    (begin0
      (thunk)
      (semaphore-post mutex))))