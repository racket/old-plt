(module channel mzscheme
  (provide create-channel channel-put channel-get channel-get-available)
  (require (lib "thread.ss"))
  
  ; Channel = (make-channel Semaphore Semaphore (cons #f (listof TST)) (cons TST null))
  
  (define-struct channel (available mutex front rear))
  
  ; create-channel : -> Channel
  (define (create-channel)
    (let ([x (cons #f null)])
      (make-channel (make-semaphore 0) (make-semaphore 1) x x)))
  
  ; channel-put : Channel TST -> Void
  (define (channel-put c x)
    (with-semaphore
     (channel-mutex c)
     (lambda ()
       (let ([tail (cons x null)])
         (set-cdr! (channel-rear c) tail)
         (set-channel-rear! c tail))
       (semaphore-post (channel-available c)))))
  
  ; channel-get : Channel -> TST
  (define (channel-get c)
    (semaphore-wait (channel-available c))
    (dequeue c))
  
  ; channel-get-available : Channel (TST -> a) -> (U a Void)
  (define (channel-get-available c k)
    (when (semaphore-try-wait? (channel-available c))
      (dequeue c)))
  
  ; dequeue : Channel -> TST
  ; the "avaiable" semaphore must already have been decremented
  (define (dequeue c)
    (with-semaphore
     (channel-mutex c)
     (lambda ()
       (let* ([front (channel-front c)]
              [next (cdr front)]
              ; next can't be null, because (channel-available c) counts the items in the list
              [v (car next)])
         (set-channel-front! c next)
         (set-car! next #f)
         v)))))
