
(module cml mzscheme
  (require (lib "contract.ss"))

  (define (spawn thunk)
    (thread/suspend-to-kill thunk))

  (define (sync w)
    (object-wait-multiple #f w))

  (define (channel)
    (make-channel))

  (define (channel-recv-evt ch)
    ch)

  (define (channel-send-evt ch v)
    (make-wrapped-waitable
     (make-channel-put-waitable ch v)
     void))

  (define (choice-evt . l)
    (apply waitables->waitable-set l))

  (define (wrap-evt w proc)
    (make-wrapped-waitable w proc))

  (define (guard-evt proc)
    (make-guard-waitable proc))

  (define (nack-guard-evt proc)
    (make-nack-guard-waitable proc))

  
  (define (thread-done-evt th)
    (thread-dead-waitable th))
  
  (define (current-time)
    (current-seconds))
  (define (time-evt t)
    (make-nack-guard-waitable
     (lambda (nack)
       (let ([s (make-semaphore)])
	 (thread-resume (thread/suspend-to-kill
			 (lambda ()
			   (object-wait-multiple (max 0 (- t (current-seconds)))
						 nack)
			   (semaphore-post s)))
			(current-thread))
	 (make-wrapped-waitable s void)))))

  (provide/contract
   (spawn ((-> any) . -> . thread?))
   (sync (object-waitable? . -> . any))
   (channel (-> channel?))
   (channel-recv-evt (channel? . -> . object-waitable?))
   (channel-send-evt (channel? any? . -> . object-waitable?))
   (choice-evt (() (listof object-waitable?) . ->* . (object-waitable?)))
   (wrap-evt (object-waitable? (any? . -> . any?) . -> . object-waitable?))
   (guard-evt ((-> any?) . -> . object-waitable?))
   (nack-guard-evt ((object-waitable? . -> . any?) . -> . object-waitable?))

   (thread-done-evt (thread? . -> . object-waitable?))
   (current-time (-> number?))
   (time-evt (number? . -> . object-waitable?))))

