
(module cml mzscheme
  (require (lib "contract.ss"))

  (define (spawn thunk)
    (thread/suspend-to-kill thunk))

  (define (channel)
    (make-channel))

  (define (channel-recv-evt ch)
    ch)

  (define (channel-send-evt ch v)
    (convert-evt
     (channel-put-evt ch v)
     void))

  (define (thread-done-evt th)
    (thread-dead-evt th))
  
  (define (current-time)
    (current-inexact-milliseconds))
  (define (time-evt t)
    (alarm-evt t))

  (define (wrap-evt e p)
    (convert-evt e p))

  (provide/contract
   (spawn ((-> any) . -> . thread?))
   (channel (-> channel?))
   (channel-recv-evt (channel? . -> . evt?))
   (channel-send-evt (channel? any? . -> . evt?))
   
   (wrap-evt (evt? (any? . -> . any) . -> . evt?))

   (thread-done-evt (thread? . -> . evt?))
   (current-time (-> number?))
   (time-evt (real? . -> . evt?))))

