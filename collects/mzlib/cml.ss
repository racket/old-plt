
(module cml mzscheme
  (require (lib "contract.ss"))

  (define (spawn thunk)
    (thread/suspend-to-kill thunk))

  (define (channel)
    (make-channel))

  (define (channel-recv-evt ch)
    ch)

  (define (channel-send-evt ch v)
    (make-wrapped-waitable
     (make-channel-put-waitable ch v)
     void))

  (define (thread-done-evt th)
    (thread-dead-waitable th))
  
  (define (current-time)
    (current-inexact-milliseconds))
  (define (time-evt t)
    (alarm-evt t))

  (provide/contract
   (spawn ((-> any) . -> . thread?))
   (channel (-> channel?))
   (channel-recv-evt (channel? . -> . evt?))
   (channel-send-evt (channel? any? . -> . evt?))
   
   (thread-done-evt (thread? . -> . evt?))
   (current-time (-> number?))
   (time-evt (real? . -> . evt?))))

