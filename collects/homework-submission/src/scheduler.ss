(module scheduler mzscheme
  (require (lib "etc.ss")
           (lib "async-channel.ss"))

  (provide schedule scheduler-thread)

  (define schedule-channel (make-channel))

  (define scheduler-thread
    (thread
     (lambda ()
       (let loop ()
         (let* ([thunk-and-channel (channel-get schedule-channel)]
                [thunk (car thunk-and-channel)]
                [result-channel (cdr thunk-and-channel)])
           (with-handlers ([(lambda (x) #t)
                            (lambda (the-exn)
                              (async-channel-put result-channel the-exn))])
             (async-channel-put result-channel (thunk)))
           (loop))))))

  ;; schedule: (-> alpha) -> alpha
  ;; schedule the execution of a thunk
  (define (schedule thunk)
    (let ([result-channel (make-async-channel)])
      (channel-put schedule-channel (cons thunk result-channel))
      (let ([res (async-channel-get result-channel)])
        (if (exn? res)
            (raise res)
            res)))))

