(module channel mzscheme
  (provide make-async-channel
	   async-channel-put
	   async-channel-get
	   async-channel-get-available
	   async-channel-try-get) ;; BEWARE - not the same as in (lib "async-channel.ss")!
  (require (all-except (lib "async-channel.ss") async-channel-try-get))
  
  ;; channel-get-available : Channel (TST -> a) -> a
  (define (channel-try c k fail-k)
    ;; Can't use channel-try-get because we might need
    ;; to distinguish #f as result from #f as failure.
    (let ([v (object-wait-multiple 0 (make-wrapped-waitable c (lambda (x) (box x))))])
      (if v
          (k (unbox v))
          (fail-k))))

  ; channel-get-available : Channel (TST -> a) -> a
  (define (async-channel-get-available c k)
    (channel-try c k void))

  ; channel-try-get : Channel ( -> TST) -> TST
  (define (async-channel-try-get c fail)
    (channel-try c values fail)))
