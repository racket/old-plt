(module send mzscheme
  (provide send/forward/callback)
  (require (lib "servlet.ss" "web-server"))

  ;; replace-procedures : xexpr/callbacks? (xexpr/callbacks? -> xexpr?) -> xexpr
  ;; Change procedures to the send/suspend of a k-url
  (define (replace-procedures p-exp p->a)
    (cond
      ((list? p-exp) (map (lambda (p-e) (replace-procedures p-e p->a))
                          p-exp))
      ((procedure? p-exp) (p->a p-exp))
      (else p-exp)))

  ;; send/forward/callback : xexpr/callback? -> void
  ;; send/back a response with callbacks in it; send/suspend those callbacks.
  ;; Clear the continuation table first.
  (define (send/forward/callback p-exp)
    (let/cc k0
      (send/back
        (begin
          (let/ec k7
            (send/forward (lambda (k-url) (k7 #f))))
          (replace-procedures
            p-exp (lambda (proc)
                    (let/cc k1 (k0 (proc (send/suspend k1))))))))))

  )
