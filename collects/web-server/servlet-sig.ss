(module servlet-sig mzscheme
  (provide servlet^
           (struct response/full (code message seconds mime extras body))
           (struct request (method uri headers bindings host-ip client-ip)))
  (require (lib "unitsig.ss"))

  ; more here - delete this or provide backwards compatability
  ;(define-signature servlet-old^
  ;  (method uri headers bindings host-ip client-ip send/suspend send/finish adjust-timeout
  ;   (struct response/full (code message seconds mime extras body) -setters)))
  
  (define-signature servlet^
    (initial-request send/suspend send/finish adjust-timeout!))
  
  (define-struct response/full (code message seconds mime extras body))
  (define-struct request (method uri headers bindings host-ip client-ip)))
