(module servlet mzscheme
  (require (lib "url.ss" "net")
           "response-encoding.ss")
  
  (provide send/back send/forward send/finish send/suspend
           current-servlet-context
           update-params
           (struct http-request (line uri headers close?))
           (struct servlet-context (instance connection request suspend))
           (struct servlet-instance (invoke-id k-table next-k-id)))

  (define-struct servlet-instance (invoke-id k-table next-k-id) (make-inspector))
  (define-struct servlet-context (instance connection request suspend)
    (make-inspector))

  ;; perhaps this should be in a data-def  module:
  (define-struct http-request (line uri headers close?))


  ;; ********************************************************************************
  ;; The current-servlet-context parameter
  (define current-servlet-context (make-parameter #f))

  ;; clear-continuations! -> void
  ;; replace the k-table for the current servlet-instance
  (define (clear-continuations!)
    (set-servlet-instance-k-table!
     (servlet-context-instance
      (current-servlet-context))
     (make-hash-table)))

  ;; store-continuation!: continuation -> url-string
  ;; store a continuation in the k-table for the current servlet-instance
  (define (store-continuation! k)
    (let* ([ctxt  (current-servlet-context)]
           [inst (servlet-context-instance ctxt)]
           [next-k-id (servlet-instance-next-k-id inst)]
           [k-table (servlet-instance-k-table inst)])
      (set-servlet-instance-next-k-id! inst (add1 next-k-id))
      (hash-table-put! k-table next-k-id k)
      (update-params
       (http-request-uri (servlet-context-request ctxt))
       (format "~a*~a" (servlet-instance-invoke-id inst)  next-k-id))))

  ;; update-params: url string -> string
  ;; replace the path/param part of a url with a new one
  ;; (assumes that there is only one path/param)
  (define (update-params a-url new-param)
    (let ([new-path
           (let update-path ([pth (url-path a-url)])
             (cond
              [(null? pth)
               (list (make-path/param "" new-param))]                      
              [(path/param? (car pth))
               (cons (if new-param
                         (make-path/param
                          (path/param-path (car pth))
                          new-param)
                         (path/param-path (car pth)))
                     (cdr pth))]
              [else (cons (car pth)
                          (update-path (cdr pth)))]))])
      (url->string
       (make-url
        (url-scheme a-url)
        (url-user a-url)
        (url-host a-url)
        (url-port a-url)
        new-path
        (url-query a-url)
        (url-fragment a-url)))))
  
  ;; **************************************************
  ;; send/*

  ;; send/back: response -> void
  ;; send a response and don't clear the continuation table
  (define (send/back resp)
    (output-page/port
     (servlet-context-connection (current-servlet-context))
     resp))
  
  ;; send/finish: response -> void
  ;; send a response and clear the continuation table
  (define (send/finish resp)
    (clear-continuations!)
    (send/back resp))

  ;; send/suspend: (url -> response) -> request
  ;; send a response and apply the continuation to the next request
  (define (send/suspend response-generator)
    (let/cc k
      (output-page/port
       (servlet-context-connection (current-servlet-context))
       (response-generator (store-continuation! k)))
      ((servlet-context-suspend (current-servlet-context)))))
    

  ;; send/forward: (url -> response) -> request
  ;; clear the continuation table, then behave like send/suspend
  (define (send/forward response-generator)
    (clear-continuations!)
    (send/suspend response-generator))

  )
     
  
