(module server-kernel mzscheme
  (require (lib "url.ss" "net")
           "server-kernel-structs.ss"
           "connection-manager.ss"
           "request-parsing.ss"
           "response-encoding.ss"
           )
  
  (provide serve)
  
  (define config:max-waiting 20)
  (define config:listen-ip #f)
  (define time-to-live 60)
  (define timeouts-file-base 60)
  (define timeouts-file-per-byte 60)
  
  ; **************************************************

  (define myprint printf)
  
  ;; serve: -> -> void
  ;; start the server and return a thunk to shut it down
  (define (serve config:port)
    (let* ([server-custodian (make-custodian)]
           [shutdown-connection-manager (start-connection-manager server-custodian)])
      (parameterize ([current-custodian server-custodian])
        (let ([get-ports
               (let ([listener (tcp-listen config:port config:max-waiting #t config:listen-ip)])
                 (lambda () (tcp-accept listener)))])
          (thread
           (lambda ()
             (server-loop server-custodian get-ports)))))
      (lambda ()
        (custodian-shutdown-all server-custodian))))
  
  
  ;; server-loop: custodian (-> i-port o-port) -> void
  ;; start a thread to handle each incoming connection
  (define (server-loop server-custodian listener)
    (let ([connection-cust (make-custodian)])
      (parameterize ([current-custodian connection-cust])
        (let-values ([(ip op) (listener)])
          (thread
           (lambda ()
             (serve-connection (new-connection ip op connection-cust #f time-to-live))))
          (server-loop server-custodian listener)))))
  
  ;; serve-connection: connection -> void
  ;; respond to all requests on this connection
  (define (serve-connection conn)
    (myprint "serve-connection: ~n")
    (let ([close? (serve-response conn)])
      (cond
        [close? (kill-connection! conn)]
        [else
         (serve-connection conn)])))
  
  ; **************************************************
  (define-struct request-line (method uri-string major-version minor-version))
  (define-struct http-request (line uri headers close?))
  (define (request->method req)
    (request-line-method (http-request-line req)))
  
  ;; serve-response: connection -> boolean
  ;; read the request-line
  ;; read the request headers
  ;; determine whether this is the last response and respond
  (define (serve-response conn)
    (myprint "serve-response: ~n")
    (let ([ip (connection-i-port conn)])
      (let-values ([(method uri-string major-version minor-version)
                    (read-request-line ip)])
        (let* ([headers (read-headers ip)]
               [uri  (string->url
                      (bytes->string/utf-8 uri-string))])
          (let-values ([(host-ip client-ip) (tcp-addresses ip)])
            (let ([close? (close-connection? headers
                                             (string->number (bytes->string/utf-8 major-version))
                                             (string->number (bytes->string/utf-8 minor-version))
                                             client-ip host-ip)])
              (set-connection-close?! conn close?)
              (serve-http-response conn (make-http-request
                                         (make-request-line method uri-string major-version minor-version)
                                         uri headers close?))
              close?))))))
  
  ;; serve-http-response: connection http-request -> void
  ;; locate a meta-resource, load it and respond
  (define (serve-http-response conn req)
    (myprint "serve-http-response: made it here!~n")
    (let* ([path (string->path "hello.html")]
           [size (file-size path)])
      (adjust-timeout! conn (+ timeouts-file-base (* size timeouts-file-per-byte)))
      (output-file path size (request->method req) conn)))
  )
