(module server-kernel mzscheme
  (require (lib "unitsig.ss")
           "server-kernel-structs.ss"
           "connection-manager.ss"
           "request-parsing.ss"
           "meta-server-cache.ss"
           )
  
  (provide serve server-kernel-config^)
  
  (define-signature server-kernel-config^ (max-waiting listen-ip port initial-time-to-live dispatch))
  ; **************************************************
  
  (define myprint printf)
  
  (define (serve config@)
    (invoke-unit/sig
     (compound-unit/sig
       (import)
       (link
        [CFG : server-kernel-config^ (config@)]
        [KRNL : () (server-kernel@ CFG)])
       (export))))                       
  
  (define server-kernel@
    (unit/sig ()
      (import [config : server-kernel-config^])
      
      ;; serve: -> -> void
      ;; start the server and return a thunk to shut it down
      (define (serve/internal)
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
                 (serve-connection (new-connection ip op connection-cust #f config:initial-time-to-live))))
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
      
      ;; serve-response: connection -> boolean
      ;; read the request-line and dispatch the the appropriate meta-server
      (define (serve-response conn)
        (let ([ip (connection-i-port conn)])
          (let-values ([(method uri-string major-version minor-version)
                        (read-request-line ip)])
            (let ([req-line (make-request-line method uri-string major-version minor-version)]
                  [meta-server-path (config:dispatch uri-string)])
              (cond
                [(cached? meta-server-path)
                 => (lambda (meta-serve)
                      (meta-serve conn req-line))]
                [else
                 ((load-meta-server meta-server-path) conn req-line)])))))
      
      ;; load-meta-server: path -> (connection request-line -> boolean)
      ;; load the specified meta-server and cache it
      (define (load-meta-server meta-server-path)
        ;; TODO: create an appropriate exception for file-not-found
        (when (not (file-exists? meta-server-path))
          (error "file-not-found"))
        (let* ([module-name `(file ,(path->string meta-server-path))]
               [meta-serve (dynamic-require module-name 'meta-serve)])
          (cache! meta-server-path meta-serve)
          meta-serve))
      
      (serve/internal))
    )
  )
