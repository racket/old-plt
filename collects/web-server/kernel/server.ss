(module server mzscheme
  (require (lib "unitsig.ss")
           (lib "tcp-sig.ss" "net")
           "connection-manager.ss"
           )
  (provide server-config^ server^ server@)

  (define-signature server-config^
    (port max-waiting listen-ip initial-time-to-live serve-connection))

  (define-signature server^ (serve))

  (define server@
    (unit/sig server^
      (import (config : server-config^)
              net:tcp^)

      ;; serve: -> -> void
      ;; start the server and return a thunk to shut it down
      (define (serve)
        (let* ([server-custodian (make-custodian)])
          (start-connection-manager server-custodian)
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
                 (serve-connection
                  (new-connection config:initial-time-to-live
                                  ip op connection-cust #f))))
              (server-loop server-custodian listener)))))

      ;; serve-connection: connection -> void
      ;; respond to all requests on this connection
      (define (serve-connection conn)
        (let ([close? (config:serve-connection conn)])
          (cond
            [close? (kill-connection! conn)]
            [else
             (serve-connection conn)])))
      ))

  )

