(module connection-manager mzscheme
  (require (lib "unitsig.ss")
           "server-kernel-structs.ss"
           "splay-tree.ss")
  
  (provide start-connection-manager
           new-connection
           adjust-timeout!
           kill-connection!
           exn:connection-manager:dead?
           
           ;; for testing
           connection-pool-get
           connection-pool-access
           connectionn-pool-fold-ascending
           get-pool
           connection->expiration-time
           connection->id)
  
  (define-struct (internal-connection connection) (expiration-time id) (make-inspector))
  (define connection->expiration-time internal-connection-expiration-time)
  (define connection->id internal-connection-expiration-time)
  
  (define connection-counter 0)
  
  ;; same?: connection connection -> boolean
  ;; determine if two connection structures represent the same connection
  (define (same? conn1 conn2)
    (and (= (internal-connection-id conn1)
            (internal-connection-id conn2))))
  
  ;; less?: connection connection -> boolean
  ;; determine if one connection structure is smaller than another
  (define (less? conn1 conn2)
    (cond
      [(< (internal-connection-expiration-time conn1)
          (internal-connection-expiration-time conn2)) #t]
      [(= (internal-connection-expiration-time conn1)
          (internal-connection-expiration-time conn2))
       (< (internal-connection-id conn1) (internal-connection-id conn2))]
      [else #f]))
  
  (define-values/invoke-unit/sig
   splay-tree^
   splay-tree@
   #f splay-tree-imports^)
  
  (define connection-pool-get tree-key)
  (define connection-pool-access access)
  (define connectionn-pool-fold-ascending fold-ascending)
  
  ; **************************************************
  (define-struct message (output-channel args))
  (define-struct (create-connection message) ())
  (define-struct (reset-timeout message) (connection))
  (define-struct (kill-connection message) (connection))
  (define-struct (request-pool message) ())
  
  (define message-channel (make-channel))
  (define manager-thread #f)
  
  ;; connection-manager: -> never
  ;; put all the connection logic in one place to conserve threads
  (define (connection-manager)
    (let loop ([connection-pool '()]
               [msg (channel-get message-channel)])
      (with-handlers ([exn? (lambda (the-exn)
                              (channel-put (message-output-channel msg) the-exn)
                              (loop connection-pool (channel-get message-channel)))])
        (cond
          [(request-pool? msg)
           (channel-put (message-output-channel msg) connection-pool)
           (loop connection-pool
                 (channel-get message-channel))]
          
          [(create-connection? msg)
           (let* ([new-conn (apply create-new-connection (message-args msg))]
                  [new-pool (insert new-conn (kill-old-connections! connection-pool))])
             (channel-put (message-output-channel msg) new-conn)
             (loop new-pool (channel-get message-channel)))]
          
          [(reset-timeout? msg)
           (let* ([conn (reset-timeout-connection msg)]
                  [rest-tree (delete conn connection-pool)])
             (apply reset-connection-timeout! `(,conn ,@(message-args msg)))
             (let ([new-pool (insert conn (kill-old-connections! rest-tree))])
               (channel-put (message-output-channel msg) #f)
               (loop new-pool (channel-get message-channel))))]
          
          [(kill-connection? msg)
           (let* ([conn (kill-connection-connection msg)]
                  [rest-tree (delete conn connection-pool)])
             (when conn (kill-connection!/internal conn))
             (let ([new-pool (kill-old-connections! rest-tree)])
               (channel-put (message-output-channel msg) #f)
               (loop new-pool
                     (channel-get message-channel))))]))))
  
  ;; create-new-connection: i-port o-port custodian number -> internal-connection
  ;; create a new connection with a unique connection-id
  (define (create-new-connection i-port o-port cust close? time-to-live)
    (make-internal-connection 
     i-port o-port cust close?
     (+ time-to-live (current-seconds))
     (begin
       (set! connection-counter (add1 connection-counter))
       connection-counter)))
  
  ;; reset-connection-timeout!: connection number -> void
  ;; reset the expiration time of a connection
  (define (reset-connection-timeout! conn time-to-live)
    (set-internal-connection-expiration-time! conn (+ time-to-live (current-seconds))))
  
  ;; kill-connection!: connection -> void
  (define (kill-connection!/internal conn)
    (custodian-shutdown-all (connection-custodian conn)))
  
  ;; kill-old-connections!: (tree-of connection) -> (tree-of connection)
  ;; split the tree along the current seconds and then custodian-shutdown-all the smaller tree
  (define (kill-old-connections! pool)
    
    ;; choose an id greater than any current connection's id (i.e. add1 connection-counter)
    ;; kill anything with expiration time smaller than current-seconds. Ties are broken
    ;; by choice of id.
    (let ([test-connection (make-internal-connection #f #f #f #f (current-seconds) (add1 connection-counter))])
      (let-values ([(oldies newbies) (split test-connection pool)])
        (tree-for-each! kill-connection!/internal oldies)
        newbies)))
  
  (define-struct (exn:connection-manager:dead exn) ())
  (define (alive?)
    (unless (and manager-thread (thread-running? manager-thread))
      (raise (make-exn:connection-manager:dead "connection manager is not running" (current-continuation-marks)))))
  
  ;; ********************************************************************************
  ;; ********************************************************************************
  ;; EXPORTS
  
  ;; start-connection-manager: custodian -> (->)
  ;; start the connection manager and return a shutdown thunk
  (define (start-connection-manager top-cust)
    (let ([man-cust (make-custodian top-cust)])
      (parameterize ([current-custodian man-cust])
        (set! manager-thread (thread connection-manager))
        (lambda () (custodian-shutdown-all man-cust)))))
    
  ;; new-connection: i-port o-port custodian number -> connection
  ;; add a new connection to the pool and return it
  (define (new-connection i-port o-port cust close? time-to-live)
    (alive?)
    (let ([result-channel (make-channel)])
      (channel-put message-channel
                   (make-create-connection 
                    result-channel
                    (list i-port o-port cust close? time-to-live)))
      (let ([result (channel-get result-channel)])
        (when (exn? result) (raise result))
        result)))
  
  ;; adjust-timeout!: connection number -> void
  ;; adjust the expiration time of an existing connection
  (define (adjust-timeout! conn time-to-live)
    (alive?)
    (let ([result-channel (make-channel)])
      (channel-put message-channel
                   (make-reset-timeout
                    result-channel
                    (list time-to-live) conn))
      (let ([result (channel-get result-channel)])
        (when (exn? result) (raise result)))))
  
  
  ;; kill-connection!: connection -> void
  ;; remove a connection from the pool and kill it
  (define (kill-connection! conn)
    (alive?)
    (let ([result-channel (make-channel)])
      (channel-put message-channel
                   (make-kill-connection
                    result-channel '() conn))
      (let ([result (channel-get result-channel)])
        (when (exn? result) (raise result)))))
  
  ;; get-pool: -> (tree-of connection)
  ;; get the connection-pool for diagnostic purposes.
  (define (get-pool)
    (alive?)
    (let ([result-channel (make-channel)])
      (channel-put message-channel
                   (make-request-pool result-channel '()))
      (let ([result (channel-get result-channel)])
        (when (exn? result) (raise result))
        result)))
  
  )