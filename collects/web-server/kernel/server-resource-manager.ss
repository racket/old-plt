(module server-resource-manager mzscheme
  (require (lib "unitsig.ss")
           (lib "contract.ss")
           (lib "async-channel.ss")
           "splay-tree.ss")

  (provide (struct exn:server-resource-manager:dead ())

           ;; for testing
           get-pool
           server-resource-pool-get
           server-resource-pool-access
           )

  (define-struct server-resource-manager (message-channel thread resource-counter))
  (define-struct server-resource (id expiration-time) (make-inspector))

  (provide/contract
   [struct server-resource ([id number?] [expiration-time number?])]
   [start-server-resource-manager (procedure? procedure? custodian? . -> . server-resource-manager?)]
   [new-server-resource ((server-resource-manager? number?) any? . ->* . (server-resource?))]
   [adjust-server-resource-timeout! (server-resource-manager? server-resource? number? . -> . void)]
   [kill-server-resource! (server-resource-manager? server-resource? . -> . void)]
   )

  (define-struct (exn:server-resource-manager:dead exn) ())

  ;; same?: server-resource server-resource -> boolean
  ;; determine if two server-resource structures represent the same server-resource
  (define (same? r-src1 r-src2)
    (and (= (server-resource-id r-src1)
            (server-resource-id r-src2))))

  ;; less?: server-resource server-resource -> boolean
  ;; determine if one server-resource structure is smaller than another
  (define (less? r-src1 r-src2)
    (cond
      [(< (server-resource-expiration-time r-src1)
          (server-resource-expiration-time r-src2)) #t]
      [(= (server-resource-expiration-time r-src1)
          (server-resource-expiration-time r-src2))
       (< (server-resource-id r-src1) (server-resource-id r-src2))]
      [else #f]))

  (define-values/invoke-unit/sig
   splay-tree^
   splay-tree@
   #f comparable^)

  (define server-resource-pool-get tree-key)
  (define server-resource-pool-access access)
  ;  (define connectionn-pool-fold-ascending fold-ascending)

  ; **************************************************
  (define-struct create-server-resource (new-id time-to-live args))
  (define-struct reset-timeout (resource time-to-live))
  (define-struct kill-server-resource (resource))
  (define-struct request-pool ())

  ;(define message-channel (make-channel))
  ;(define manager-thread #f)

  ;; resource-manager-dispatch: channel (alpha* -> beta) (beta ->) -> never
  ;; dispatch based on messages
  (define (resource-manager-dispatch message-channel make-a-new-one kill-it!)
    (let loop ([server-resource-pool '()]
               [msg-wrapper (channel-get message-channel)])
      (let ([resource-counter (car msg-wrapper)]
            [result-channel (cadr msg-wrapper)]
            [msg (caddr msg-wrapper)])
        (with-handlers ([exn? (lambda (the-exn)
                                (async-channel-put result-channel the-exn)
                                (loop server-resource-pool (channel-get message-channel)))])
          (cond
            [(request-pool? msg)
             (async-channel-put result-channel server-resource-pool)
             (loop server-resource-pool
                   (channel-get message-channel))]

            [(create-server-resource? msg)
             (let* ([new-id (create-server-resource-new-id msg)]
                    [time-to-live (create-server-resource-time-to-live msg)]
                    [args (create-server-resource-args msg)]
                    [new-r-src
                     (apply make-a-new-one
                            (cons new-id (cons (+ time-to-live
                            (current-seconds)) args)))]

                    [new-pool
                     (insert new-r-src
                             (kill-old-resources! resource-counter
                                                  kill-it! server-resource-pool))])
               (async-channel-put result-channel new-r-src)
               (loop new-pool
                     (channel-get message-channel)))]

            [(reset-timeout? msg)
             (let* ([r-src (reset-timeout-resource msg)]
                    [time-to-live (reset-timeout-time-to-live msg)]
                    [rest-tree (delete r-src server-resource-pool)])
               (reset-server-resource-timeout! r-src time-to-live)
               (let ([new-pool
                      (insert r-src
                              (kill-old-resources! resource-counter kill-it! server-resource-pool))])
                 (async-channel-put result-channel #f)
                 (loop new-pool
                       (channel-get message-channel))))]

            [(kill-server-resource? msg)
             (let* ([r-src (kill-server-resource-resource msg)]
                    [rest-tree (delete r-src server-resource-pool)]
                    [new-pool
                     (kill-old-resources! resource-counter kill-it! rest-tree)])
               (when r-src (kill-it! r-src))
               (async-channel-put result-channel #f)
               (loop new-pool
                     (channel-get message-channel)))])))))

  ;; reset-server-resource-timeout!: server-resource number -> void
  ;; reset the expiration time of a server-resource
  (define (reset-server-resource-timeout! r-src time-to-live)
    (set-server-resource-expiration-time! r-src (+ time-to-live (current-seconds))))


  ;; kill-old-resources!: (tree-of server-resource) (server-resource -> ) -> (tree-of server-resource)
  ;; split the tree along the current seconds and then kill everything in the smaller tree
  (define (kill-old-resources! resource-counter kill-it! pool)

    ;; choose an id greater than any current connection's id (i.e. add1 resource-counter)
    ;; kill anything with expiration time smaller than current-seconds. Ties are broken
    ;; by choice of id.
    (let ([test-r-src (make-server-resource resource-counter (current-seconds))])
      (let-values ([(oldies newbies) (split test-r-src pool)])
        (tree-for-each! kill-it! oldies)
        newbies)))

  ;; alive?: server-resource-manager -> boolean
  (define (alive? mgr)
    (unless (thread-running? (server-resource-manager-thread mgr))
      (raise
       (make-exn:server-resource-manager:dead
        "server resource manager is not running"
        (current-continuation-marks)))))

  ;; ********************************************************************************
  ;; ********************************************************************************
  ;; A helper for EXPORTS

  ;; call-method: server-resource-manager message -> (union server-resource void)
  (define (call-method mgr the-message)
    (alive? mgr)
    (let ([result-channel (make-async-channel)])
      (channel-put (server-resource-manager-message-channel mgr)
                   (list (server-resource-manager-resource-counter mgr)
                         result-channel the-message))
      (let ([result (async-channel-get result-channel)])
        (when (exn? result)
          (raise result))
        result)))

  ;; EXPORTS

  ;; start-server-resource-manager: (args -> server-resource) (server-resource ->)
  ;;                                custodian -> server-resource-manager
  ;; start a new server-resource-manager and return a server-resource-manager struct
  (define (start-server-resource-manager make-a-new-one kill-it! man-cust)
    (parameterize ([current-custodian man-cust])
      (let ([message-channel (make-channel)])
        (make-server-resource-manager
         message-channel
         (thread
          (lambda () (resource-manager-dispatch message-channel make-a-new-one kill-it!)))
         0))))

  ;; new-server-resource: server-resource-manager time-to-live args -> server-resource
  ;; add a new connection to the pool and return it
  (define (new-server-resource mgr time-to-live . args)
    (let ([new-id (server-resource-manager-resource-counter mgr)])
      (set-server-resource-manager-resource-counter!
       mgr
       (add1 (server-resource-manager-resource-counter mgr)))
      (call-method
       mgr
       (make-create-server-resource new-id time-to-live args))))

  ;; adjust-server-resource-timeout!: server-resource-manager server-resource number -> void
  ;; adjust the expiration time of an existing connection
  (define (adjust-server-resource-timeout! mgr r-src time-to-live)
    (call-method mgr (make-reset-timeout r-src time-to-live))
    (void))

  ;; kill-server-resource!: server-resource-manager server-resource -> void
  ;; remove a connection from the pool and kill it
  (define (kill-server-resource! mgr r-src)
    (call-method mgr (make-kill-server-resource r-src))
    (void))

  ;; get-pool: server-resource-manager -> (tree-of server-resource)
  ;; get the connection-pool for diagnostic purposes.
  (define (get-pool mgr)
    (call-method mgr (make-request-pool)))
  )
