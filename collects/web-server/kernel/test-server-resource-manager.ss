(module test-server-resource-manager mzscheme
  (require "test-harness.ss"
           "server-resource-manager.ss")

  (define manager-custodian (make-custodian (current-custodian)))

  (define the-manager
    (start-server-resource-manager make-server-resource void manager-custodian))

  (test "trivial get-pool test"
        (lambda ()
          (and (null? (get-pool the-manager))
               (null? (get-pool the-manager)))))

  (define res1 (new-server-resource the-manager 60))
  (define res2 (new-server-resource the-manager 60))
  (define res3 (new-server-resource the-manager 60))
  (define res4 (new-server-resource the-manager 60))
  (define res5 (new-server-resource the-manager 60))
  (define res6 (new-server-resource the-manager 60))
  (define res7 (new-server-resource the-manager 60))
  (define res8 (new-server-resource the-manager 60))

  (test "create 8 resources"
        (lambda ()
          (let ([server-resource-pool (get-pool the-manager)])
            (and
             (eq? res1 (server-resource-pool-get
                        (server-resource-pool-access res1 server-resource-pool)))
             (eq? res2 (server-resource-pool-get
                        (server-resource-pool-access res2 server-resource-pool)))
             (eq? res3 (server-resource-pool-get
                        (server-resource-pool-access res3 server-resource-pool)))
             (eq? res4 (server-resource-pool-get
                        (server-resource-pool-access res4 server-resource-pool)))
             (eq? res5 (server-resource-pool-get
                        (server-resource-pool-access res5 server-resource-pool)))
             (eq? res6 (server-resource-pool-get
                        (server-resource-pool-access res6 server-resource-pool)))
             (eq? res7 (server-resource-pool-get
                        (server-resource-pool-access res7 server-resource-pool)))
             (eq? res8 (server-resource-pool-get
                        (server-resource-pool-access res8 server-resource-pool)))
             ))))

  (test "adjust-server-resource-timeout! test"
        (lambda ()
          (let ([old-timestamp (server-resource-expiration-time res1)]
                [expected-timestamp (+ 1000 (current-seconds))])
            (adjust-server-resource-timeout! the-manager res1 1000)
            (let ([new-timestamp (server-resource-expiration-time res1)])
              (and (<= expected-timestamp new-timestamp)
                   (<= new-timestamp (add1 expected-timestamp)))))))

  (kill-server-resource! the-manager res7)
  (kill-server-resource! the-manager res8)

  (test "kill-server-resource! test"
        (lambda ()
          (let ([connection-pool (get-pool the-manager)])
            (and (not (eq? res7 (server-resource-pool-get
                                 (server-resource-pool-access res7 connection-pool))))
                 (not (eq? res8 (server-resource-pool-get
                                 (server-resource-pool-access res8 connection-pool))))))))

  (adjust-server-resource-timeout! the-manager res1 3)
  (adjust-server-resource-timeout! the-manager res2 3)
  (adjust-server-resource-timeout! the-manager res3 3)
  (adjust-server-resource-timeout! the-manager res4 3)
  (adjust-server-resource-timeout! the-manager res5 3)
  (adjust-server-resource-timeout! the-manager res6 3)

  (sleep 4)

  (kill-server-resource! the-manager res1)
  (null? (get-pool the-manager))
  (custodian-shutdown-all manager-custodian)

  (test "check with a dead manager 01"
        (lambda ()
          (with-handlers ([exn:server-resource-manager:dead?
                           (lambda (the-exn) #t)])
            (get-pool the-manager)
            #f)))

  (test "check with a dead manager 02"
        (lambda ()
          (with-handlers ([exn:server-resource-manager:dead?
                           (lambda (the-exn) #t)])
            (new-server-resource the-manager 60)
            #f)))

  (test "check with a dead manager 03"
        (lambda ()
          (with-handlers ([exn:server-resource-manager:dead?
                           (lambda (the-exn) #t)])
            (adjust-server-resource-timeout! the-manager res1 17)
            #f)))

  (test "check with a dead manager 04"
        (lambda ()
          (with-handlers ([exn:server-resource-manager:dead?
                           (lambda (the-exn) #t)])
            (kill-server-resource! the-manager res1)
            #f)))

  ;; the following test is harder to make now that I have contracts
  ;; on the server-resource-manager interface
  ;;
;(define another-custodian (make-custodian (current-custodian)))
;(define another-manager
;  (start-server-resource-manager make-server-resource void another-custodian))
;
;(empty? (get-pool another-manager))
;
;;; this should cause an error that gets trapped in the connection manager
;;; and then propagated to the calling thread without stopping the connection manager
  ;;(string=? "+: expects type <number> as 1st argument, given: 'foo; other arguments were: "
  ;;          (substring
;           (with-handlers ([exn? (lambda (the-exn)
;                                   (exn-message the-exn))])
;             (new-server-resource another-manager 'foo))
  ;;           0 77))
;
;;; check that it is still running
;(and (empty? (get-pool another-manager)))
;
;(custodian-shutdown-all another-custodian)


  )
