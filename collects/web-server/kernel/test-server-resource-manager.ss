(module test-server-resource-manager mzscheme
  (require (lib "async-channel.ss")
           "test-harness.ss"
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


  ;; ************************************************************
  ;; ************************************************************
  (define-struct (duvalaki server-resource) (cust) (make-inspector))

  (define confirm-channel (make-async-channel))

  (define (kill-duvalaki duv)
    (sleep 3)
    (async-channel-put confirm-channel 'goodbye)
    (custodian-shutdown-all (duvalaki-cust duv)))

  (define manager2-cust (make-custodian))

  (define manager2 (start-server-resource-manager make-duvalaki kill-duvalaki
                                                 manager2-cust))

  ;; create-duv: -> (union duvalaki #f)
  ;; if nothing exciting happens in 3 seconds return #f
  (define (create-duv)
    (let ([cust (make-custodian)]
          [result-channel (make-channel)])
      (parameterize ([current-custodian cust])
        (thread
         (lambda ()
           (channel-put result-channel
                        (new-server-resource manager2 300 cust))))
        (thread
         (lambda ()
           (sleep 3)
           (channel-put result-channel #f)))
        (channel-get result-channel))))

  (test "test for create-duv"
        (lambda ()
          (duvalaki? (create-duv))))

  ;; should return only after kill operation is complete
  (define duv1 (create-duv))

  (test "test return only after kill"
        (lambda ()
          (kill-server-resource! manager2 duv1)
          (eqv? 'goodbye (async-channel-try-get confirm-channel))))

  ;; should not stall manager thread if calling thread gets killed

  ;; create-and-kill-duvalaki: -> void
  ;; create a duvalaki and kill it from the duvalaki thread
  (define (create-and-kill-duvalaki)
    (let ([cust (make-custodian)])
      (parameterize ([current-custodian cust])
        (thread
         (lambda ()
           (let ([duv (new-server-resource manager2 300 cust)])
             (kill-server-resource! manager2 duv)))))))

  (create-and-kill-duvalaki)
  (test "check for dead after create-and-kill"
        (lambda ()
          (eqv? 'goodbye (async-channel-get confirm-channel))))

  (test "should still be able to create one"
        (lambda () (duvalaki? (create-duv))))

  ;; ********************************************************************************
  ;; some old stuff:

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
