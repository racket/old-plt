(require "connection-manager.ss")

(define shutdown (start-connection-manager (current-custodian)))
(define test-cust (make-custodian (current-custodian)))

(and (empty? (get-pool))
     (empty? (get-pool)))

(define conn1 (new-connection 'ip 'op test-cust #f 60))
(define conn2 (new-connection 'ip 'op test-cust #f 60))
(define conn3 (new-connection 'ip 'op test-cust #f 60))
(define conn4 (new-connection 'ip 'op test-cust #f 60))
(define conn5 (new-connection 'ip 'op test-cust #f 60))
(define conn6 (new-connection 'ip 'op test-cust #f 60))
(define conn7 (new-connection 'ip 'op test-cust #f 60))
(define conn8 (new-connection 'ip 'op test-cust #f 60))

(let ([connection-pool (get-pool)])
  (and (eq? conn1 (connection-pool-get (connection-pool-access conn1 connection-pool)))
       (eq? conn2 (connection-pool-get (connection-pool-access conn2 connection-pool)))
       (eq? conn3 (connection-pool-get (connection-pool-access conn3 connection-pool)))
       (eq? conn4 (connection-pool-get (connection-pool-access conn4 connection-pool)))
       (eq? conn5 (connection-pool-get (connection-pool-access conn5 connection-pool)))
       (eq? conn6 (connection-pool-get (connection-pool-access conn6 connection-pool)))
       (eq? conn7 (connection-pool-get (connection-pool-access conn7 connection-pool)))
       (eq? conn8 (connection-pool-get (connection-pool-access conn8 connection-pool)))))


(let ([old-timestamp (connection->expiration-time conn1)]
      [expected-timestamp (+ 1000 (current-seconds))])
  (adjust-timeout! conn1 1000)
  (let ([new-timestamp (connection->expiration-time conn1)])
    (and (<= expected-timestamp new-timestamp)
         (<= new-timestamp (add1 expected-timestamp)))))

(kill-connection! conn7)
(kill-connection! conn8)

(let ([connection-pool (get-pool)])
  (and (not (eq? conn7 (connection-pool-get (connection-pool-access conn7 connection-pool))))
       (not (eq? conn8 (connection-pool-get (connection-pool-access conn8 connection-pool))))))

(adjust-timeout! conn1 3)
(adjust-timeout! conn2 3)
(adjust-timeout! conn3 3)
(adjust-timeout! conn4 3)
(adjust-timeout! conn5 3)
(adjust-timeout! conn6 3)

(sleep 4)

(kill-connection! conn1)
(null? (get-pool))
(shutdown)

(with-handlers ([exn:connection-manager:dead?
                 (lambda (the-exn) #t)])
  (get-pool)
  #f)

(with-handlers ([exn:connection-manager:dead?
                 (lambda (the-exn) #t)])
  (new-connection 'ip 'op test-cust #f 10)
  #f)

(with-handlers ([exn:connection-manager:dead?
                 (lambda (the-exn) #t)])
  (adjust-timeout! conn1 17)
  #f)

(with-handlers ([exn:connection-manager:dead?
                 (lambda (the-exn) #t)])
  (kill-connection! conn1)
  #f)

(define shutdown (start-connection-manager (current-custodian)))

(empty? (get-pool))

;; this should cause an error that gets trapped in the connection manager
;; and then propagated to the calling thread without stopping the connection manager
(string=? "+: expects type <number> as 1st argument, given: 'foo; other arguments were: "
          (substring
           (with-handlers ([exn? (lambda (the-exn)
                                   (exn-message the-exn))])
             (new-connection 'ip 'op test-cust #f 'foo))
           0 77))

;; check that it is still running
(and (empty? (get-pool)))

(define bogus-cust-conn (new-connection 'ip 'op 'foo #f 3))

(string=? "custodian-shutdown-all: expects argument of type <custodian>; given 'foo"
          (with-handlers ([exn? (lambda (the-exn)
                                  (exn-message the-exn))])
            (kill-connection! bogus-cust-conn)))

(eq? bogus-cust-conn (connection-pool-get (connection-pool-access bogus-cust-conn (get-pool))))

(sleep 4)

(string=? "custodian-shutdown-all: expects argument of type <custodian>; given 'foo"
          (with-handlers ([exn? (lambda (the-exn)
                                  (exn-message the-exn))])
            (new-connection 'ip 'op test-cust #f 60)))

(set-connection-custodian! bogus-cust-conn test-cust)
(kill-connection! bogus-cust-conn)
(empty? (get-pool))


