(module test-connection-manager mzscheme
  (require "test-harness.ss"
           "connection-manager.ss")


  ;; ********************************************************************************
  ;; test whether a connection thread can kill itself
  ;; must be able to send the kill connection message
  ;; from the same connection thread without stalling the connection
  ;; manager thread.

  (define top-cust (current-custodian))

  (start-connection-manager top-cust)

  ;; create-connection: -> (union connection #f)
  ;; if nothin exciting happens in 3 seconds return #f
  (define (create-connection)
    (let ([cust (make-custodian)]
          [result-channel (make-channel)])
      (parameterize ([current-custodian cust])
        (let ([i-port (open-input-string "foo")]
              [o-port (open-output-string)])
          (thread
           (lambda ()
             (channel-put result-channel
                          (new-connection 300 i-port o-port cust #f))))))
      (thread
       (lambda ()
         (sleep 3)
         (channel-put result-channel #f)))
      (channel-get result-channel)))

  ;; create-and-kill-connection: -> void
  ;; create a connection and kill from the connection thread
  (define (create-and-kill-connection)
    (let ([cust (make-custodian)])
      (parameterize ([current-custodian cust])
        (let ([i-port (open-input-string "foo")]
              [o-port (open-output-string)])
          (thread
           (lambda ()
             (let ([conn (new-connection 300 i-port o-port cust #f)])
               (kill-connection! conn))))))))

  (define conn1 (create-connection))

  (test "create connection"
        (lambda () (connection? conn1)))

  ;(kill-connection! conn1)

  (create-and-kill-connection)
  (sleep 3)

  (define conn2 (create-connection))

  (test "create-connection (after a kill)"
        (lambda ()
          (printf "conn2 = ~a~n" conn2)
          (connection? conn2))))

