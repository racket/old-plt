(module monitor-server mzscheme
  (require (lib "etc.ss")
           (lib "sendmail.ss" "net"))
  (provide monitor)
  
  (define OK-REGEXP (regexp "^HTTP/[0-9]*.[0-9]* 200"))
  
  ; monitor : str str [nat] [num] [num] -> doesn't
  (define monitor
    (opt-lambda (alert-email-address
                 server-name
                 [server-port 80]
                 [poll-frequency-seconds 3600]
                 [server-response-timeout-seconds 75])
      (let check-server ()
        (let* ([cust (make-custodian)]
               [blow-up-handler
                (lambda (exn)
                  (blow-up alert-email-address cust server-name server-port exn))])
          (parameterize ([current-custodian cust])
            ; more here - there is a race condition which could result in two emails
            (thread (lambda ()
                      (with-handlers ([void blow-up-handler])
                        (sleep server-response-timeout-seconds))
                      (blow-up-handler 'timeout)))
            (thread
             (lambda ()
               (with-handlers ([void blow-up-handler])
                 (let-values ([(in out) (tcp-connect server-name server-port)])
                   (fprintf out "HEAD / HTTP/1.0\r\n")
                   (fprintf out "Host: ~a\r\n\r\n" server-name)
                   (let ([line (read-line in)])
                     (unless (regexp-match OK-REGEXP line)
                       (send-email-alert
                        alert-email-address
                        server-name server-port
                        (list (format "The web server ~a:~a did not respond" server-name server-port)
                              "to a head request for its home page with an 'okay' result."
                              (format "Received: ~a" line)))))))
               (custodian-shutdown-all cust))))
          (sleep poll-frequency-seconds)
          (custodian-shutdown-all cust))
        (check-server))))
  
  ; blow-up : str custodian str nat exn -> doesn't
  (define (blow-up alert-address cust server-name server-port exn)
    (send-email-alert alert-address server-name server-port
                      (list (format "Attempting to send a head request to ~a:~a" server-name server-port)
                            "resulted in the following exception:"
                            ""
                            (format "~a" (if (exn? exn)
                                             (exn-message exn)
                                             exn))))
    (custodian-shutdown-all cust))
  
  ; send-email-alert : str str nat (listof str) -> void
  (define (send-email-alert alert-address server-name server-port message)
    (send-mail-message alert-address
                       (format "The server ~a:~a is not responding!" server-name server-port)
                       (list alert-address)
                       null
                       null
                       (append message '("" "Fix it ASAP!!!")))))
