(module thread-across-connections mzscheme
  (require (lib "servlet.ss" "web-server")
           "a-thread.ss")

  (provide timeout interface-version start)

  (define timeout 30)

  (define interface-version 'v1)

  (define (start req)
    (send/suspend (lambda (k-url) `(form ((action ,k-url))
                                         (input ((type "submit"))))))
    (send/finish
      (if (thread-running? the-thread)
        '(p "Okay")
        '(p "Not okay"))))

  )
