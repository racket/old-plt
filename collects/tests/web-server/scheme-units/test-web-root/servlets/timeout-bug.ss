;; Reproduce a bug where the timeout is killing all connections.
(module timeout-bug mzscheme
  (require (lib "servlet.ss" "web-server")
           )

  (provide start timeout interface-version)

  (define timeout 5)

  (define interface-version 'v1)

  (define (start req)
    (send/suspend
      (lambda (k-url)
        `(html (title "test")
               (a ((href ,k-url)) "Next"))))
    (sleep 20)
    (send/suspend
      (lambda (k-url)
        '(html (title "second test")
               "Won't get here"))))

  )
