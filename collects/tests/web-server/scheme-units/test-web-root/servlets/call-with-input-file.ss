;; A servlet that calls call-with-input-file.
(module call-with-input-file mzscheme
  (require (lib "servlet.ss" "web-server"))
  (provide timeout start interface-version)

  (define timeout 5)
  (define interface-version 'v1)

  (define (start req)
    (send/suspend
      (lambda (k-url)
        `(html (head (title "Test"))
               (body (a ((href ,k-url)) "Next")))))

    (send/finish
      `(html (head (title "Test"))
             (body (p ,(call-with-input-file
                         "/etc/passwd"
                         (lambda (ip)
                           (read-line ip))))))))

  )
