(module second-servlet mzscheme
  (require "servlet.ss")

  (define (send-a-page title str)
    (send/suspend
     (lambda (k-url)
       `(html (head (title "Second Servlet"))
              (body
               (h1 ,title)
               "Click " (a ([href ,k-url]) "here ") "for the " ,str " page.")))))

  (send-a-page "First Page" "second")
  (send-a-page "Second Page" "last")

  (send/finish
   `(html (head (title "Second Servlet"))
          (body
           (h1 "Last Page")
           "All done now."))))
