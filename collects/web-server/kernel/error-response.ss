(module error-response mzscheme
  (require (lib "xml.ss" "xml")
           "response.ss")

  (provide report-error)

  ;; This is rudimentary and will need to be replaced
  ;; This stuff will eventually have to be configurable by host

  ;; report-error: number connection method -> void
  ;; report an error to the browser based on the error code
  (define (report-error which-n conn meth)
    (let-values ([(short-mess message title headers) (error-code->message-parts which-n)])
      (output-response/method
       conn
       (make-response/full
        which-n short-mess headers (current-seconds) "text/html"
        (list (xexpr->string
               `(html (head (title ,title))
                      (body
                       (p ,message))))))
       meth)))

  ;; error-code->message-parts: number -> string string string (listof header)
  (define (error-code->message-parts which-n)
    (case which-n
      [(500) (values
              "Servlet didn't load"
              "There was an error when trying to load the servlet."
              "Servlet didn't load."
              '())]

      [(404) (values
              "File not found"
              "The file referred to by this url was not found"
              "File not found"
              '())]
      [(400) (values
              "Malformed Request"
              "There was an error when trying to parse the request"
              "Malformed Request"
              '())]))

  )
