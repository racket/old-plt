(module persistent-servlet02 mzscheme
  (require "../servlet.ss")

  (provide-servlet-entry (start initial-request)
    (send/persistent/dispatch
     `(html (head (title "The Main Page"))
            (body (h1 "The Main Page: persistent-servlet02.ss")
                  (p (a ([href ,(apply-callback callback-page "first arg"
                                                "second arg" "third arg")])
                        "Click here for the other page"))))))

  (provide-servlet-entry (callback-page req . args)
    (send/back
     `(html (head (title "Callback Page"))
            (body (h1 "Callback Page: persistent-servlet02.ss"))
            (p ,(format "You have reached the callback page. Arguments were: ~s"
                        args)))))
  )
