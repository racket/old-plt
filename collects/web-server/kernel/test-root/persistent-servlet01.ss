(module persistent-servlet01 mzscheme
  (require "../servlet.ss")

  (provide-servlet-entry (start initial-request)
    (send/stop/persistent
     the-other-page
     (lambda (purl)
       `(html (head (title "The Main Page"))
              (body (h1 "The Main Page")
                    (p (a ([href ,purl]) "Click here for the other page")))))))

  (provide-servlet-entry (the-other-page req)
    (send/back
     `(html (head (title "The Other Page"))
            (body (h1 "The Other Page")
                  (p "You have reached the other page")))))
  )

