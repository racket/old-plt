(module persistent-servlet03 mzscheme
  (require "../servlet.ss")

  ;; ********************************************************************************
  ;; a paper is some struct
  (define-struct paper (title author reviewer))

  (define the-papers
    (list (make-paper "Building Tractors" "John Deere" "Fred Flintstone")
          (make-paper "Eating Chocolate" "Joe Hershey" "Wilma Flintstone")
          (make-paper "Ruling" "Queen Elizebeth" "Barney Rubble")))

  ;; show-paper-page: string string string -> void
  (provide-servlet-entry (show-paper-page req title author reviewer)
    (send/back
     `(html (head (title ,title))
            (body
             (h1 ,title)
             (p "Author: " ,author)
             (p "Reviewer: " ,reviewer)))))

  ;; show-main-page: (listof paper) -> doesn't
  (define (show-main-page some-papers)
    (send/persistent/dispatch
     `(html (head (title "Some Papers"))
            (body
             (h1 "Some Papers:")
             ,@(map
                (lambda (a-paper)
                  `(p (a ([href ,(apply-callback show-paper-page
                                                 (paper-title a-paper)
                                                 (paper-author a-paper)
                                                 (paper-reviewer a-paper))])
                         ,(paper-title a-paper))))
                some-papers)))))

  (provide-servlet-entry (start initial-request)
    (show-main-page the-papers))
  )

