(module acknowledge mzscheme
  (require (lib "servlet-helpers.ss" "web-server")
           (lib "acks.ss" "drscheme")
           (lib "servlet.ss" "web-server"))
  
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define (start initial-request)
    (report-errors-to-browser send/finish)
    `(HTML 
      (TITLE "Acknowledgements")
      (BODY 
       (A ((NAME "acknowledgements") (VALUE "acknowledgements")))
       (H1  "Acknowledgements")
       (P)
       ,(get-general-acks)
       (P)
       ,(get-translating-acks)))))
