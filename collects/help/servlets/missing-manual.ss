(module missing-manual mzscheme
  (require (lib "servlet-sig.ss" "web-server")
           (lib "servlet-helpers.ss" "web-server"))
  
  (require "private/headelts.ss")
  (require "private/util.ss")
  
  (define doc-root "http://download.plt-scheme.org/doc")
  
  (define (no-manual manual label)
    (let* ([vno (version)]
           [html-url (format "~a/~a/html/~a/index.htm" 
                             doc-root vno manual)]
           [plt-url (format "~a/~a/bundles/~a-doc.plt" 
                            doc-root 
                            (if (cvs?) "pre-release" vno)
                            manual)])
      
      `(HTML
        (HEAD ,hd-css
              ,@hd-links 
              (TITLE "Missing PLT manual")) 
        (body ((bgcolor "white")) 
              ,(color-with "red" `(h1 "Documentation missing"))
              (P)
              "You tried to access documentation for "
              ,(color-with "blue" `(B ,label)) ". "
              "The documentation is not installed on this "
              "machine, probably because it is not part of the "
              "standard DrScheme distribution."
              (p)
              (h2 "To download and/or install the documentation")
              (p)
              (ul (li (a ((href ,plt-url)) "Click here")))
              ,@(if (cvs?)
                    '()
                    `((p)
                      (h2 "To read the documentation online")
                      (p)
                      (ul (li (a ((href ,html-url)) "Click here")))))))))
  
  (require (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define (start initial-request)
    (report-errors-to-browser send/finish)
    
    (let ([bindings (request-bindings initial-request)])
      (no-manual (extract-binding/single 'manual 
                                         bindings)
                 (extract-binding/single 'name
                                         bindings)))))
