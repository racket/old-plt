(module cookie mzscheme
  (require (lib "class.ss")
           (lib "contract.ss"))
  
  (define-struct hd-cookie (port 
                            shutdown-server
                            url-on-server-test
                            extract-url-path
                            url->string
                            find-browser
                            new-browser)
                 (make-inspector))
  
  (define (hd-cookie->browser hd-cookie)
    (or ((hd-cookie-find-browser hd-cookie))
        ((hd-cookie-new-browser hd-cookie))))
  
  (define (visit-url-in-browser hd-cookie url)
    (let ([browser (hd-cookie->browser hd-cookie)])
      (send browser show #t)
      (let* ([hp (send browser get-hyper-panel)]
             [hc (send hp get-canvas)])
        (send hc goto-url url #f)
        (void))))
  
  (define (visit-url-in-new-browser hd-cookie url)
    (let ([browser ((hd-cookie-new-browser hd-cookie))])
      (send browser show #t)
      (let* ([hp (send browser get-hyper-panel)]
             [hc (send hp get-canvas)])
        (send hc goto-url url #f)
        (void))))
  
  (provide/contract (visit-url-in-browser (hd-cookie? string? . -> . void?))
                    (visit-url-in-new-browser (hd-cookie? string? . -> . void?)))
  
  (provide
   hd-cookie->browser
   (struct hd-cookie (port 
                      shutdown-server
                      url-on-server-test
                      extract-url-path
                      url->string
                      find-browser
                      new-browser))))