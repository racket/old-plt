;; Raise an exception
(module error mzscheme
  (require (lib "servlet.ss" "web-server"))
  (provide timeout interface-version start)

  (define timeout 5)

  (define interface-version 'v1)

  (define (start req)
    (send/suspend                                                                 
      (lambda (url)                                                                
        `(html (head (title "Test01.ss"))                                          
               (body                                                               
                 (a ((href ,url)) "click here to go on")))))
    (error 'ka-boom!))
  
  )
