(module web-interaction mzscheme
  (require (rename "expander.ss" send/suspend0 send/suspend)
           (all-except "expander.ss" send/suspend)
           "utils.ss"
           "session.ss"
           (lib "request-parsing.ss" "web-server")
           (lib "url.ss" "net"))
  
  (provide (all-from-except mzscheme #%module-begin)
           (rename lang-module-begin #%module-begin)
           send/suspend
           start-servlet)
  
  ;; start-servlet: -> request
  ;; set the initial interaction point for the servlet
  (define (start-servlet)
    (start-session dispatch)
    (start-interaction
     (lambda (req)
       (or (url->continuation (request-uri req))
           (lambda (req) (dispatch-start req))))))
  
  ;; send/suspend: (url -> response) -> request
  ;; the usual send/suspend
  (define (send/suspend page-maker)
    (send/suspend0
     (lambda (k)
       (page-maker (make-k-url k)))))
  
  ;; **********************************************************************
  ;; **********************************************************************
  ;; CONTINUATION TABLES
  (define k-table (make-hash-table))
  
  ;; continuation->number: continuation -> number
  ;; store a continuation and provide the key
  (define continuation->number
    (let ([n 0])
      (lambda (k)
        (set! n (add1 n))
        (hash-table-put! k-table n k)
        n)))
  
  ;; url->continuation: url -> (union continuation #f)
  ;; extract the key from the url and then lookup the continuation
  (define (url->continuation req-uri)
    (let ([ses-uri (session-url (current-session))])
      (let ([url-path-suffix (split-url-path ses-uri req-uri)])
        (and url-path-suffix
             (not (null? url-path-suffix))
             (hash-table-get k-table
                             (string->number (car url-path-suffix))
                             (lambda () #f))))))
  
  ;; make-k-url: continuation -> url
  ;; encode a continuation in a url
  (define (make-k-url k)
    (let ([uri (session-url (current-session))])
      (make-url
       (url-scheme uri)
       (url-user uri)
       (url-host uri)
       (url-port uri)
       (append (url-path uri) (list (number->string (continuation->number k))))
       (url-query uri)
       (url-fragment uri))))
  )