(module test-servlet mzscheme
  (require (lib "url.ss" "net")
           (lib "uri-codec.ss" "net")
           (lib "xml.ss" "xml")
           (lib "port.ss")
           (lib "match.ss")
           "connection-manager.ss"
           "request-parsing.ss"
           "servlet.ss"
           "test-harness.ss")

  ;; ********************************************************************************
  ;; ********************************************************************************
  ;;  TESTS FOR *embed*

  (define (embed-test invoke-id k-id)
    (lambda (in-url)
      (test (format "embed-test: url = ~a~n   " (url->string in-url))
            (lambda ()
              (equal? (list invoke-id k-id)
                      (embedded-ids? (string->url (embed-ids invoke-id k-id in-url))))))))

  (define (embed-fail-test in-url)
    (test (format "embed-fail-test: url = ~a~n   " (url->string in-url))
          (lambda ()
            (with-handlers ([exn:servlet?
                             (lambda (the-exn) #t)])
              (embed-ids 100 100 in-url)
              #f))))

  (define url-list1
    (list
     (string->url "http://www.home.org")
     (string->url "http://www.home.org/a")
     (string->url "http://www.home.org/a/")
     (string->url "http://www.home.org/a/b")
     (string->url "http://www.home.org/a/b/")
     (string->url "http://www.home.org/a/b/c")))

  (for-each (embed-test 100 100) url-list1)
  (for-each (embed-test 100 200) url-list1)

  (define simple-url-list1
    (list
     (string->url "")
     (string->url "/a") (string->url "/a/")
     (string->url "/a/b")
     (string->url "/a/b/")
     (string->url "/a/b/c")))

  (for-each (embed-test 100 100) simple-url-list1)
  (for-each (embed-test 100 200) simple-url-list1)

  (define url-with-param-list1
    (list
     (string->url "http://www.home.org;aparam")
     (string->url "http://www.home.org/;aparam")
     (string->url "http://www.home.org/a;aparam")
     (string->url "http://www.home.org/a/;aparam")
     (string->url "http://www.home.org;aparam/a/b/c")
     (string->url "http://www.home.org/a;aparam/a/b/c")
     (string->url "http://www.home.org;aparam/a/b/c/")
     (string->url "http://www.home.org/a;aparam/a/b/c/")))

  (for-each embed-fail-test url-with-param-list1)
  (for-each (embed-test 333 333)
            (map
             (lambda (a-url)
               (string->url
                (remove-ids a-url)))
             url-with-param-list1))

  (define simple-url-with-param-list1
    (list
     (string->url ";aparam")
     (string->url "/;aparam")
     (string->url "/a;aparam")
     (string->url "/a/;aparam")
     (string->url ";aparam/a/b/c")
     (string->url "/a;aparam/a/b/c")
     (string->url ";aparam/a/b/c/")
     (string->url "/a;aparam/a/b/c/")))

  (for-each embed-fail-test simple-url-with-param-list1)
  (for-each (embed-test 333 333)
            (map
             (lambda (a-url)
               (string->url
                (remove-ids a-url)))
             simple-url-with-param-list1))

  ;; ********************************************************************************
  ;; ********************************************************************************
  ;; TESTS FOR send/*

  (define the-instance (make-servlet-instance 0 (make-hash-table) 0))

  (define op (open-output-string)) ;; access with get-output-string


  ;; new-servlet-context: request o-port (->) -> servlet-context
  (define (new-servlet-context req op suspend)
    (make-servlet-context
     the-instance
     (make-connection 0 0 (open-input-string "foo") op (make-custodian) #t)
     req
     suspend))

  ;; new-request: string -> request
  (define (new-request a-url-str)
    (make-request 'get (string->url a-url-str) '() '() "a-host-ip" "a-client-ip"))

  ;; extract-url-binding/number: string request -> number
  ;; extract url-encoded bindings from an request
  (define (extract-url-binding/number key req)
    (let ([query (url-query (request-uri req))])
      (string->number
       (cdr
        (assoc key (form-urlencoded->alist query))))))

  ;; embed-url-bindings: (listof (cons string string)) url -> url
  ;; encode bindings in a url
  (define (embed-url-bindings env in-url)
    (let* ([query (url-query in-url)]
           [old-env (if query
                        (form-urlencoded->alist query)
                        '())])
      (make-url
       (url-scheme in-url)
       (url-user in-url)
       (url-host in-url)
       (url-port in-url)
       (url-path in-url)
       (alist->form-urlencoded (append env old-env))
       (url-fragment in-url))))

  ;; remove-query: url -> url
  ;; remove the query part from a url
  (define (remove-query in-url)
    (make-url
     (url-scheme in-url)
     (url-user in-url)
     (url-host in-url)
     (url-port in-url)
     (url-path in-url)
     ""
     (url-fragment in-url)))

  ;; run-servlet: request str -> s-expression
  ;; run a servlet and return it's next response
  ;; note that the servlet may be a continuation
  (define (run-servlet req svt)
    (let* ([cust (make-custodian)]
           [result-channel (make-channel)]
           [op (open-output-string)]
           [sc (new-servlet-context req op
                                    (make-suspender result-channel op cust))])
      (let ([old-ctx (thread-cell-ref current-servlet-context)])
        (parameterize ([current-custodian cust])
          (thread
           (lambda ()
             (thread-cell-set! current-servlet-context sc)
             (svt)))))
      (channel-get result-channel)))

  ;; make-suspender: channel o-port custodian -> ->
  (define (make-suspender result-channel op cust)
    (lambda ()
      (channel-put
       result-channel
       (let ([ip (open-input-string
                  (get-output-string op))])
         (purify-port ip)
         (match (xml->xexpr (read-xml/element ip))
           [('result (['href url-str]) rest ...)
            (cons (string->url url-str) rest)]
           [('result () rest ...) rest]
           [expr
            (format "result didn't match: ~a~n" expr)])))
      (custodian-shutdown-all cust)))

  ;; start-servlet: thunk -> s-expression
  ;; start a servlet and return it's first response
  (define (start-servlet svt)
    (run-servlet (new-request "www.nowhere.com") svt))

  ;; resume-servlet: url (listof (cons string string)) -> s-expression
  ;; resume a servlet and return the it's next response
  (define (resume-servlet prev-url env)
    (cond
     [(embedded-ids? prev-url)
      => (lambda (res)
           (let ([k (hash-table-get (servlet-instance-k-table the-instance) (cadr res))]
                 [new-req (new-request (remove-ids (embed-url-bindings env prev-url)))])
             (run-servlet new-req (lambda () (k new-req)))))]
     [(error "url doesn't encode a servlet continuation")]))

;;  ********************************************************************************
;;  ********************************************************************************

  (define (servlet01)
    (send/suspend
     (lambda (url-str)
       `(result ([href ,url-str]) "argument")))

    (send/finish
     `(result () "All done!")))

  (define (add.com)
    (let ([get-number
           (lambda ()
             (extract-url-binding/number
              "the-number"
              (send/suspend
               (lambda (url-str)
                 `(result ([href ,url-str]) "enter a number")))))])

      (send/back
       `(result () ,(format "~a" (+ (get-number) (get-number)))))))

  ;;(let* ([first-response (start-servlet servlet01)]
  ;;       [second-response
  ;;        (resume-servlet (car first-response) '())])
  ;;  (list first-response second-response))

  (define first-page (start-servlet add.com))
  (define second-page
    (resume-servlet (car first-page) (list (cons "the-number" "1"))))

  (define (myadd1 a-number)
    (string->number
     (car
      (resume-servlet (remove-query (car second-page))
                      (list (cons "the-number" (number->string a-number)))))))
  )
