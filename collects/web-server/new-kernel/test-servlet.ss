(require (lib "url.ss" "net")
         (lib "uri-codec.ss" "net")
         (lib "xml.ss" "xml")
         (lib "port.ss")
         (lib "match.ss")
         "servlet.ss"
         "server-kernel-structs.ss")

;; ********************************************************************************
;; ********************************************************************************
;;  TESTS FOR *embed*

;(define (make-embed-test invoke-id k-id)
;  (lambda (in-url)
;    (printf "embed-test: url = ~a~n   " (url->string in-url))
;    (let ([new-url (string->url (embed-ids invoke-id k-id in-url))])
;      (if (equal? (list invoke-id k-id)
;                  (embedded-ids? new-url))
;          (printf "PASS: invoke-id = ~a, k-id = ~a~n" invoke-id k-id)
;          (printf "FAIL: invoke-id = ~a, k-id = ~a~n" invoke-id k-id)))))

;(define (embed-fail-test in-url)
;  (printf "embed-fail-test: url = ~a~n   " (url->string in-url))
;  (with-handlers ([exn:servlet?
;                   (lambda (the-exn)
;                     (printf "PASS~n"))])
;   (embed-ids 100 100 in-url)
;   (printf "FAIL:~n")))

;(define url-list1
;  (list
;   (string->url "http://www.home.org")
;   (string->url "http://www.home.org/a")
;   (string->url "http://www.home.org/a/")
;   (string->url "http://www.home.org/a/b")
;   (string->url "http://www.home.org/a/b/")
;   (string->url "http://www.home.org/a/b/c")))

;(for-each (make-embed-test 100 100) url-list1)
;(for-each (make-embed-test 100 200) url-list1)

;(define simple-url-list1
;  (list
;   (string->url "")
;   (string->url "/a") (string->url "/a/")
;   (string->url "/a/b")
;   (string->url "/a/b/")
;   (string->url "/a/b/c")))

;(for-each (make-embed-test 100 100) simple-url-list1)
;(for-each (make-embed-test 100 200) simple-url-list1)

;(define url-with-param-list1
;  (list
;   (string->url "http://www.home.org;aparam")
;   (string->url "http://www.home.org/;aparam")
;   (string->url "http://www.home.org/a;aparam")
;   (string->url "http://www.home.org/a/;aparam")
;   (string->url "http://www.home.org;aparam/a/b/c")
;   (string->url "http://www.home.org/a;aparam/a/b/c")
;   (string->url "http://www.home.org;aparam/a/b/c/")
;   (string->url "http://www.home.org/a;aparam/a/b/c/")))

;(for-each embed-fail-test url-with-param-list1)
;(for-each (make-embed-test 333 333)
;          (map
;           (lambda (a-url)
;             (string->url
;              (remove-ids a-url)))
;           url-with-param-list1))

;(define simple-url-with-param-list1
;  (list
;   (string->url ";aparam")
;   (string->url "/;aparam")
;   (string->url "/a;aparam")
;   (string->url "/a/;aparam")
;   (string->url ";aparam/a/b/c")
;   (string->url "/a;aparam/a/b/c")
;   (string->url ";aparam/a/b/c/")
;   (string->url "/a;aparam/a/b/c/")))

;(for-each embed-fail-test simple-url-with-param-list1)
;(for-each (make-embed-test 333 333)
;          (map
;           (lambda (a-url)
;             (string->url
;              (remove-ids a-url)))
;           simple-url-with-param-list1))

;; ********************************************************************************
;; ********************************************************************************
;; TESTS FOR send/*

;;   (define-struct connection (i-port o-port custodian close?) (make-inspector))

(define the-instance (make-servlet-instance 0 (make-hash-table) 0))

(define op (open-output-string)) ;; access with get-output-string

;; create-servlet-context: string suspend-proc -> servlet-context
;; create a servlet-context for testing purposes
(define (create-servlet-context a-url-str suspend)
  (make-servlet-context
   the-instance
   (make-connection #f (open-output-string) #f #t)
   (make-http-request 'line (string->url a-url-str) 'headers #t)
   suspend))

(define (create-servlet-context/op op a-url-str suspend)
  (make-servlet-context
   the-instance
   (make-connection #f op #f #t)
   (make-http-request 'line (string->url a-url-str) 'headers #t)
   suspend))


;; servlet-context->string: servlet-context -> string
;; extract the string from the string output-port contained in a servlet-context
(define (servlet-context->string sc)
  (let ([ip (open-input-string
             (get-output-string
              (connection-o-port
               (servlet-context-connection sc))))])
;        [op (open-output-string)])
    (purify-port ip)
    (read-line ip)))

;; extract-url-binding: string http-request -> number
;; extract url-encoded bindings from an http-request
(define (extract-url-binding key req)
  (let ([query (url-query (http-request-uri req))])
    (assoc key (form-urlencoded->alist query))))

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

;; so we want to make some non-trivial calls to send/? hmm...

;; in this context, a servlet should just be a thunk that calls
;; the send/? functions. This thunk will be run on an arbitrary
;; thread. So how do we do this...

;; 1. cook up some servlet context
;; 2. parameterize over the servlet context and launch the servlet thread
;;    with the thunk
;; 3. when the servlet calls suspend via a send/* procedure then the test
;;    needs to play the role of the client.
;; 4. To play the role of the client, look at what was written to the string port
;;    in the connection part of the servlet context.

;; run-servlet: url str -> s-expression
;; run a servlet and return it's next response
;; note that the servlet may be a continuation
(define (run-servlet a-url-str svt)
  (let* ([cust (make-custodian)]
         [result-channel (make-channel)]
         [op (open-output-string)]
         [sc (create-servlet-context/op
              op
              a-url-str
              (lambda ()
                (channel-put
                 result-channel
                 (let ([ip (open-input-string
                            (get-output-string op))])
                   (purify-port ip)
                   (match (xml->xexpr (read-xml/element ip))
                    [('result (['href url-str]) rest ...) (cons (string->url url-str)
                                                          rest)]
                    [('result () rest ...) rest]
                    [expr (format "result didn't match: ~a" expr)])))
                (custodian-shutdown-all cust)))])
    (parameterize ([current-servlet-context sc]
                   [current-custodian cust])
                  (thread svt))
    (channel-get result-channel)))

;; start-servlet: thunk -> s-expression
;; start a servlet and return it's first response
(define (start-servlet svt)
  (run-servlet "www.nowhere.com" svt))

;; resume-servlet: url (listof (cons string string)) -> s-expression
;; resume a servlet and return the it's next response
(define (resume-servlet prev-url env)
  (cond
   [(embedded-ids? prev-url)
    => (lambda (res)
         (let ([k (hash-table-get (servlet-instance-k-table the-instance) (cadr res))])
           (run-servlet (remove-ids (embed-url-bindings env prev-url)) k)))]
   [(error "url doesn't encode a servlet continuation")]))

;; simple-test: string string proc -> void
;; just see if the send/* function is alive
(define (simple-test uri-str proc)
  (let ([result
         (let/ec k
          (letrec ([sc (create-servlet-context uri-str
                                               (lambda ()
                                                 (k (servlet-context->string sc))))])
                   (parameterize ([current-servlet-context sc])
                                 (proc uri-str))))])
    (let ([result (remove-ids (string->url result))])
;      (printf "result = ~s   uri-str = ~s~n" result uri-str)
      (if (string=? result uri-str)
          (printf "PASS~n")
          (printf "FAIL~n")))))

(printf "simple-test with send/back: ")
(simple-test "http://www.home.org/" send/back)

(printf "simple-test with send/finish: ")
(simple-test "http://www.home.org/" send/finish)

(printf "simple-test with send/suspend: ")
(simple-test "http://www.home.org/"
             (lambda (ignore-url)
               (send/suspend
                (lambda (k-url)
                  k-url))))

(printf "simple-test with send/forward: ")
(simple-test "http://www.home.org/"
             (lambda (ignore-url)
               (send/forward
                (lambda (k-url)
                  k-url))))

(define (servlet01)
  (send/suspend
   (lambda (url-str)
     `(result ([href ,url-str]) "argument")))

  (send/finish
   `(result () "All done!")))


(define (servlet02)
  (send/suspend
   (lambda (url-str)
     `(result ([href ,url-str]) "argument")))

  (send/suspend
   (lambda (url-str)
   `(result ([href ,url-str]) "All done!"))))

(let* ([first-response (start-servlet servlet02)]
       [second-response
        (resume-servlet (car first-response) '())])
  (list first-response second-response))

   
