;; Test send/suspend, send/back, send/finish, and send/forward
;;; TODO: test send/suspend/callback, send/suspend/dispatch
(module test-send mzscheme
  (require (lib "test.ss" "schemeunit")
           (lib "match.ss")
           (all-except (lib "1.ss" "srfi")
                       reverse! member map for-each assoc append!)
           (lib "url.ss" "net")
           (lib "uri-codec.ss" "net")
           (lib "xml.ss" "xml")
           (lib "servlet.ss" "web-server")
           (lib "servlet-tables.ss" "web-server")
           (lib "connection-manager.ss" "web-server")
           (lib "timer.ss" "web-server")
           (all-except (lib "request-parsing.ss" "web-server")
                       request-bindings)
           )

  (provide test-send)

  (define test-send
    (make-test-suite
      "Test send/suspend, send/back, send/finish, and send/forward"

      ;; send/back
      (make-test-case
        "send/back"
        (assert-output-response
          (lambda () (send/back '(result "The result")))
          '(result () "The result")))

      ;; send/finish
      (make-test-case
        "send/finish"
        (assert-output-response/finish
          (lambda () (send/finish '(result "The result")))
          '(result () "The result")))

      ;; send/suspend
      (make-test-case
        "send/suspend"
        (assert-output-response/finish/send/suspend
          (lambda ()
            (let ((get-num
                    (lambda ()
                      (string->number
                        (extract-binding/single
                          'n
                          (request-bindings
                            (send/suspend
                              (lambda (k-url)
                                `(query ,k-url)))))))))
              (send/finish
                `(result ,(number->string
                           (+ (get-num) (get-num)))))))
          (cons 'n "2") (cons 'n "2")
          "<result>4</result>"))

      ;; send/forward
      (make-test-case
        "send/suspend"
        ;;; Feed the send/forward
        ;;; Ensure output-response produces the right value
        ;;; Make sure the continuation table is cleared by send/forward
        ;;; Make sure the continuation table is cleared by send/finish
        (assert-output-response/finish/send/forward
          (lambda ()
            (let ((get-num
                    (lambda ()
                      (string->number
                        (extract-binding/single
                          'n
                          (request-bindings
                            (send/forward
                              (lambda (k-url)
                                `(query ,k-url)))))))))
              (send/finish
                `(result ,(number->string
                           (+ (get-num) (get-num)))))))
          (cons 'n "2") (cons 'n "2")
          "<result>4</result>"))

      ))

  ;; Ensure output-response produces the right value
  (define-simple-assertion (assert-output-response outputter out)
    (equal-output-response? outputter out))

  ;; Ensure output-response produces the right value
  ;; Make sure the continuation table is cleared
  (define-simple-assertion (assert-output-response/finish outputter out)
    (and (equal-output-response? outputter out)
         (clear-continuation-tables?)))

  ;; Ensure output-response produces the right value
  ;; Feed the send/suspends
  ;; Make sure the continuation table is cleared by send/finish
  (define (assert-output-response/finish/send/suspend outputter . input)
    (let ((out (last input))
          (in (all-but-last input)))
      (and
        (fold/acc
          (lambda (x i)
            (match x
              (('query () prev-url) (resume-servlet prev-url i))
              (else #f)))
          (start-servlet outputter)
          in
          (lambda (o) (equal? o out)))
        (clear-continuation-tables?))))

  ;; Ensure output-response produces the right value
  ;; Feed the send/forward
  ;; Make sure the continuation table is cleared by send/forward
  ;; Make sure the continuation table is cleared by send/finish
  (define (assert-output-response/finish/send/forward outputter . input)
    (let ((out (last input))
          (in (all-but-last input)))
      (and
        (fold/acc
          (lambda (x i)
            (match x
              (('query () prev-url) (and (resume-servlet prev-url i)
                                         (clear-continuation-tables?))) ;;; ?
              (else #f)))
          (start-servlet outputter)
          in
          (lambda (o) (equal? o out)))
        (clear-continuation-tables?))))


  ;; ('a 'b -> 'a) 'a (listof 'b) ('a -> 'c) -> 'c
  ;; Perform f on the a and the first element of l to get the next value.
  ;; Perform f on the next value and the next element of l, and so on, until l
  ;; is empty, in which case f produces its final value. Apply g to f's final
  ;; value.
  (define (fold/acc f a l g)
    (cond
      ((null? l) (g a)) ;;; Where this differs from fold
      (else (fold/acc f (f a (car l)) (cdr l) g))))

  ;; Produce all elements of the list, except for the last one.
  (define (all-but-last l)
    (reverse (cdr (reverse l))))

  ;; Does the output-response used in o1 produce the same value as o2?
  (define (equal-output-response? o1 o2)
    (equal? (start-servlet o1) o2))

  ;; Are the continuation tables cleared?
 (define (clear-continuation-tables?)
   #t) ;;;

 ;; Start the servlet
 (define (start-servlet svt)
   (run-servlet (new-request) svt))

 (define the-instance
   (make-servlet-instance 'id0 (make-hash-table) 0 (make-semaphore 0)))

 ;; new-servlet-context: request o-port (-> void) -> servlet-context
 (define (new-servlet-context req op suspend)
   (make-servlet-context 
     the-instance
     (let ((cust (make-custodian)))
       (make-connection 
         (start-timer 15 (lambda () (custodian-shutdown-all cust)))
         (open-input-string "foo") op cust #t))
     req
     suspend))

 ;; run-servlet: request string -> s-expression
 ;; Run a servlet and return its next response. Note that the servlet may be a
 ;; continuation.
 (define (run-servlet req svt)
   (let* ((cust (make-custodian))
          (result-channel (make-channel))
          (op (open-output-string))
          (sc (new-servlet-context req op
                                   (make-suspender result-channel op cust))))
     (parameterize ((current-custodian cust))
       (thread
         (lambda ()
           (thread-cell-set! current-servlet-context sc)
           (svt))))
     (channel-get result-channel)))

 ;; make-suspender: channel o-port custodian -> (-> void)
 (define (make-suspender result-channel op cust)
   (lambda ()
     (channel-put
       result-channel
       (let ((ip (open-input-string (get-output-string op))))
         (purify-port ip)
         (xml->xexpr (read-xml/element ip))))
     (custodian-shutdown-all cust)))

 ;; Resume the servlet
 (define (resume-servlet prev-url input)
   (let ((u (string->url prev-url)))
     (cond
       ((embedded-ids? u)
        => (lambda (res)
             (let ((k (hash-table-get (servlet-instance-k-table the-instance)
                                      (cadr res)))
                   (new-req (new-request/url
                              (embed-url-bindings (list input) u))))
               (run-servlet new-req (lambda () (k new-req))))))
       (else (error "url doesn't encode a servlet continuation")))))

  ;; embed-url-bindings: (listof (cons string string)) url -> url
  ;; encode bindings in a url
  (define (embed-url-bindings env in-url)
    (let* ((query (url-query in-url))
           (old-env (or query '())))
      (make-url
       (url-scheme in-url)
       (url-user in-url)
       (url-host in-url)
       (url-port in-url)
       (url-path in-url)
       (append env old-env)
       (url-fragment in-url))))

  (define (remove-query an-url)
    (make-url
      (url-scheme an-url)
      (url-user an-url)
      (url-host an-url)
      (url-port an-url)
      (url-path an-url)
      '()
      (url-fragment an-url)))

 ;; Produce a new request
 (define (new-request)
   (new-request/bindings '()))

 ;; Produce a new request, with an url
 (define (new-request/url new-url)
   (make-request 'get (remove-query new-url) '() (url-query new-url)
                 "a-host-ip" "a-client-ip"))

 ;; Produce a new request, with bindings
 (define (new-request/bindings bs)
   (make-request 'get (string->url "http://www.example.com/") '() bs
                 "a-host-ip" "a-client-ip"))

  )
