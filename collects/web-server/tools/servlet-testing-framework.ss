;; The idea is to mimick the entire Web server as part of the framework for
;; this testing infrastructure. Copy most of this stuff from v299. The v299 Web
;; server was written with the assumption that continuations exist across
;; threads; this is not the case in the exp Web server. As a result, only one
;; thread should be used at a time.

;; Since the real send/* are used, with their full continuation table, one can
;; use this to fully pretend to be a Web browser, including back buttons and
;; cloning Web pages.
(module servlet-testing-framework mzscheme
  (require (lib "match.ss")
           (lib "list.ss")
           (lib "xml.ss" "xml")

           "uri-codec.ss"
           "url.ss"
           "servlet-helpers.ss"
           "servlet-tables.ss"
           "connection-manager.ss"
           "timer.ss"
           (all-except "request-parsing.ss" request-bindings)
           )

  (provide start-servlet resume-servlet)

  ;; Start the servlet
  (define (start-servlet svt)
    (run-servlet (new-request) svt))

  (define the-instance
    (make-servlet-instance 'id0 (make-hash-table) 0 (make-semaphore 0)))

  ;; new-servlet-context: request o-port (-> void) (box ('a -> void))
  ;; -> servlet-context
  (define (new-servlet-context req op suspend abort)
    (make-servlet-context
      the-instance
      (let ((cust (make-custodian)))
        (make-connection
          (start-timer 15 (lambda () ((unbox abort) (void))))
          (open-input-string "foo") op cust #t))
      req
      suspend))

  ;; run-servlet: request ( -> void) -> s-expression
  ;; Run a servlet and return its next response. Note that the servlet may be a
  ;; continuation.
  (define (run-servlet req svt)
    (let* ((op    (open-output-string))
           (abort (box #f))
           (sc    (new-servlet-context req op (make-suspender op abort) abort)))
      (update-current-servlet-context! sc)
        (let/cc k
          (set-box! abort k)
          (svt))
      (let ((ip (open-input-string (get-output-string op))))
        (purify-port ip)
        (xml->xexpr (read-xml/element ip)))))

  ;; make-suspender: o-port (box (value -> void)) -> (-> void)
  (define (make-suspender op abort)
    (lambda () ((unbox abort) (void))))

  ;; Resume the servlet
  (define (resume-servlet prev-url input)
    (let ((u (string->url prev-url)))
      (cond
        ((embedded-ids? u)
         => (lambda (res)
              (let ((k (hash-table-get (servlet-instance-k-table the-instance)
                                       (cadr res)))
                    (new-req (new-request/url (embed-url-bindings input u))))
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
