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
           (lib "url.ss" "net")
           (lib "uri-codec.ss" "net")
           (lib "xml.ss" "xml")

           (lib "servlet.ss" "web-server")
           (lib "servlet-tables.ss" "web-server")
           (lib "connection-manager.ss" "web-server")
           (lib "timer.ss" "web-server")
           (all-except (lib "request-parsing.ss" "web-server")
                       request-bindings)

           "backend-servlet-testing.ss"
           )

  (provide start-servlet resume-servlet resume-servlet/headers)

  ;; Start the servlet
  (define (start-servlet svt)
    (simple-start-servlet
      (make-request 'get (string->url "http://example.com/")
                    '() '() "a-host-ip" "a-client-ip")
      svt))

  (define (resume-servlet/headers prev-url input headers)
    (with-handlers
      ((exn:fail:contract?
         (lambda (e)
           '(html (head (title "Timeout"))
                  (body
                    (p "The transaction referred to by this url is no longer "
                       "active.  Please " (a ((href "...")) "restart")
                       " the transaction."))))))
      (let ((a-url (string->url prev-url)))
        (simple-resume-servlet
          (let ((new-url (embed-url-bindings input a-url)))
            (make-request 'get new-url headers (url-query new-url)
                          "a-host-ip" "a-client-ip"))
          a-url))))

  ;; Resume the servlet
  (define (resume-servlet prev-url input)
    (resume-servlet/headers prev-url input '()))

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

  )
