;; Mike Burns, July 28th, 2004, netgeek@speakeasy.net
;; Test servlets.
;; - Normal servlet call.
;; - Servlet call plus arguments on the URL
;; - Incremental servlets
;; - Various MIME formats
;; - URL paths
(module test-servlets mzscheme
  (require (lib "contract.ss")
           (lib "test.ss" "schemeunit")
           (lib "url.ss" "net")
           "assertions.ss"
           )

  (provide/contract
    (test-servlets test-suite?))

  (define test1-output "<html><head><title>Title</title></head></html>")

  (define test2-output
    (string-append
      "<html><head><title>Title</title></head><body><h1>Title</h1><p>Current "
      "path: " (path->string (build-path web-root "servlets"))
      "</p></body></html>"))

  (define test2-incremental-output
    (string-append
      "<html><head><title>Title</title></head><body><h1>Title</h1><p>Current "
      "path: " (path->string (build-path web-root "servlets"))
      "</p></body></html>"))

  (define test3-output "blah blah plain text")

  (define test4-output
    (string-append
      "<html><head><title>Title</title></head><body><h1>Title</h1><p>ab</p>"
      "<p>seed</p></body></html>"))

  (define test5-output
    (string-append
      "<html><head><title>Title</title></head><body><h1>Title</h1><p>ab</p>"
      "<p>seed</p><p>Current path: " (path->string
                                       (build-path web-root "servlets"))
      "</p></body></html>"))

  (define test6-output "abseed")

  (define test7-output (path->string (build-path web-root "servlets")))

  (define test8-output (string-append (path->string
                                        (build-path web-root "servlets"))
                                      "abseed"))

  (define add-output "<title>The Answer</title><p>6</p>")

  (define test-servlets
    (make-test-suite
      "Miscellaneous servlet tests"

      ;; Non-incrementals
      (make-test-case
        (string-append
          "Non-incremental servlet with no arguments on the URL, "
          "in text/html, no URL path")
        (assert-serve/string "/servlets/test1.ss"
                      test1-output
                      "text/html"))

      (make-test-case
        (string-append
          "Non-incremental servlet with no arguments on the URL, "
          "in text/html, with URL path")
        (assert-serve/string "/servlets/test2.ss/home"
                      test2-output
                      "text/html"))

      (make-test-case
        (string-append
          "Non-incremental servlet with no arguments on the URL, "
          "in text/plain, no URL path")
        (assert-serve/string "/servlets/test3.ss"
                      test3-output
                      "text/plain"))

      (make-test-case
        (string-append
          "Non-incremental servlet with arguments on the URL, "
          "in text/html, no URL path")
        (assert-serve/string "/servlets/test4.ss?a=b&see=d"
                      test4-output
                      "text/html"))

      (make-test-case
        (string-append
          "Non-incremental servlet with arguments on the URL, "
          "in text/html, with URL path")
        (assert-serve/string "/servlets/test5.ss/home?a=b&see=d"
                      test5-output
                      "text/html"))

      (make-test-case
        (string-append
          "Non-incremental servlet with arguments on the URL, "
          "in text/plain, no URL path")
        (assert-serve/string "/servlets/test6.ss?a=b&see=d"
                      test6-output
                      "text/plain"))

      (make-test-case
        (string-append
          "Non-incremental servlet no arguments on the URL, "
          "in text/plain, with URL path")
        (assert-serve/string "/servlets/test7.ss/home"
                      test7-output
                      "text/plain"))

      (make-test-case
        (string-append
          "Non-incremental servlet with arguments on the URL, "
          "in text/plain, with URL path")
        (assert-serve/string "/servlets/test8.ss/home?a=b&see=d"
                      test8-output
                      "text/plain"))

      ;; Incrementals
      (make-test-case
        (string-append
          "Incremental servlet with no arguments on the URL, "
          "in text/html, no URL path")
        (assert-serve/string "/servlets/test1-incremental.ss"
                      test1-output
                      "text/html"))

      (make-test-case
        (string-append
          "Incremental servlet with no arguments on the URL, "
          "in text/html, with URL path")
        (assert-serve/string "/servlets/test2-incremental.ss/home"
                      test2-incremental-output
                      "text/html"))
      ;; Only the first two are tested incrementally.

      ;;; TODO
      ;;; - <form action="...?a=b;c=d" method="POST"> ... </form>

      ;; A servlet with an implicit send/back.
      (make-test-case
        "Implicit send/back"
        (let ((stop-server (start-server)))
          (let* ((p1 (get-pure-port
                       (string->url
                         (format "http://~a:~a/servlets/add.ss"
                                 THE-IP THE-PORT))))
                 (m1 (regexp-match #rx"action=\"([^\"]*)\"" p1))
                 (p2 (post-pure-port
                       (string->url
                         (format "http://~a:~a~a" THE-IP THE-PORT (cadr m1)))
                       #"number=1"
                       null))
                 (m2 (regexp-match #rx"action=\"([^\"]*)\"" p2))
                 (p3 (sync/timeout
                       5
                       (post-pure-port
                         (string->url
                           (format "http://~a:~a~a" THE-IP THE-PORT (cadr m2)))
                         #"number=2"
                         null))))
            (if p3
              (begin0
                (equal? (read-string 100 p3) add-output)
                (stop-server))
              (begin (stop-server) (fail))))))

      ;; A servlet that exits
      (make-test-case
        "A servlet that exits"
        (let ((stop-server (start-server)))
          (or
           (begin0
             (and
              (eof-object?
               (read-line
                (get-pure-port
                 (string->url
                  (format "http://~a:~a/servlets/exit.ss"
                          THE-IP THE-PORT)))))
              (eof-object?
               (read-line
                (get-pure-port
                 (string->url
                  (format "http://~a:~a/servlets/exit.ss"
                          THE-IP THE-PORT))))))
             (stop-server))
           (begin
             (stop-server)
             (fail)))))

      ;; Predict continuation URLs
      (make-test-case
        "Predictable continuation URLs"
        (let ((stop-server (start-server)))
          (let* ((p1 (get-pure-port
                       (string->url
                         (format "http://~a:~a/servlets/add.ss"
                                 THE-IP THE-PORT))))
                 (m1 (regexp-match #rx"action=\"([^\"]*)\"" p1)))
            (stop-server)
            (and
              (regexp-match #rx"/servlets;id[0-9]*\\*0/add.ss"
                            (bytes->string/utf-8 (cadr m1)))
              (fail)))))

      ;; Make sure timeouts don't kill connections
      (make-test-case
        "Timeouts that kill connections"
        (let ((stop-server (start-server)))
          (let* ((p1 (get-pure-port
                       (string->url
                         (format "http://~a:~a/servlets/timeout-bug.ss"
                                 THE-IP THE-PORT))))
                 (m1 (regexp-match #rx"href=\"([^\"]*)\"" p1))
                 (p2 (get-pure-port
                       (string->url
                         (format "http://~a:~a~a"
                                 THE-IP THE-PORT (cadr m1))))))
            (stop-server)
            (or (and (eof-object? p2) (fail)) #t))))

      ;; Exceptions can be repeated
      (make-test-case
        "Exceptions can be raised repeatedly"
        (let ((stop-server (start-server)))
          (let* ((p1 (get-pure-port
                       (string->url
                         (format "http://~a:~a/servlets/error.ss"
                                 THE-IP THE-PORT))))
                 (m1 (regexp-match #rx"href=\"([^\"]*)\"" p1))
                 (p2 (sync/timeout/enable-break
                       5
                       (get-impure-port
                         (string->url
                           (format "http://~a:~a~a"
                                   THE-IP THE-PORT (cadr m1))))))
                 (p3 (sync/timeout/enable-break
                       5
                       (get-impure-port
                         (string->url
                           (format
                             "http://~a:~a~a"
                             THE-IP THE-PORT (cadr m1)))))))
            (stop-server)
            (or (and p2 p3
                     (begin
                       (purify-port p3)
                       (purify-port p2)
                       (input-port-equal? p2 p3)))
                (fail)))))

      ;; call-with-input-file is a problem
      (make-test-case
        "call-with-input-file"
        (let ((stop-server (start-server)))
          (let* ((p1 (get-pure-port
                       (string->url
                         (format "http://~a:~a/servlets/call-with-input-file.ss"
                                 THE-IP THE-PORT))))
                 (m1 (regexp-match #rx"href=\"([^\"]*)\"" p1))
                 (p2 (sync/timeout/enable-break
                       5
                       (get-impure-port
                         (string->url
                           (format "http://~a:~a~a"
                                   THE-IP THE-PORT (cadr m1)))))))
            (stop-server)
            (or p2 (fail)))))


      ;; Threads started from the servlet must persist across connections
      (make-test-case
        "Thread started from the servlet persists across connections"
        (let ((stop-server (start-server)))
          (let* ((p1 (get-pure-port
                       (string->url
                         (format "http://~a:~a/servlets/thread-across-connections.ss"
                                 THE-IP THE-PORT))))
                 (m1 (regexp-match #rx"action=\"([^\"]*)\"" p1))
                 (p2 (get-pure-port
                       (string->url
                         (format "http://~a:~a~a"
                                 THE-IP THE-PORT (cadr m1))))))
            (stop-server)
            (or (regexp-match #rx"<p>Okay</p>" p2) (fail)))))

      ;; Thread at load time, still running after a timeout.
      (make-test-case
        "Thread at load time, still running after a timeout"
        (let ((stop-server (start-server)))
          (let ((u (string->url
                     (format "http://~a:~a/servlets/thread-at-load.ss"
                             THE-IP THE-PORT))))
            (let ((p1 (get-pure-port u)))
              (sleep 7)
              (let ((p2 (get-pure-port u)))
                (stop-server)
                (begin0
                  (input-port-equal? p1 p2)
                  (close-input-port p1)
                  (close-input-port p2)))))))

      ;; Thread started from within (start), stopped after a timeout.
      (make-test-case
        "Thread started from within (start), stopped after a timeout"
        (let ((stop-server (start-server)))
          (close-input-port
            (get-pure-port
              (string->url
                (format "http://~a:~a/servlets/thread-in-start.ss"
                        THE-IP THE-PORT))))
          (sleep 9)
          (or
            (begin0
              (let ((p (build-path "/" "tmp" "thread-in-start")))
                (if (file-exists? p)
                  (> (- (current-seconds) (file-or-directory-modify-seconds p))
                      2)
                  #f))
              (stop-server))
            (fail))))

      ;; Thread started at load time, shutdown during a refresh.
      (make-test-case
        "Thread started at load time, shutdown during a refresh"
        (let ((stop-server (start-server)))
          (close-input-port
            (get-pure-port
              (string->url
                (format "http://~a:~a/servlets/thread-at-load.ss"
                        THE-IP THE-PORT))))
          (close-input-port
            (get-pure-port
              (string->url
                (format "http://~a:~a/conf/refresh-servlets"
                        THE-IP THE-PORT))))
          (sleep 3)
          (or
            (begin0
              (let ((p (build-path "/" "tmp" "thread-at-load")))
                (if (file-exists? p)
                  (<= (- (current-seconds) (file-or-directory-modify-seconds p))
                      2)
                  #f))
              (stop-server))
            (fail))))

      ))

  )
