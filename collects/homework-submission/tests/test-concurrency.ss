;; Drive the servlet using HTTP, with a new connection for every page request.
;; The idea of this stress test is to test parrallel usages of this servlet.
;; Are there deadlocks or other concurrency issues?

;; This is not a SchemeUnit test. To run this test, require this module then
;; run (test-concurrency).

(module test-concurrency mzscheme
  (require (lib "etc.ss")
           (lib "url.ss" "net")
           (lib "send-assertions.ss" "web-server" "tools")
           (lib "xml.ss" "xml"))

  (provide test-concurrency)

  (define *SERVER-URL* "http://syrma.ccs.neu.edu:8125")
  (define *SERVLET-URL* (string-append *SERVER-URL* "/servlets/submit.ss"))

  (define (test-concurrency)
    (let ((tester
            (lambda (id)
              (let loop ((n 0))
                (single-concurrency-test)
                (printf "Concurrency stress test ~a:~a~n" id n)
                (when (= (remainder n 50) 0)
                  (printf "Resting~n")
                  (sleep 3))
                (loop (add1 n))))))
      (thread (lambda () (tester 0)))
      (thread (lambda () (tester 1))))
    (sleep +inf.0))

  ;; id-display : a -> a
  ;; Print the argument to STDOUT, then produce the argument.
  (define (id-display x) (printf "~v~n" x) x)

  ;; Read a page and convert it to a Xexpr
  (define (pre-process-page p)
    (begin0
      (xml->xexpr (read-xml/element p))
      (close-input-port p)))

  ;; Convert a k-url to a url.
  ;; post-process-page : string -> (string -> url)
  (define (post-process-page inputs)
    (lambda (k-url)
      (string->url (string-append *SERVER-URL* k-url "?" inputs))))

  (define (single-concurrency-test)
    ;; A user logs in, changes his or her password, then logs out.
    (let* ((login-page (user-logs-in "The Test Username" "The Test Password"))
           (passwd-page (to-password-page login-page))
           (change-passwd-page (call/input-url
                                 passwd-page
                                 get-pure-port
                                 (compose
                                   (post-process-page
                                     (string-append
                                       "old-password=The+Test+Password&"
                                       "new-password1=The+Test+Password&"
                                       "new-password2=The+Test+Password"))
                                   form->k-url
                                   pre-process-page))))
      (logout change-passwd-page))
    )

  (define (user-logs-in username password)
    (call/input-url
      (string->url *SERVLET-URL*)
      get-pure-port
      (compose 
        (post-process-page
          (string-append
            "username="
            (regexp-replace* " " username "+") "&"
            "password="(regexp-replace* " " password "+")))
        form->k-url
        pre-process-page)))

  (define (to-password-page a-url)
    (call/input-url
      a-url
      get-pure-port
      (compose
        (post-process-page "")
        (hyperlink->k-url "Change Password")
        pre-process-page)))

  (define (logout a-url)
    (close-input-port
      (get-pure-port
        (call/input-url
          a-url
          get-pure-port
          (compose
            (post-process-page "")
            (hyperlink->k-url "Logout")
            pre-process-page)))))

  )
