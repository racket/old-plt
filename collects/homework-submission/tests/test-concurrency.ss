#!/bin/sh
#|
exec mzscheme -vt "$0" "$@" -e '(test-concurrency)'
|#

;; Drive the servlet using HTTP, with a new connection for every page request.
;; The idea of this stress test is to test parrallel usages of this servlet.
;; Are there deadlocks or other concurrency issues?

;; This is not a SchemeUnit test. To run this test, require this module then
;; run (test-concurrency).

(module test-concurrency mzscheme
  (require (lib "etc.ss")
           (lib "url.ss" "net")
           (lib "send-assertions.ss" "web-server" "tools")
           (lib "xml.ss" "xml")
           "create-data.ss")

  (provide test-concurrency)

  (define *SERVER-URL* "http://acrux.ccs.neu.edu:8125")
  (define *SERVLET-URL* (string-append *SERVER-URL* "/servlets/submit.ss"))

  (define (test-concurrency)
    (with-handlers ((exn? (lambda (e) (db-do "ROLLBACK") (raise e))))
      (db-do "BEGIN")
      (cleanup)
      (setup)
      (db-do "COMMIT"))
    (let ((tester
            (lambda (id)
;              (let loop ((n 0))
                (printf "Started concurrency stress test ~a~n" id)
                (single-concurrency-test)
                (printf "Ended concurrency stress test ~a~n" id)
;                (printf "Concurrency stress test ~a:~a~n" id n)
;                (when (= (remainder n 50) 0)
;                  (printf "Resting~n")
;                  (sleep 3))
;                (loop (add1 n))
)))
;          )
      (for-each 
        (lambda (n)
          (thread (lambda () (tester n)))
          ;(sleep 1)
          )
        (build-list 20 (lambda (x) (add1 x)))))
    (sleep +inf.0)
    (with-handlers ((exn? (lambda (e) (db-do "ROLLBACK") (raise e))))
      (db-do "BEGIN")
      (cleanup)
      (db-do "COMMIT")))

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
    (with-handlers ((exn:xml? (lambda (e) (printf "XML parse error~n") "")))
      ;; A user logs in, changes his or her password, then logs out.
      #;(let* ((login-page (user-logs-in "student one" "password"))
             (passwd-page (to-password-page login-page))
             (change-passwd-page (call/input-url
                                   passwd-page
                                   get-pure-port
                                   (compose
                                     (post-process-page
                                       (string-append
                                         "old-password=password&"
                                         "new-password1=password&"
                                         "new-password2=password"))
                                     form->k-url
                                     pre-process-page))))
        (logout change-passwd-page))
      ;; A user logs in, selects a course in which he or she is a student,
      ;; goes to assignments, submits an assignment, logs out.
      (let* ((login-page (user-logs-in "student one" "password"))
             (course-main-page (to-course-main-page login-page))
             (assignments-page (to-assignments-page course-main-page))
             (assignments-page2
               (call/input-url
                 assignments-page
                 get-pure-port
                 (compose
                   (post-process-page "file=/etc/passwd")
                   form->k-url
                   pre-process-page)
                 (list "enctype: multipart/form-data"))))
        (logout assignments-page2))))

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

  (define (to-course-main-page a-url)
    (call/input-url
      a-url
      get-pure-port
      (compose
        (post-process-page "")
        (hyperlink->k-url "The Test Course")
        pre-process-page)))

  (define (to-assignments-page a-url)
    (call/input-url
      a-url
      get-pure-port
      (compose
        (post-process-page "")
        (hyperlink->k-url "Assignments")
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
