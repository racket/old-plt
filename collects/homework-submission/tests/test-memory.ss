#!/bin/sh
#|
exec mzscheme -vt "$0" "$@" -e '(test-memory)'
|#

;; Drive the servlet using HTTP, with a new connection for every page request.
;; The idea of this stress test is to test memory usage: does the servlet
;; continuously eat memory, and where?

;; This is not a SchemeUnit test. To run this test, require this module then
;; run (test-memory).

(module test-memory mzscheme
  (require (lib "etc.ss")
           (lib "url.ss" "net")
           (lib "send-assertions.ss" "web-server" "tools")
           (lib "xml.ss" "xml"))

  (provide test-memory)

  (define *SERVER-URL* "http://subra.ccs.neu.edu:8125")
  (define *SERVLET-URL* (string-append *SERVER-URL* "/servlets/submit.ss"))

  (define (test-memory)
    (fprintf (current-error-port) "here~n")
    (let ((tester (lambda (id)
                    (let loop ((n 0))
                      (single-memory-test)
                      (fprintf (current-error-port)
                               "Memory stress test ~a:~a~n" id n)
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

  (define (single-memory-test)
    ;; A user logs in, then closes the Web browser
    (call/input-url
      (call/input-url
        (string->url *SERVLET-URL*)
        get-pure-port
        (compose (post-process-page
                   (string-append
                     "username=student one&"
                     "password=password"))
                 form->k-url
                 pre-process-page))
      get-pure-port
      pre-process-page))

  )
