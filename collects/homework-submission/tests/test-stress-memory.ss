;; Drive the servlet using HTTP, with a new connection for every page request.
;; The idea of this stress test is to test memory usage: does the servlet
;; continuously eat memory, and where?

;; This is not a SchemeUnit test. To run this test, require this module then
;; run (test-stress-memory).

(module test-stress-memory mzscheme
  (require (lib "etc.ss")
           (lib "url.ss" "net")
           (lib "send-assertions.ss" "web-server" "tools")
           (lib "xml.ss" "xml"))

  (provide test-stress-memory)

  (define *SERVER-URL* "http://syrma.ccs.neu.edu:8125")
  (define *SERVLET-URL* (string-append *SERVER-URL* "/servlets/submit.ss"))

  (define (test-stress-memory)
    (let loop ((n 0))
      (single-memory-test)
      (printf "Memory stress test ~a~n" n)
      (loop (add1 n))))

  ;; id-display : a -> a
  ;; Print the argument to STDOUT, then produce the argument.
  (define (id-display x) (printf "~v~n" x) x)

  ;; Read a page and convert it to a Xexpr
  (define (pre-process-page p)
    (id-display (xml->xexpr (id-display (read-xml/element p)))))

  ;; Convert a k-url to a url.
  ;; post-process-page : string -> (string -> url)
  (define (post-process-page inputs)
    (lambda (k-url)
      (string->url (id-display (string-append *SERVER-URL* k-url "?" inputs)))))

  (define (single-memory-test)
    ;; A user logs in, then closes the Web browser
    (call/input-url
      (call/input-url
        (string->url *SERVLET-URL*)
        get-pure-port
        (compose (post-process-page
                   (string-append
                     "username=The+Test+Username&"
                     "password=The+Test+Password"))
                 form->k-url
                 pre-process-page))
      get-pure-port
      pre-process-page))

  )
