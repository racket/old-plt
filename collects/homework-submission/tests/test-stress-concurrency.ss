;; Drive the servlet using HTTP, with a new connection for every page request.
;; The idea of this stress test is to test parrallel usages of this servlet.
;; Are there deadlocks or other concurrency issues?

;; This is not a SchemeUnit test. To run this test, require this module then
;; run (test-stress-concurrency).

(module test-stress-concurrency mzscheme
  (require (lib "etc.ss")
           (lib "url.ss" "net")
           (lib "send-assertions.ss" "web-server" "tools")
           (lib "xml.ss" "xml"))

  (provide test-stress-concurrency)

  (define *SERVER-URL* "http://syrma.ccs.neu.edu:8125")
  (define *SERVLET-URL* (string-append *SERVLET-URL* "/servlets/hw.ss"))

  (define (test-stress-concurrency)
    (let loop ((n 0))
      (single-concurrency-test)
      (printf "Concurrency stress test ~a~n" n)
      (loop (add1 n))))

  ;; id-display : a -> a
  ;; Print the argument to STDOUT, then produce the argument.
  (define (id-display x) (printf "~v~n" x) x)

  ;; Read a page and convert it to a Xexpr
  (define (pre-process-page p)
    (xml->xexpr (read-xml/element p)))

  ;; Convert a k-url to a url.
  (define (post-process-page p)
    (string->url (string-append *SERVER-URL* p)))

  (define (single-concurrency-test)
    (void))

  )
