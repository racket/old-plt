;; run these tests with:
;;  % mzscheme --require test.ss

(module test mzscheme
  (require (lib "xml.ss" "xml"))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  
  ;; utils
  ;;

  ;; test-bad-read-input : format-str str -> void
  ;; First argument is the input, second is the error message
  (define (test-bad-read-input format-str err-string)
    (let ([str (format format-str)])
      (with-handlers ([exn:xml?
		       (lambda (x)
                         (unless (equal? (exn-message x) err-string)
			   (report-err format-str (exn-message x) err-string)))])
	(read-xml (open-input-string str))
	(report-err str "no error" err-string))))

  ;; tests-failed : number
  ;; incremened for each test that fails
  (define tests-failed 0)

  ;; report-err : string string string -> void
  ;; reports an error in the test suite
  ;; increments tests-failed.
  (define (report-err test got expected)
    (set! tests-failed (+ tests-failed 1))
    (printf "FAILED     test: ~a~n            got: ~a~n       expected: ~a~n"
	    test got expected))

  ;; done : -> void
  ;; prints out a message saying the tests are done.
  ;; if any tests failed, prints a message saying how many
  (define (done)
    (if (= tests-failed 0)
	(printf "All tests passed~n")
	(printf "~a tests failed~n" tests-failed)))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; reader error tests
  ;;

  (test-bad-read-input "<" "read-xml: lex-error: at position 1.2/2: unexpected eof")
  (test-bad-read-input "<a>" "read-xml: parse-error: unclosed `a' tag at [1.1/1 1.4/4]")
  (test-bad-read-input
   "<a></b>"
   "read-xml: parse-error: start tag `a' at [1.1/1 1.4/4] doesn't match end tag `b' at [1.4/4 1.8/8]")
  (test-bad-read-input
   "<a <a>" "read-xml: lex-error: at position 1.5/5: expected / or > to close tag `a'")

  (test-bad-read-input "~n<" "read-xml: lex-error: at position 2.2/3: unexpected eof")
  (test-bad-read-input "~n<a>" "read-xml: parse-error: unclosed `a' tag at [2.1/2 2.4/5]")
  (test-bad-read-input
   "~n<a></b>"
   "read-xml: parse-error: start tag `a' at [2.1/2 2.4/5] doesn't match end tag `b' at [2.4/5 2.8/9]")
  (test-bad-read-input
   "~n<a <a>" "read-xml: lex-error: at position 2.5/6: expected / or > to close tag `a'")


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; done
  ;;
  (done))
