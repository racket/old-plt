(module test mzscheme
  (require (lib "xml.ss" "xml"))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  
  ;; utils
  ;;

  ;; test-bad-read-input : str str -> void
  ;; First argument is the input, second is the error message
  (define (test-bad-read-input str err-string)
    (with-handlers ([exn:user?
		     (lambda (x)
		       (unless (equal? (exn-message x) err-string)
			 (report-err str (exn-message x) err-string)))])
      (read-xml (open-input-string str))
      (report-err str "no error" err-string)))

  ;; report-err : string string string -> void
  ;; reports an error in the test suite
  (define (report-err test got expected)
    (printf "FAILED     test: ~a~n            got: ~a~n       expected: ~a~n"
	    test got expected))




  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; reader error tests
  ;;

  (test-bad-read-input "<" "unclosed tag")
  (test-bad-read-input "<a>" "no end tag")
  (test-bad-read-input "<a></b>" "bad end tag")

  )

(require test)

(exit)