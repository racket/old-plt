(module reload-test mzscheme
  (require "reload.ss")
  (define tmp-files '("dne1.ss" "dne2.ss"))

  (define (set-file fn sexp)
    (call-with-output-file fn
      (lambda (port) (write sexp port))
      'text 'truncate))

  (define (reload-test test-name expected-output to-rewrite)
    (let/ec escape
      (let ([o (open-output-string)])
	(with-handlers ([(lambda (x) #t)
			 (lambda (x)
			   (failed test-name
				   (if (exn? x)
				       (exn-message x)
				       (format "~s" x)))
			   (escape (void)))])
	  (parameterize ([current-output-port o]
			 [current-namespace (make-namespace)])
	    (let ([secs (current-seconds)])
	      (reload (car tmp-files) secs)
	      (sleep 1.1)  ;; arg! how do I mess with time to test properly?
	      (when to-rewrite
		(rewrite-file to-rewrite))
	      (reload (car tmp-files) secs))))
	(unless (string=? (get-output-string o) expected-output)
	  (failed test-name (get-output-string o) expected-output)))))

  (define (rewrite-file filename)
    (set-file filename (call-with-input-file filename read)))

  (define any-failed? #f)
  (define failed
    (case-lambda
     [(test-name error-msg)
      (set! any-failed? #t)
      (printf "FAILED: ~a; exn: ~a~n" test-name error-msg)]
     [(test-name got expected)
      (set! any-failed? #t)
      (printf "FAILED: ~a~n             got: ~s~n        expected: ~s~n"
	      test-name got expected)]))

  (set-file "dne1.ss" '(module dne1 mzscheme (display "1")))
  (reload-test "no change" "1" #f)

  (set-file "dne1.ss" '(module dne1 mzscheme (display "1")))
  (reload-test "direct change" "11" "dne1.ss")

  (set-file "dne1.ss" '(module dne1 mzscheme (require "dne2.ss")))
  (set-file "dne2.ss" '(module dne2 mzscheme (display "1")))
  (reload-test "required change" "11" "dne2.ss")

  (set-file "dne1.ss" '(module dne1 mzscheme (require "dne2.ss") (display "1")))
  (set-file "dne2.ss" '(module dne2 mzscheme))
  (reload-test "required change/orig load" "11" "dne2.ss")

  (set-file "dne1.ss" '(module dne1 mzscheme (require-for-syntax "dne2.ss")))
  (set-file "dne2.ss" '(module dne2 mzscheme (display "1")))
  (reload-test "required-for-syntax change" "111" "dne2.ss")

  (set-file "dne1.ss" '(module dne1 mzscheme (require-for-syntax "dne2.ss") (display "1")))
  (set-file "dne2.ss" '(module dne2 mzscheme))
  (reload-test "required-for-syntax change/orig load" "11" "dne2.ss")

  (unless any-failed?
    (printf "all tests passed~n"))

  (for-each delete-file tmp-files))
