(define jr-input (build-path (collection-path "mzlib")
			     'up 'up "tests" "drscheme-jr" "all-tests.scm"))
(unless (file-exists? jr-input)
  (error 'drscheme-jr.ss "cannot find the drscheme jr test files, looked in: ~s"
	 jr-input))
(set! jr-input (normalize-path jr-input))

(define drs (wait-for-drscheme-frame))
(define interactions-text (ivar drs interactions-text))

(define flush-err void)
(define flush-out void)
(define language-level/beginner "Beginning Student")
(define language-level/intermediate "Intermediate Student")
(define language-level/advanced "Advanced Student")
(define (try input output)
  (type-in-interactions drs input)
  (let ([answer-start (+ (send interactions-text last-position) 1)])
    (type-in-interactions drs (string #\newline))
    (wait-for-computation drs)
    (let* ([got (fetch-output
		 drs
		 answer-start
		 (send interactions-text paragraph-end-position
		       (- (send interactions-text last-paragraph) 1)))]
	   [expected (if (list? output)
			 (cadr output)
			 output)]
	   [matched?
	    (cond
	     [(eq? output 'void) (equal? got "")]
	     [(list? output)
	      (regexp-match (cadr output) got)]
	     [else
	      (equal? got expected)])])
      (unless matched?
	(printf "FAILED:    input: ~s~n        expected: ~s~n             got: ~s~n"
		input expected got)))))

(define run-test
  (case-lambda
   [(language) (run-test language  "Graphical (MrEd)")]
   [(language language-level/mzscheme/debug)
    (set-language-level! language)
    (do-execute drs)
    (invoke-unit/sig
     (load jr-input)
     test-environment^)]))

(run-test language-level/beginner)
(run-test language-level/intermediate)
(run-test language-level/advanced)
(run-test "Graphical (MrEd)" "Graphical (MrEd)")
(run-test "Textual (MzScheme)" "Textual (MzScheme)")
