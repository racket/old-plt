;; Language levels test suite.

;; There's a little source-position testing here (it checks to make
;; sure a reasonable line is reported).

(define language-level/beginner "Beginning Student")
(define language-level/intermediate "Intermediate Student")
(define language-level/advanced "Advanced Student")
(define language-level/mzscheme/debug "Textual Full Scheme")
     
(load-relative "sig.scm")

(define (go argv)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                      Testing utilities                       ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define exit-on-err? #t)
     
  (define (non-regexp s)
    (list->string
     (apply
      append
      (map
       (lambda (c)
	 (cond 
	  [(memq c '(#\$ #\| #\\ #\[ #\] #\. #\* #\? #\+ #\( #\) #\^))
	   (list #\\ c)]
	  [else (list c)]))
       (string->list s)))))
  
  (define re:stdin-error (regexp "^stdin: ([0-9]*)[.]([0-9]*)-([0-9]*)[.]([0-9]*): "))

  (define-values (jr-out jr-stdout) (make-pipe))
  (define-values (jr-err jr-stderr) (make-pipe))
  (define-values (jr-stdin jr-in) (make-pipe))

  (define jr-custodian (make-custodian))

  (define jr-thread
    (parameterize ([current-custodian jr-custodian])
      (thread
       (lambda ()
	 (current-input-port jr-stdin)
	 (current-output-port jr-stdout)
	 (current-error-port jr-stderr)
	 (exit-handler (lambda (v) (custodian-shutdown-all jr-custodian)))
	
	 ; argv is effectively passed on...
	 (require-library "go.ss" "drscheme-jr")))))
  
  (define error? #f)

  (define (perror s . args)
    (apply printf (format "~n~nERROR~n~a" s) args)
    (set! error? #t)
    (newline)
    (when exit-on-err?
      (exit)))

  (define (expect r s)
    (unless (regexp-match r s)
      (perror "expected ~s, got ~s" r s)))

  (define (expect-prompt)
    (let ([c (read-char out)])
      (unless (char=? c #\>)
	(perror "expected >, got ~s" c)))
    (let ([c (read-char out)])
      (unless (char=? c #\space)
	(perror "expected space, got ~s" c))))

     

  (define (try input _output)
    (define err? (pair? _output))
    (define output (cond
		    [(string? _output)
		     (format "^~a$" (non-regexp _output))]
		    [(pair? _output)
		     (cadr _output)]
		    [else _output]))
    (define start-pos pos)
    (define start-line line)
    (printf "~a~n" input)
    (fprintf in "~a~n" input)
    (unless (eq? 'void output)
      (let ([result (read-line (if err? err out))])
	(expect output result)
	(when err?
	  (let ([m (regexp-match re:stdin-error result)])
	    (if m
		(let ([sline (string->number (list-ref m 1))]
		      [spos (string->number (list-ref m 2))]
		      [eline (string->number (list-ref m 3))]
		      [epos (string->number (list-ref m 4))])
		  ;; we want to allow for a buggy definition followed
		  ;; by its use, so we allow one line of slack
		  (unless (and (<= (sub1 start-line) sline (sub1 line))
			       (<= (sub1 start-line) eline (sub1 line)))
		    (perror "out-of-range lines in error: ~a-~a not in ~a-~a" 
			    sline eline
			    (sub1 start-line) (sub1 line))))
		(perror "expected `stdin' error, got: ~a" result))))))
    (expect-prompt))

  (define (flush-err)
    (read-line err)
    (expect-prompt))

  (define (flush-out)
    (read-line out)
    (expect-prompt))

  (define-values (out copy-out) (make-pipe))
  (define-values (err copy-err) (make-pipe))
  (define-values (copy-in in) (make-pipe))

  (define pos 1)
  (define line 1)

  (define (start-copy src dest track-pos?)
    (thread
     (lambda ()
       (let loop ()
	 (let ([r (read-char src)])
	   (if (eof-object? r)
	       (close-output-port dest)
	       (begin
		 (if track-pos?
		     (if (memq r '(#\newline #\return))
			 (begin
			   (set! line (add1 line))
			   (set! pos 1))
			 (begin
			   (set! pos (add1 pos))))
		     (write-char r))
		 (write-char r dest)
		 (loop))))))))

  (define language
    (if (< 1 (vector-length argv))
	(vector-ref argv 1)
	"MzScheme Debug"))


  (start-copy jr-out copy-out #f)
  (start-copy jr-err copy-err #f)
  (start-copy copy-in jr-in #t)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                            Testing                           ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (expect "DrScheme Jr is loading" (read-line out))
  (expect "Welcome to DrScheme Jr" (read-line out))
  (expect (format "Language: ~a" language)
	  (read-line out))
  (expect-prompt)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (invoke-unit/sig (load-relateive "all-tests.scm")
                   test-environment^)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (printf "~nDone: ~a.~n"
	  (if error?
	      "ERRORS ENCOUNTERED"
	      "no errors"))

  error?)

(let ([go go]
      [errs? #f])
  (set! errs? (go `#("-l" ,language-level/beginner)))
  (set! errs? (or (go `#("-l" ,language-level/intermediate)) errs?))
  (set! errs? (or (go `#("-l" ,language-level/advanced)) errs?))
  (set! errs? (or (go `#("-l" ,language-level/mzscheme/debug)) errs?))
  (when errs?
    (printf "THERE WERE ERRORS~n")))

(exit)
