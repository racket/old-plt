;; Language levels test suite.

;; The current implementation is specific to DrScheme Jr, but I'll
;; abstract out the right things to make it work for DrScheme, too.

;; There's a little source-position testing here (it checks to make
;; sure a reasonable line is reported).

(define language-level/beginner
  "Beginning Student")
(define language-level/intermediate
  "Intermediate Student")
(define language-level/advanced
  "Advanced Student")
(define language-level/mzscheme/debug
  "Textual Full Scheme")

(define (go)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                      Testing utilities                       ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    (newline))

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

  (define (try-void input)
    (try input 'void))

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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                       Configurations                         ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define mz? (regexp-match language-level/mzscheme/debug language))
  (define beg? (string=? language language-level/beginner))
  (define int? (string=? language language-level/intermediate))
  (define adv? (string=? language language-level/advanced))
  (define beg/inter? (member language
		       (list language-level/beginner
			 language-level/intermediate)))
  (define beg/inter/adv? (member language
		       (list language-level/beginner
			 language-level/intermediate
			 language-level/advanced)))
  (define adv/mz? (member language
		    (list language-level/advanced
		      language-level/mzscheme/debug)))

  (define print-convert? (not mz?))
  (define case-sens? (not mz?))
  (define cond-bool? beg/inter?)
  (define empty-lambda? adv/mz?)
  (define one-arm-if? adv/mz?)
  (define begin? (not (or beg/inter?)))
  (define gen-quote? (not beg?))
  (define fall-through? mz?)
  (define quasiquote? (not beg?))
  (define let? (not beg?))
  (define named-let? adv/mz?)
  (define local? (and let? (not mz?)))
  (define imperative? adv/mz?)
  (define symbol-apps? beg/inter?)
  (define student-level? beg/inter/adv?)
  (define apply-no-lambda-bound? beg/inter?)
  (define proper-list? (not mz?))
  (define define-struct? #t)
  (define let-struct? (not beg?))
  (define struct-set? (not beg/inter?))
  (define explicit-inexact? (not mz?))
  (define abbrev-list? (not beg?))
  (define mzscheme? mz?)
  (define single-and/or-ok? mz?)
  (define nonbool-and/or-ok? (not beg?))
  (define no-struct-supertype? beg/inter?)
  (define lambda-in-define-only? beg?)

  (define (mk-diff flag?)
    (lambda (x other)
      (if flag? x other)))
	  
  (define begin-diff (mk-diff begin?))
  (define student-diff (mk-diff student-level?))
  (define pc-diff (mk-diff print-convert?))
  (define el-diff (mk-diff empty-lambda?))
  (define gq-diff (mk-diff gen-quote?))
  (define cs-diff (mk-diff case-sens?))
  (define ft-diff (mk-diff fall-through?))
  (define cb-diff (mk-diff cond-bool?))
  (define oai-diff (mk-diff one-arm-if?))
  (define mz-diff (mk-diff mzscheme?))
  (define sa-diff (mk-diff symbol-apps?))
  (define anlb-diff (mk-diff apply-no-lambda-bound?))
  (define pl-diff (mk-diff proper-list?))
  (define ds-diff (mk-diff define-struct?))
  (define ls-diff (mk-diff let-struct?))
  (define struct!-diff (mk-diff struct-set?))
  (define qq-diff (mk-diff quasiquote?))
  (define nl-diff (mk-diff named-let?))
  (define ei-diff (mk-diff explicit-inexact?))
  (define al-diff (mk-diff abbrev-list?))
  (define sao-diff (mk-diff single-and/or-ok?))
  (define nss-diff (mk-diff no-struct-supertype?))
  (define ldo-diff (mk-diff lambda-in-define-only?))

  (define false-string (pc-diff "false" "#f"))
  (define true-string (pc-diff "true" "#t"))

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

  ;; ;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;

  (try "5" "5")
  (try "#t" true-string)
  (try "#f" false-string)
  (try "\"apple\"" "\"apple\"")
  (try "'apple" (pc-diff "'apple" "apple"))
  (try "(quote apple)" (pc-diff "'apple" "apple"))
  (try "'aPPle" (cs-diff (pc-diff "'aPPle" "aPPle")
			 (pc-diff "'apple" "apple")))
  (try "(eq? 'a 'A)" (cs-diff false-string true-string))
  (try "apple" '(error "undefined.*apple$"))
  (try "'5" (gq-diff "5" '(error "'5 is not a symbol")))
  (try "'(apple)" (gq-diff (pc-diff "(list 'apple)" "(apple)")
			   '(error "'[(]apple[)] is not a symbol")))
  (try "'()" (gq-diff (pc-diff "empty" "()")
		      '(error "'[(][)] is not a symbol")))
  (try "()" (mz-diff "()" '(error "empty combination")))

  (try "`(a a a)" (qq-diff
		   (gq-diff (pc-diff "(list 'a 'a 'a)"
				     "(a a a)")
			    '(error "'[(]a a a[)] is not a symbol"))
		   '(error "illegal use of \"`\"")))
  (unless quasiquote? (flush-err))

  (try "`(a ,(car '(b a)) a)" (qq-diff
			       (gq-diff (pc-diff "(list 'a 'b 'a)"
						 "(a b a)")
					'(error "is not a symbol"))
			       '(error "illegal use of \"`\"")))
  (unless quasiquote? 
    (flush-err)(flush-err)(flush-err)(flush-err))

  (try "'(1 . 2)" (pl-diff '(error "improper lists are not allowed")
		           (pc-diff "(cons 1 2)"
				    "(1 . 2)")))
  (when proper-list?
    (flush-out)
    (flush-err))
  
  (try "null" (pc-diff "empty" "()"))
  (try "(cons 1 null)" (al-diff (pc-diff "(list 1)" "(1)")
				"(cons 1 empty)"))
  (try "(cons 2 (cons 1 null))" (al-diff (pc-diff "(list 2 1)" "(2 1)")
					 "(cons 2 (cons 1 empty))"))

  (try "4.0" (ei-diff "4" "4.0"))

  ;; ;;;;;;;;;;;;;;;;;;;;;; prims ;;;;;;;;;;;;;;;;;;;;;;;;;

  (try "(cons 3 4)"
       (pl-diff '(error "second argument must be of type <list>")
		(pc-diff "(cons 3 4)" "(3 . 4)")))

  ;; ;;;;;;;;;;;;;;;;;;;; definitions ;;;;;;;;;;;;;;;;;;;;;;;;;;

  (try "(set! some-undefined-identifier 5)"
       `(error ,(if imperative?
		    "cannot set undefined identifier"
		    "reference to undefined identifier")))
  (try "some-undefined-identifier"
       '(error "reference to undefined identifier"))

  ;; ;;;;;;;;;;;;;;;;;;;;;; applications ;;;;;;;;;;;;;;;;;;;

  (try "(17 92)" (sa-diff '(error "must be a function name")
			  '(error "application")))

  (try "((car (list car cdr)) (list 1 2))"
    (sa-diff '(error "must be a function name") "1"))

  (try "(define (f x) (x 3))"
    (anlb-diff '(error "is a function-bound") 'void))

  (if local?
    (try "(local ([define f car]) (f (list 1 2)))"
      "1")
    )


  ;; ;;;;;;;;;;;;;;;;;;;;;; lambda ;;;;;;;;;;;;;;;;;;;;;;;;;

  (try-void "(define test-define-of-five 5)")
  (try "test-define-of-five" "5")

  (try "(lambda (x) 5)" (ldo-diff '(error "only in a definition")
				  (pc-diff "(lambda (a1) ...)" "#<procedure>")))
  (try "((lambda (x) 592) 92)" (ldo-diff '(error "only in a definition")
					 (sa-diff '(error "must be a function name")
						  "592")))
  (try "(lambda () 5)" (ldo-diff '(error "only in a definition")
				 (el-diff (pc-diff "(lambda () ...)" "#<procedure>") 
					  '(error "at least one argument"))))
  (try "(define (func) 5)" (el-diff 'void
				    '(error "at least one argument")))
  (try "(define (testing-implicit-begin-define x) 5 6)"
    (mz-diff 'void '(error "must have exactly one")))
  (try "(define (testing-no-body-define x))"
    (mz-diff '(error "malformed definition") '(error "must have exactly one")))
  (try "((lambda () 5))" (ldo-diff '(error "only in a definition")
				   (el-diff (sa-diff '(error "must paren")
						     "5")
					    '(error "at least one argument"))))
  
  (try "(case-lambda [(x) 10])" (ldo-diff '(error "only in a definition")
					  (pc-diff "(lambda (a1) ...)" "#<procedure>")))

  (try "lambda" '(error "invalid use of keyword"))
  (try "case-lambda" '(error "invalid use of keyword"))

  (try-void "(define test-define-f (lambda (x) (+ x 5)))")
  (try "(test-define-f 3)" "8")
  (try "(test-define-f 4)" "9")
  (try "(define (f x) (define (g 3) 4))"
    (student-diff '(error "must be at the top level")
      '(error "internal definition")))

  ;; ;;;;;;;;;;;;;;;;;; cond ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (try "(cond (#f 3) (#t 51))" "51")
  (try "(cond (#f 3) (else 52))" "52")
  (try "(cond (else 53))" "53")
  (try "(cond (#t 54))" "54")

  (try "(cond (5 5) (else 9))" (cb-diff '(error "neither true nor false") "5"))
  (try "(if 5 51 5)" (cb-diff '(error "neither true nor false") "51"))

  (try "(void? (cond (#f 32)))" (ft-diff true-string '(error "no matching")))

  (try "(if #t 5 3)" "5")
  (try "(if (odd? 10) 5 3)" "3")
  (try "(if #f 51 32)" "32")
  (try "(if #t 10)" (oai-diff "10" '(error "must have an else")))
  (try "(define (y x) (if #f (void)))"
       (oai-diff 'void
		 '(error "must have an else")))

  ;; ;;;;;;;;;;;;;;;;;; define-struct, let-struct ;;;;;;;;;;;;;;;;;;;;;

  (try "(define-struct a (b c))"
       (ds-diff 'void '(error "undefined")))
  (try "(a-b (make-a 2 1))"
       (ds-diff "2" '(error "undefined")))
  (try "(length (list struct:a make-a a? a-b a-c))"
       (ds-diff "5" '(error "undefined")))
  (try "(length (list struct:a make-a a? a-b a-c set-a-b! set-a-c!))"
       (struct!-diff "7" '(error "undefined")))

  (try "(let-struct a (x) 3)"
    (ls-diff "3" '(error "undefined")))
  (try "(let-struct a (x) (set-a-x! (make-a 3) 4))"
    (ls-diff (struct!-diff 'void '(error "undefined"))
      '(error "undefined")))
  (try "(let-struct (a struct:exn) (x) 3)"
    (ls-diff (nss-diff '(error "not an identifier") "3")
      '(error "undefined")))
  (try "(let-struct a (x) 1 2)"
    (ls-diff (student-diff '(error "malformed") "2")
      '(error "undefined")))
  
  (try "(local ((define-struct aa (bb cc))) (procedure? aa-bb))"
       (if local? 
	   (ds-diff true-string '(error "undefined"))
	   '(error "definition")))
  (try "(local ((define-struct aa (bb cc))) (procedure? set-aa-bb!))"
       (if local? 
	   (struct!-diff true-string '(error "undefined"))
	   '(error "definition")))

  (try "(define-struct empty-testing-structure ())"
       (ds-diff 'void '(error "Empty combination")))
  (try '(length (list struct:empty-testing-structure
		      make-empty-testing-structure
		      empty-testing-structure?))
       (ds-diff "3" '(error "undefined")))
  
  (when (and let? define-struct?)
    (try '(let ([anA (make-a 5 3)])
	    (a-b anA))
	 "5")

    (try '(let ([anA (make-a 3 4)])
	    (let ([z (set-a-b! anA 55)])
	      (a-b anA)))
	 (struct!-diff "55" '(error "undefined")))
    (try '(a? (make-a 3 4)) true-string))

  (try "(define-struct (a 0) (b c))"
       (nss-diff '(error "ot an identifier")
		 '(error "not a struct type value")))
  
  ;; ;;;;;;;;;;;;;;;;;; local ;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (if local?

      (begin

	(unless mz?
	  (try "(local () 5 6)" '(error "local: malformed")))

	(try "(local () 5)" "5")
	(try "(local ((define x 56)) x)" "56")

	(try '(local ([define (even? x)
			(if (zero? x)
			    #t
			    (odd? (sub1 x)))]
		      [define (odd? x)
			(if (zero? x)
			    #f
			    (even? (sub1 x)))])
		     (and (not (even? 9))
			  (odd? 9)
			  (even? 10)
			  (not (odd? 10))))
	     true-string)

	(try '(local ([define even?
			(lambda (x)
			  (if (zero? x)
			      #t
			      (odd? (sub1 x))))]
		      [define odd?
			(lambda (x)
			  (if (zero? x)
			      #f
			      (even? (sub1 x))))])
		     (and (not (even? 9))
			  (odd? 9)
			  (even? 10)
			  (not (odd? 10))))
	     true-string)

	'...)

      (try "local" '(error "undefined")))
  
  ;; ;;;;;;;;;;;;;;;;;; let ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (if let?

      (begin
	(try "(let () 5)" "5")
	(try '(let ([x 3])
		(let ([x 2]
		      [y x])
		  (+ x y)))
	     "5")

	(try '(let ([x (lambda (y) x)])
		(equal? x (x 5)))
	     '(error "undefined identifier: x$"))

	(try '(let* ([x 3] 
		     [z 1])
		(let* ([x 2] 
		       [y x])
		  (+ z (+ x y))))
	     "5")
	(try '(let* ([x (lambda (y) x)])
		(equal? x (x 5)))
	     '(error "undefined identifier: x$"))

	(try '(letrec ([x (lambda (y) x)])
		(equal? x (x 5)))
	     true-string)

	(try '(letrec ([even?
			(lambda (x)
			  (if (zero? x)
			      #t
			      (odd? (sub1 x))))]
		       [odd?
			(lambda (x)
			  (if (zero? x)
			      #f
			      (even? (sub1 x))))])
		(and (not (even? 9))
		     (odd? 9)
		     (even? 10)
		     (not (odd? 10))))
	     true-string)

	(try '(letrec ([x 5]
		       [y x])
		x)
	     "5")

	(try '(let loop ([x 10])
		(if (zero? x) 1
		  (* x (loop (sub1 x)))))
	  (nl-diff "3628800"
	    '(error "let: malformed")))

	(try '(let f ([x 9] 
		      [y 100])
		(if (<= x 4)
		    y
		    (f (sub1 x) (add1 y))))
	     (nl-diff "105"
		      '(error "let: malformed")))

	(try '(recur f ([x 9] [y 500])
		(if (<= x 4)
		    y
		    (f (sub1 x) (add1 y))))
	     (nl-diff (mz-diff '(error "undefined.*: recur$")
			       "505")
		      '(error "must be a function name")))

	(try '(let ([g (rec f (lambda (x) f))])
		(equal? g (g 5)))
	     (nl-diff (mz-diff '(error "undefined.*: rec$")
			       true-string)
		      '(error "undefined.*: rec$")))
	
	'...)

      (try "let" '(error "undefined")))
	
  ;; ;;;;;;;;;;;;;;;;;; when, unless ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (if imperative?

      (begin

	(try '(when #t 50) "50")
	(try-void '(when #f (cons 1 2)))
	(try '(let ([x 5])
		(let ([dummy (when #f 
			       (set! x 92))])
		  x))
	     "5")


	(try '(unless #f 50) "50")
	(try-void '(unless #t (cons 1 2)))
	(try '(let ([x 56])
		(let ([dummy (unless #t (set! x 92))])
		  x))
	     "56")


	'...)

      (begin
	(try "when" '(error "undefined"))
	(try "unless" '(error "undefined"))))
  
  ;; ;;;;;;;;;;;;;;;;;; and, or ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (try '(and #t #t) true-string)
  (try '(and #t #f) false-string)
  (try '(and #f #t) false-string)
  (try '(and #f #f) false-string)
  (try '(or #t #t) true-string)
  (try '(or #t #f) true-string)
  (try '(or #f #t) true-string)
  (try '(or #f #f) false-string)

  (try '(and) (sao-diff true-string '(error "and: malformed")))
  (try '(or) (sao-diff false-string '(error "or: malformed")))
    
  (try '(and #t) (sao-diff true-string '(error "and: malformed")))
  (try '(or #t) (sao-diff true-string '(error "or: malformed")))
  (try '(and #f) (sao-diff false-string '(error "and: malformed")))
  (try '(or #f) (sao-diff false-string '(error "or: malformed")))

  (try '(and 7) (sao-diff "7" '(error "and: malformed")))
  (try '(or 14) (sao-diff "14" '(error "or: malformed")))

  (try '(and 9 7) (cb-diff '(error "neither true nor false") "7"))
  (try '(or 9 7) (cb-diff '(error "neither true nor false") "9"))
  (try '(and #t 7) (cb-diff '(error "neither true nor false") "7"))
  (try '(or #f 7) (cb-diff '(error "neither true nor false") "7"))
  (try '(and #f 7) false-string)
  (try '(or #t 7) true-string)
  (try '(and 1 #f #f) (cb-diff '(error "neither true nor false") false-string))
  (try '(or 3 #t #t) (cb-diff '(error "neither true nor false") "3"))

  ;; ;;;;;;;;;;;;;;;;;; set!, begin, begin0, do, delay ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (if adv?

    (begin

      (try '(lambda (x) (set! x 5))
	'(error "cannot mutate procedure-bound"))

      (try '(local ([define x 5]) (set! x 4))
	'void)

      ))

  (if imperative?

      (begin

	(try '(let ([x 3])
		(begin
		  (set! x 5)
		  x))
	     "5")

	(try '(begin (cons 2 null) 5) "5")
	(try '(begin0 512 (cons 3 null)) "512")

	(try '(let ([x '(1 3 5 7 9)])
		(do ([x x (cdr x)]
		     [sum 0 (+ sum (car x))])
		    ((null? x) (/ sum 5))))
	     "5")

	(try '(do ([vec (make-vector 5)]
		   [i 0 (+ i 1)])
		  ((= i 5) vec)
		(vector-set! vec i i))
	     (pc-diff "(vector 0 1 2 3 4)"
		      "#5(0 1 2 3 4)"))
	
	(try '(promise? (delay 5)) true-string)
	(try '(promise? (delay (let ([x (lambda (x) (x x))])
				 (x x))))
	     true-string)
	(try '(let* ([x 5]
		     [v (delay x)])
		(force v))
	     "5")



	'...)

      (begin
	(try "set!" '(error "undefined"))
	(try "begin" '(error "undefined"))
	(try "begin0" '(error "undefined"))
	(try "do" '(error "undefined"))
	(try "delay" '(error "undefined"))))
 
  (try-void "(define (intl-beg x) (begin (+ x 1) (+ x 2)))")
  (try "(intl-beg 3)"
    (begin-diff "5" '(error "undefined")))
 
  ;; ;;;;;;;;;;;;;;;;;; call/cc, let/cc  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (when imperative?
    (try '(call/cc (lambda (x)
		     (begin
		       (x 55)
		       ((lambda (x) (x x)) (lambda (x) (x x))))))
	 "55"))

  (when let?
    (try '(let ([b (call/cc (lambda (x) x))])
	    (if (number? b)
		b
		(b 15)))
	 "15"))

  
  (if imperative?

      (begin
	
	(try '(let/cc x
		(begin
		  (x 35)
		  ((lambda (x) (x x)) (lambda (x) (x x)))))
	     "35")

	(try '(let ([b (let/cc x x)])
		(if (number? b)
		    b
		    (b 5)))
	     "5")

	'...)

      (begin
	(try "let/cc" '(error "undefined"))))
  
  ;; ;;;;;;;;;;;;;;;;;; time ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (if beg?
      (try '(time 1)
	'(error "reference to undefined identifier"))
    (try '(let ([x (time 1)]) (void))
	 (regexp "^cpu time: [0-9]* real time: [0-9]* gc time: [0-9]*$")))

  ;; ;;;;;;;;;;;;;;;;;; keywords  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (let ([kws '(#%interface
	       #%unit #%unit/sig
	       #%invoke-unit #%compound-unit
	       #%invoke-unit/sig #%compound-unit/sig
	       #%struct #%define-struct
	       #%quote #%quasiquote #%unquote #%unquote-splicing
	       #%define-expansion-time #%let-expansion-time
	       #%define-id-macro #%let-id-macro
	       #%define-macro #%let-macro
	       #%let #%let* #%letrec
	       #%let-values #%let*-values #%letrec-values
	       #%set! #%if #%begin0 #%begin #%when #%unless
	       #%lambda #%case-lambda
	       #%define-values #%define
	       #%cond #%case
	       #%class #%class* #%class*/names)])
    (for-each
     (lambda (kw)
       (try kw `(error ,(format "invalid use of keyword ~a" 
				(non-regexp (symbol->string kw))))))
     kws))

   (try '(define #%define 10)
        '(error "invalid use of keyword"))
   (try '(set! #%define 10)
        '(error "invalid use of keyword"))
 
   (try '(define (x define) define)
        (if mz?
 	   'void
 	   '(error "invalid use of keyword")))
 
   (try '(define define 10)
        (if mz?
 	   'void
 	   '(error "invalid use of keyword")))
   (try 'define
        (if mz?
 	   "10"
 	   '(error "invalid use of keyword")))
 
   (try '(set! lambda 12)
        (if mz?
 	   'void
 	   '(error "invalid use of keyword")))
   (try 'lambda
        (if mz?
 	   "12"
 	   '(error "invalid use of keyword")))

  (try '(define #%define 10)
       '(error "invalid use of keyword"))
  (try '(set! #%define 10)
       '(error "invalid use of keyword"))

  ;; ;;;;;;;;;;;;;;;;;; macro mappings  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (try '(eval (list 'define-macro 'if 'void) (make-namespace)) 'void)
  (try '(if #t 1 2) "1")
  (try '(#%if #t 1 2) "1")

  (try '(#%define (cond-macro x) x) 'void)
  (try '(define-macro cond cond-macro) 'void)
  (try '(cond 12) "12")
  (try '(#%cond 12) '(error "clause is not in question-answer format"))
  (try '(eval (list 'cond 12) (make-namespace)) '(error "clause is not in question-answer format"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (printf "~nDone: ~a.~n"
	  (if error?
	      "ERRORS ENCOUNTERED"
	      "no errors"))

  error?)

(let ([go go]
      [errs? #f])
  (set! argv `#("-l" ,language-level/beginner))
  (set! errs? (go))
  (set! argv `#("-l" ,language-level/intermediate))
  (set! errs? (or (go) errs?))
  (set! argv `#("-l" ,language-level/advanced))
  (set! errs? (or (go) errs?))
  (set! argv `#("-l" ,language-level/mzscheme/debug))
  (set! errs? (or (go) errs?))
  (when errs?
    (printf "THERE WERE ERRORS~n")))

(exit)