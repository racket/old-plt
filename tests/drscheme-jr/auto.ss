;; Language levels test suite.

;; The current implementation is specific to DrScheme Jr, but I'll
;; abstract out the right things to make it work for DrScheme, too.

;; There's a little source-position testing here (it checks to make
;; sure a reasonable line is reported).

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
    (apply printf (format "ERROR: ~a" s) args)
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
		  (unless (and (<= start-line sline (sub1 line))
			       (<= start-line eline (sub1 line)))
		    (perror "out-of-range lines in error: ~a-~a not in ~a-~a" 
			    sline eline
			    start-line (sub1 line))))
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

  (define mz? (string=? language "MzScheme Debug"))
  (define beg? (string=? language "Beginner"))
  (define beg/inter? (member language '("Beginner" "Intermediate")))
  (define adv/mz? (member language '("Advanced" "MzScheme Debug")))

  (define print-convert? (not mz?))
  (define case-sens? (not mz?))
  (define cond-bool? beg/inter?)
  (define empty-lambda? adv/mz?)
  (define one-arm-if? adv/mz?)
  (define gen-quote? adv/mz?)
  (define fall-through? mz?)
  (define quasiquote? adv/mz?)
  (define let? (not beg?))
  (define named-let? adv/mz?)
  (define local? (and let? (not mz?)))
  (define imperative? adv/mz?)
  (define symbol-apps? beg/inter?)
  (define proper-list? (not mz?))
  (define define-struct? #t)
  (define explicit-inexact? (not mz?))
  (define abbrev-list? (not beg?))
  (define mzscheme? mz?)

  (define (mk-diff flag?)
    (lambda (x other)
      (if flag? x other)))
	  
  (define pc-diff (mk-diff print-convert?))
  (define el-diff (mk-diff empty-lambda?))
  (define gq-diff (mk-diff gen-quote?))
  (define cs-diff (mk-diff case-sens?))
  (define ft-diff (mk-diff fall-through?))
  (define cb-diff (mk-diff cond-bool?))
  (define oai-diff (mk-diff one-arm-if?))
  (define mz-diff (mk-diff mzscheme?))
  (define sa-diff (mk-diff symbol-apps?))
  (define pl-diff (mk-diff proper-list?))
  (define ds-diff (mk-diff define-struct?))
  (define qq-diff (mk-diff quasiquote?))
  (define nl-diff (mk-diff named-let?))
  (define ei-diff (mk-diff explicit-inexact?))
  (define al-diff (mk-diff abbrev-list?))

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
  (try "\"apple\"" "\"apple\"")
  (try "'apple" (pc-diff "'apple" "apple"))
  (try "(quote apple)" (pc-diff "'apple" "apple"))
  (try "'aPPle" (cs-diff (pc-diff "'aPPle" "aPPle")
			 (pc-diff "'apple" "apple")))
  (try "(eq? 'a 'A)" (cs-diff "#f" "#t"))
  (try "apple" '(error "undefined.*apple$"))
  (try "'5" (gq-diff "5" '(error "'5 is not a symbol")))
  (try "'(apple)" (gq-diff (pc-diff "(list 'apple)" "(apple)")
			   '(error "'[(]apple[)] is not a symbol")))
  (try "'()" (gq-diff (pc-diff "empty" "()")
		      '(error "'[(][)] is not a symbol")))
  (try "()" (mz-diff "()" '(error "Empty combination")))

  (try "`(a a a)" (qq-diff
		   (gq-diff (pc-diff "(list 'a 'a 'a)"
				     "(a a a)")
			    '(error "'[(]a a a[)] is not a symbol"))
		   '(error "illegal use of \"`\"")))
  (unless quasiquote? (flush-err))

  (try "`(a ,(car '(b a)) a)" (qq-diff
			       (gq-diff (pc-diff "(list 'a 'b 'a)"
						 "(a b a)")
					'(error "Misuse of quote"))
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

  (try "4.0" (ei-diff "#i4.0" "4.0"))

  ;; ;;;;;;;;;;;;;;;;;;;;;; prims ;;;;;;;;;;;;;;;;;;;;;;;;;

  (try "(eq? 5 5)" 
       (cb-diff '(error "expected symbols as arguments") "#t"))
       
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

  ;; ;;;;;;;;;;;;;;;;;;;;;; lambda ;;;;;;;;;;;;;;;;;;;;;;;;;

  (try-void "(define test-define-of-five 5)")
  (try "test-define-of-five" "5")

  (try "(lambda (x) 5)" (pc-diff "(lambda (a1) ...)" "#<procedure>"))
  (try "((lambda (x) 592) 92)" (sa-diff '(error "First term after paren")
					"592"))
  (try "(lambda () 5)" (el-diff (pc-diff "(lambda () ...)" "#<procedure>") 
				'(error "at least one argument")))
  (try "((lambda () 5))" (el-diff (sa-diff '(error "First term after paren")
					   "5")
				  '(error "at least one argument")))
  
  (try-void "(define test-define-f (lambda (x) (+ x 5)))")
  (try "(test-define-f 3)" "8")
  (try "(test-define-f 4)" "9")

  ;; ;;;;;;;;;;;;;;;;;; cond ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (try "(cond (#f 3) (#t 51))" "51")
  (try "(cond (#f 3) (else 52))" "52")
  (try "(cond (else 53))" "53")
  (try "(cond (#t 54))" "54")

  (try "(cond (5 5) (else 9))" (cb-diff '(error "neither #t nor #f") "5"))
  (try "(if 5 51 5)" (cb-diff '(error "neither #t nor #f") "51"))

  (try "(void? (cond (#f 32)))" (ft-diff "#t" '(error "no matching")))

  (try "(if #t 5 3)" "5")
  (try "(if (odd? 10) 5 3)" "3")
  (try "(if #f 51 32)" "32")
  (try "(if #t 10)" (oai-diff "10" '(error "must have an else")))
  (try "(lambda (x) (if #f (set! x 92)))"
       (oai-diff (pc-diff "(lambda (a1) ...)" "#<procedure>")
		 '(error "must have an else")))

  ;; ;;;;;;;;;;;;;;;;;; define-struct ;;;;;;;;;;;;;;;;;;;;;

  (try "(define-struct a (b c))"
       (ds-diff 'void '(error "undefined")))
  (try "(a-b (make-a 2 1))"
       (ds-diff "2" '(error "undefined")))
  (try "(length (list struct:a make-a a? a-b a-c set-a-b! set-a-c!))"
       (ds-diff "7" '(error "undefined")))
  
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
	 "55")
    (try '(a? (make-a 3 4)) "#t"))

  ;; ;;;;;;;;;;;;;;;;;; local ;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (if local?

      (begin
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
	     "#t")

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
	     "#t")

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
	     "#t")

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
	     "#t")

	(try '(letrec ([x 5]
		       [y x])
		x)
	     "5")

	(try '(let f ([x 9] 
		      [y 100])
		(if (<= x 4)
		    y
		    (f (sub1 x) (add1 y))))
	     (nl-diff "105"
		      '(error "Malformed let")))

	(try '(recur f ([x 9] [y 500])
		(if (<= x 4)
		    y
		    (f (sub1 x) (add1 y))))
	     (nl-diff (mz-diff '(error "undefined.*: recur$")
			       "505")
		      '(error "First term after paren")))

	(try '(let ([g (rec f (lambda (x) f))])
		(equal? g (g 5)))
	     (nl-diff (mz-diff '(error "undefined.*: rec$")
			       "#t")
		      '(error "undefined.*: rec$")))
	
	'...)

      (try "let" '(error "undefined")))
	
  ;; ;;;;;;;;;;;;;;;;;; when. unless ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (if imperative?

      (begin

	(try '(when #t 50) "50")
	(try-void '(when #f (cons 1 2)))
	(try '(let ([x 5])
		(when #f 
		  (set! x 92))
		x)
	     "5")


	(try '(unless #f 50) "50")
	(try-void '(unless #t (cons 1 2)))
	(try '(let ([x 56])
		(unless #t (set! x 92))
		x)
	     "56")


	'...)

      (begin
	(try "when" '(error "undefined"))
	(try "unless" '(error "undefined"))))
  
  ;; ;;;;;;;;;;;;;;;;;; set!, begin, begin0, do, delay ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (if imperative?

      (begin

	(try '(let ([x 3])
		(set! x 5)
		x)
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
	
	(try '(promise? (delay 5)) "#t")
	(try '(promise? (delay (let ([x (lambda (x) (x x))])
				 (x x))))
	     "#t")
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
  
  ;; ;;;;;;;;;;;;;;;;;; call/cc, let/cc  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (when imperative?
    (try '(call/cc (lambda (x)
		     (x 55)
		     ((lambda (x) (x x)) (lambda (x) (x x)))))
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
		(x 35)
		((lambda (x) (x x)) (lambda (x) (x x))))
	     "35")

	(try '(let ([b (let/cc x x)])
		(if (number? b)
		    b
		    (b 5)))
	     "5")

	'...)

      (begin
	(try "let/cc" '(error "undefined"))))
  
  ;; ;;;;;;;;;;;;;;;;;; keywords  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (let ([kws '(#%interface
	       #%unit #%unit/sig
	       #%invoke-open-unit #%invoke-unit #%compound-unit
	       #%invoke-open-unit/sig #%invoke-unit/sig #%compound-unit/sig
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
       (try kw `(error ,(format "Invalid use of keyword ~a" 
				(non-regexp (symbol->string kw))))))
     kws))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (printf "~nDone: ~a.~n"
	  (if error?
	      "ERRORS ENCOUNTERED"
	      "no errors"))

  error?)

(let ([go go]
      [errs? #f])
  (set! argv #("-l" "Beginner"))
  (set! errs? (go))
  (set! argv #("-l" "Intermediate"))
  (set! errs? (or (go) errs?))
  (set! argv #("-l" "Advanced"))
  (set! errs? (or (go) errs?))
  (set! argv #("-l" "MzScheme Debug"))
  (set! errs? (or (go) errs?))
  (when errs?
    (printf "THERE WERE ERRORS~n")))
