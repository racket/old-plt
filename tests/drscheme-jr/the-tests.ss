;;; the-tests.ss

;;; N.B. These tests are also run through DrScheme
;;;      If you add or modify a test in this file,
;;;      you need to go to plt/test/drscheme and 
;;;      perform the same operation on lang-level-terms.ss


(define beginner "Beginner")
(define intermediate "Intermediate")
(define advanced "Advanced")
(define r4rs "R4RS+")
(define all-levels (list beginner intermediate advanced r4rs))
(define no-levels null)
(define iar (cdr all-levels))
(define ar  (cddr all-levels))

;; a test consists of  
;; name
;; list of levels at which it is supposed to pass
;; one or more sexp's

'define
all-levels
(define test-define-of-five 5) test-define-of-five

'lambda
all-levels
(lambda (x) 5)

'app
all-levels
(define test-define-f (lambda (x) 5)) 
(test-define-f 3)

'cond-1
all-levels
(cond (#f 3) (#t 5))

'cond-2
all-levels
(cond (#f 3) (else 5))

'cond-3
all-levels
(cond (else 5))

'cond-4
all-levels
(cond (#t 5))

'if-1
all-levels
(if #t 5 3)

'if-2
all-levels
(if #f 3 5)

'quote-1
all-levels
(quote a)

'quote-2
all-levels
'a

'local-1
iar
(local () 5)

'local-2
iar
(local ((define x 5)) x)

'local-3
iar
(local 
 ([define (even? x)
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


'local-4
iar
(local 
 ([define even?
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

'let-1
iar
(let ([x 3])
  (let ([x 2]
	[y x])
    (+ x y)))

'let-2
no-levels
(let ([x (lambda (y) x)])
  (equal? x (x 5)))

'let-3
iar
(let* ([x 3] 
       [z 1])
  (let* ([x 2] 
	 [y x])
    (+ z (+ x y))))

'let-4
no-levels
(let* ([x (lambda (y) x)])
       (equal? x (x 5)))

'letrec-1
iar
(letrec ([x (lambda (y) x)])
  (equal? x (x 5)))

'letrec-2
iar
(letrec ([even?
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

'letrec-3  ; not defined in R4RS, but okay in DrScheme
iar
(letrec ([x 5]
	 [y x])
  x)


'case-sense
all-levels
(not (eq? 'a 'A))

'set!
ar
(let ([x 3])
  (set! x 5)
  x)

'begin
ar
(begin 92 5)

'begin0
ar
(begin0 5 104)

'recur-1
ar
(let f ([x 9] 
	[y 0])
  (if (<= x 4)
      y
      (f (sub1 x) (add1 y))))

'recur-2
ar
(recur f 
       ([x 9] [y 0])
       (if (<= x 4)
	   y
	 (f (sub1 x) (add1 y))))

'recur-3
ar
(let ([g (rec f (lambda (x) f))])
  (equal? g (g 5)))

'do-1
ar
(let ([x '(1 3 5 7 9)])
  (do ([x x (cdr x)]
       [sum 0 (+ sum (car x))])
      ((null? x) (/ sum 5))))

'do-2
ar
(do ([vec (make-vector 5)]
     [i 0 (+ i 1)])
    ((= i 5) vec)
  (vector-set! vec i i))

'call-cc-1
ar
(call/cc (lambda (x)
	   (x 5)
	   ((lambda (x) (x x)) (lambda (x) (x x)))))

'call-cc-2
ar
(let ([b (call/cc (lambda (x) x))])
  (if (number? b)
      b
      (b 5)))

'let-cc-1
ar
(let/cc x
	(x 5)
	((lambda (x) (x x)) (lambda (x) (x x))))

'let-cc-2
ar
(let ([b (let/cc x x)])
  (if (number? b)
      b
      (b 5)))

'when-1
ar
(when #t 4 5)

'when-2
ar
(let ([x 5])
  (when #f 
	(set! x 92)
	x))

'unless-1
ar
(unless #f 92 37 42 5)

'unless-2
ar
(let ([x 5])
  (unless #t (set! x 92))
  x)

'one-armed-if-1
ar
(if #t 5)

'one-armed-if-2
ar
(let ([x 5])
  (if #f (set! x 92))
  x)

'eq?-1
ar
(eq? 5 5)

'quote-num-1
ar
'5

'quote-num-2
ar
(quote 5)

'if-3
ar
(if 5 5 9)

'cond-1
ar
(cond (5 5) (else 9))

'thunk
ar
(let ([x (lambda () 5)])
  (x))

'anonymous-fun-app
ar
((lambda (x) 5) 92)

;; keywords

'keyword-interface
no-levels
#%interface

'keyword-unit
no-levels
#%unit

'keyword-struct
no-levels
#%struct

'keyword-unquote
no-levels
#%unquote

'keyword-let-macro
no-levels
#%let-macro

'keyword-set!
no-levels
#%set!

'keyword-if
no-levels
#%if

'keyword-quote
no-levels
#%quote

'keyword-begin0
no-levels
#%begin0

'keyword-begin
no-levels
#%begin

'keyword-let
no-levels
#%let

'keyword-let*
no-levels
#%let*

'keyword-letrec*
no-levels
#%letrec*

'keyword-letrec
no-levels
#%letrec

'keyword-lambda
no-levels
#%lambda

'keyword-let-expansion-time
no-levels
#%let-expansion-time

'keyword-define-expansion-time
no-levels
#%define-expansion-time

'keyword-invoke-open-unit
no-levels
#%invoke-open-unit

'keyword-invoke-unit
no-levels
#%invoke-unit

'keyword-compound-unit
no-levels
#%compound-unit

'keyword-unquote-splicing
no-levels
#%unquote-splicing

'keyword-let-id-macro
no-levels
#%let-id-macro

'keyword-define-id-macro
no-levels
#%define-id-macro

'keyword-define-macro
no-levels
#%define-macro

'keyword-case-lambda
no-levels
#%case-lambda

'keyword-let-values
no-levels
#%let-values

'keyword-let*-values
no-levels
#%let*-values

'keyword-letrec*-values
no-levels
#%letrec*-values

'keyword-letrec-values
no-levels
#%letrec-values

'keyword-define-values
no-levels
#%define-values

'keyword-cond
no-levels
#%cond

'keyword-class*/names
no-levels
#%class*/names

;; end keywords

'struct-1
iar
(define-struct a (b c))
(list make-a a? a-b a-c set-a-b! set-a-c!)

'struct-2
iar
(define-struct empty-testing-structure ())
(list make-empty-testing-structure empty-testing-structure?)

'struct-3
iar
(define-struct a (b c))
(let ([anA (make-a 5 3)])
  (a-b anA))

'struct-4
iar
(define-struct a (b c))
(let ([anA (make-a 3 4)])
  (let ([z (set-a-b! anA 5)])
    (a-b anA)))

'struct-5
iar
(define-struct a (b c))
(let ([a (make-a 3 4)])
  (if (a? a)
      5
      8))

'delay-1
ar
(delay 5)

'delay-2
ar
(delay (let ([x (lambda (x) (x x))])
	 (x x)))

'delay-3
ar
(let* ([x 5]
       [v (delay x)])
  (force v))

'quote-list-1
ar
(quote (a a a))

'quote-list-2
ar
'(a a a)

'empty-list-1
ar
'()

'empty-list-2
ar
(quote ())

'back-quote-1
ar
`(a a a)

'back-quote-2
ar
`(a ,(car '(a a)) a)

'back-quote-3  
all-levels     
`(a ,@(list 'a 'a))

'quasi-quote-1
all-levels
(quasiquote a)

'quasi-quote-2
ar
(quasiquote (a a a))

'quasi-quote-3
ar
(quasiquote (a (unquote (car (quote (a a)))) a))

'quasi-quote-4
ar
(quasiquote (a ,(car (quote (a a))) a))

'set!-id
no-levels
(set! some-undefined-identifier 5)
some-undefined-identifier

'non-list
r4rs
(cons 3 4)

'fallthrough-cond-1
no-levels
(void? (cond (#f 32)))

'fallthrough-cond-2
no-levels
(void? (case 'a ((b c) 32)))
