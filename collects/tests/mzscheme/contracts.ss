(load-relative "loadtest.ss")
(require (lib "specs.ss" "framework")
	 (lib "class.ss"))
  
(SECTION 'contracts)

(parameterize ([error-print-width 200])
(let ()
  ;; test/spec-passed : symbol sexp -> void
  ;; tests a passing specification
  (define (test/spec-passed name expression)
    (printf "testing: ~s\n" name)
    (test (void)
          (let ([for-each-eval (lambda (l) (for-each eval l))]) for-each-eval)
          (list expression '(void))))
  
  (define (test/spec-passed/result name expression result)
    (printf "testing: ~s\n" name)
    (test result
          eval
          expression))
  
  ;; test/spec-failed : symbol sexp string -> void
  ;; tests a failing specification with blame assigned to `blame'
  (define (test/spec-failed name expression blame)
    (define (ensure-contract-failed x)
      (let ([result (with-handlers ([(lambda (x) (and (not-break-exn? x) (exn? x)))
                                     exn-message])
                      (list 'normal-termination
                            (eval x)))])
        (if (string? result)
            (cond
              [(regexp-match ": ([^ ]*) broke" result) => cadr]
              [(regexp-match "([^ ]+): .* does not imply" result) => cadr]
              [else (format "no blame in error message: \"~a\"" result)])
            result)))
    (printf "testing: ~s\n" name)
    (test blame
          ensure-contract-failed
          expression))
  
  (test/spec-passed
   'contract-flat1 
   '(contract not #f 'pos 'neg))
  
  (test/spec-failed
   'contract-flat2 
   '(contract not #t 'pos 'neg)
   "pos")
  
  (test/spec-passed
   'contract-arrow-star0a
   '(contract (->* (integer?) (integer?))
              (lambda (x) x)
              'pos
              'neg))
  
  (test/spec-failed
   'contract-arrow-star0b
   '((contract (->* (integer?) (integer?))
               (lambda (x) x)
               'pos
               'neg)
     #f)
   "neg")
  
  (test/spec-failed
   'contract-arrow-star0c
   '((contract (->* (integer?) (integer?))
               (lambda (x) #f)
               'pos
               'neg)
     1)
   "pos")
  
  (test/spec-passed
   'contract-arrow-star1
   '(let-values ([(a b) ((contract (->* (integer?) (integer? integer?))
                                   (lambda (x) (values x x))
                                   'pos
                                   'neg)
                         2)])
      1))
  
  (test/spec-failed
   'contract-arrow-star2
   '((contract (->* (integer?) (integer? integer?))
               (lambda (x) (values x x))
               'pos
               'neg)
     #f)
   "neg")
  
  (test/spec-failed
   'contract-arrow-star3
   '((contract (->* (integer?) (integer? integer?))
               (lambda (x) (values 1 #t))
               'pos
               'neg)
     1)
   "pos")
  
  (test/spec-failed
   'contract-arrow-star4
   '((contract (->* (integer?) (integer? integer?))
               (lambda (x) (values #t 1))
               'pos
               'neg)
     1)
   "pos")
  
  
  (test/spec-passed
   'contract-arrow-star5
   '(let-values ([(a b) ((contract (->* (integer?) 
                                        (listof integer?)
                                        (integer? integer?))
                                   (lambda (x) (values x x))
                                   'pos
                                   'neg)
                         2)])
      1))
  
  (test/spec-failed
   'contract-arrow-star6
   '((contract (->* (integer?) (listof integer?) (integer? integer?))
               (lambda (x) (values x x))
               'pos
               'neg)
     #f)
   "neg")
  
  (test/spec-failed
   'contract-arrow-star7
   '((contract (->* (integer?) (listof integer?) (integer? integer?))
               (lambda (x) (values 1 #t))
               'pos
               'neg)
     1)
   "pos")
  
  (test/spec-failed
   'contract-arrow-star8
   '((contract (->* (integer?) (listof integer?) (integer? integer?))
               (lambda (x) (values #t 1))
               'pos
               'neg)
     1)
   "pos")
  
  (test/spec-passed
   'contract-arrow-star9
   '((contract (->* (integer?) (listof integer?) (integer?))
               (lambda (x . y) 1)
               'pos
               'neg)
     1 2))
  
  (test/spec-failed
   'contract-arrow-star10
   '((contract (->* (integer?) (listof integer?) (integer?))
               (lambda (x . y) 1)
               'pos
               'neg)
     1 2 'bad)
   "neg")

  (test/spec-failed
   'contract-d1
   '(contract (integer? . ->d . (lambda (x) (lambda (y) (= x y))))
              1
              'pos
              'neg)
   "pos")
  
  (test/spec-passed
   'contract-d2
   '(contract (integer? . ->d . (lambda (x) (lambda (y) (= x y))))
              (lambda (x) x)
              'pos
              'neg))
  
  (test/spec-failed
   'contract-d2
   '((contract (integer? . ->d . (lambda (x) (lambda (y) (= x y))))
               (lambda (x) (+ x 1))
               'pos
               'neg)
     2)
   "pos")

  (test/spec-passed
   'contract-arrow1
   '(contract (integer? . -> . integer?) (lambda (x) x) 'pos 'neg))
  
  (test/spec-failed
   'contract-arrow2
   '(contract (integer? . -> . integer?) (lambda (x y) x) 'pos 'neg)
   "pos")
  
  (test/spec-failed
   'contract-arrow3
   '((contract (integer? . -> . integer?) (lambda (x) #f) 'pos 'neg) #t)
   "neg")
  
  (test/spec-failed
   'contract-arrow4
   '((contract (integer? . -> . integer?) (lambda (x) #f) 'pos 'neg) 1)
   "pos")


  (test/spec-passed
   'contract-arrow-any1
   '(contract (integer? . -> . any) (lambda (x) x) 'pos 'neg))
  
  (test/spec-failed
   'contract-arrow-any2
   '(contract (integer? . -> . any) (lambda (x y) x) 'pos 'neg)
   "pos")
  
  (test/spec-failed
   'contract-arrow-any3
   '((contract (integer? . -> . any) (lambda (x) #f) 'pos 'neg) #t)
   "neg")

  (test/spec-passed
   'contract-arrow-star-d1
   '((contract (->d* (integer?) (lambda (arg) (lambda (res) (= arg res))))
               (lambda (x) x)
               'pos
               'neg)
     1))
  
  (test/spec-passed
   'contract-arrow-star-d2
   '(let-values ([(a b)
                  ((contract (->d* (integer?) (lambda (arg) 
                                                (values (lambda (res) (= arg res)) 
                                                        (lambda (res) (= arg res)))))
                             (lambda (x) (values x x))
                             'pos
                             'neg)
                   1)])
      1))
  
  (test/spec-failed
   'contract-arrow-star-d3
   '((contract (->d* (integer?) (lambda (arg) 
                                  (values (lambda (res) (= arg res)) 
                                          (lambda (res) (= arg res)))))
               (lambda (x) (values 1 2))
               'pos
               'neg)
     2)
   "pos")
  
  (test/spec-failed
   'contract-arrow-star-d4
   '((contract (->d* (integer?) (lambda (arg) 
                                  (values (lambda (res) (= arg res)) 
                                          (lambda (res) (= arg res)))))
               (lambda (x) (values 2 1))
               'pos
               'neg)
     2)
   "pos")
  
  (test/spec-passed
   'contract-arrow-star-d5
   '((contract (->d* ()
                     (listof integer?)
                     (lambda (arg) (lambda (res) (= arg res))))
               (lambda (x) x)
               'pos
               'neg)
     1))
  
  (test/spec-passed
   'contract-arrow-star-d6
   '((contract (->d* () 
                     (listof integer?)
                     (lambda (arg) 
                       (values (lambda (res) (= arg res)) 
                               (lambda (res) (= arg res)))))
               (lambda (x) (values x x))
               'pos
               'neg)
     1))
  
  (test/spec-failed
   'contract-arrow-star-d7
   '((contract (->d* () 
                     (listof integer?)
                     (lambda (arg) 
                       (values (lambda (res) (= arg res)) 
                               (lambda (res) (= arg res)))))
               (lambda (x) (values 1 2))
               'pos
               'neg)
     2)
   "pos")
  
  (test/spec-failed
   'contract-arrow-star-d8
   '((contract (->d* ()
                     (listof integer?)
                     (lambda (arg) 
                       (values (lambda (res) (= arg res)) 
                               (lambda (res) (= arg res)))))
               (lambda (x) (values 2 1))
               'pos
               'neg)
     2)
   "pos")
  
  (test/spec-failed
   'contract-case->1
   '(contract (case-> (integer? integer? . -> . integer?) (integer? . -> . integer?))
              (lambda (x) x)
              'pos
              'neg)
   "pos")
  
  (test/spec-failed
   'contract-case->2
   '((contract (case-> (integer? integer? . -> . integer?) (integer? . -> . integer?))
               (case-lambda 
                 [(x y) 'case1]
                 [(x) 'case2])
               'pos
               'neg)
     1 2)
   "pos")
  
  (test/spec-failed
   'contract-case->3
   '((contract (case-> (integer? integer? . -> . integer?) (integer? . -> . integer?))
               (case-lambda 
                 [(x y) 'case1]
                 [(x) 'case2])
               'pos
               'neg)
     1)
   "pos")
  
  (test/spec-failed
   'contract-case->4
   '((contract (case-> (integer? integer? . -> . integer?) (integer? . -> . integer?))
               (case-lambda 
                 [(x y) 'case1]
                 [(x) 'case2])
               'pos
               'neg)
     'a 2)
   "neg")
  
  (test/spec-failed
   'contract-case->5
   '((contract (case-> (integer? integer? . -> . integer?) (integer? . -> . integer?))
               (case-lambda 
                 [(x y) 'case1]
                 [(x) 'case2])
               'pos
               'neg)
     2 'a)
   "neg")
  
  (test/spec-failed
   'contract-case->6
   '((contract (case-> (integer? integer? . -> . integer?) (integer? . -> . integer?))
               (case-lambda 
                 [(x y) 'case1]
                 [(x) 'case2])
               'pos
               'neg)
     #t)
   "neg")
  
  (test/spec-failed
   'contract-d-protect-shared-state
   '(let ([x 1])
      ((contract ((->d (lambda () (let ([pre-x x]) (lambda (res) (= x pre-x)))))
                  . -> .
                  (lambda (x) #t))
                 (lambda (thnk) (thnk))
                 'pos
                 'neg)
       (lambda () (set! x 2))))
   "neg")
 
  (test/spec-failed
   'combo1
   '(let ([cf (contract (case->
                         ((class? . ->d . (lambda (%) (lambda (x) #f))) . -> . void?)
                         ((class? . ->d . (lambda (%) (lambda (x) #f))) boolean? . -> . void?))
                        (letrec ([c% (class object% (super-instantiate ()))]
                                 [f
                                  (case-lambda
                                    [(class-maker) (f class-maker #t)]
                                    [(class-maker b) 
                                     (class-maker c%)
                                     (void)])])
                          f)
                        'pos
                        'neg)])
      (cf (lambda (x%) 'going-to-be-bad)))
   "neg")   

  (test/spec-failed
   'union1
   '(contract (union false?) #t 'pos 'neg)
   "pos")

  (test/spec-passed
   'union2
   '(contract (union false?) #f 'pos 'neg))

  (test/spec-passed
   'union3
   '((contract (union (-> integer? integer?)) (lambda (x) x) 'pos 'neg) 1))
  
  (test/spec-failed
   'union4
   '((contract (union (-> integer? integer?)) (lambda (x) x) 'pos 'neg) #f)
   "neg")
  
  (test/spec-failed
   'union5
   '((contract (union (-> integer? integer?)) (lambda (x) #f) 'pos 'neg) 1)
   "pos")
  
  (test/spec-passed
   'union6
   '(contract (union false? (-> integer? integer?)) #f 'pos 'neg))
  
  (test/spec-passed
   'union7
   '((contract (union false? (-> integer? integer?)) (lambda (x) x) 'pos 'neg) 1))

  (test/spec-passed
   'define/contract1
   '(let ()
      (define/contract i integer? 1)
      i))
  
  (test/spec-failed
   'define/contract2
   '(let ()
      (define/contract i integer? #t)
      i)
   "i")
  
  (test/spec-failed
   'define/contract3
   '(let ()
      (define/contract i (-> integer? integer?) (lambda (x) #t))
      (i 1))
   "i")
  
  (test/spec-failed
   'define/contract4
   '(let ()
      (define/contract i (-> integer? integer?) (lambda (x) 1))
      (i #f))
   "")
  
  (test/spec-failed
   'define/contract5
   '(let ()
      (define/contract i (-> integer? integer?) (lambda (x) (i #t)))
      (i 1))
   "")
  
  (test/spec-passed
   'define/contract6
   '(let ()
      (define/contract contracted-func
                       (string?  string? . -> . string?)
                       (lambda (label t)
                         t))
      (contracted-func
       "I'm a string constant with side effects"
       "ans")))
  
  (test/spec-passed
   'provide/contract1
   '(let ()
      (eval '(module contract-test-suite1 mzscheme
               (require (lib "contracts.ss"))
               (provide/contract (x integer?))
               (define x 1)))
      (eval '(require contract-test-suite1))
      (eval 'x)))
  
  (test/spec-passed
   'provide/contract2
   '(let ()
      (eval '(module contract-test-suite2 mzscheme
               (require (lib "contracts.ss"))
               (provide/contract)))
      (eval '(require contract-test-suite2))))
  
  (test/spec-failed
   'provide/contract3
   '(let ()
      (eval '(module contract-test-suite3 mzscheme
               (require (lib "contracts.ss"))
               (provide/contract (x integer?))
               (define x #f)))
      (eval '(require contract-test-suite3))
      (eval 'x))
   "contract-test-suite3")
  
  (test/spec-passed
   'provide/contract4
   '(let ()
      (eval '(module contract-test-suite4 mzscheme
               (require (lib "contracts.ss"))
               (provide/contract (struct s ((a any?))))
               (define-struct s (a))))
      (eval '(require contract-test-suite4))
      (eval '(list (make-s 1)
                   (s-a (make-s 1))
                   (s? (make-s 1))
                   (set-s-a! (make-s 1) 2)))))
  
  (test/spec-passed
   'provide/contract5
   '(let ()
      (eval '(module contract-test-suite5 mzscheme
               (require (lib "contracts.ss"))
               (provide/contract (struct s ((a any?)))
                                 (struct t ((a any?))))
               (define-struct s (a))
               (define-struct t (a))))
      (eval '(require contract-test-suite5))
      (eval '(list (make-s 1)
                   (s-a (make-s 1))
                   (s? (make-s 1))
                   (set-s-a! (make-s 1) 2)
                   (make-t 1)
                   (t-a (make-t 1))
                   (t? (make-t 1))
                   (set-t-a! (make-t 1) 2)))))
  
  (test/spec-passed
   'provide/contract6
   '(let ()
      (eval '(module contract-test-suite6 mzscheme
               (require (lib "contracts.ss"))
               (provide/contract (struct s ((a any?))))
               (define-struct s (a))))
      (eval '(require contract-test-suite6))
      (eval '(define-struct (t s) ()))))
  
  
  (test/spec-passed/result
   'contract-=>flat1
   '(contract-=> (>=/c 5) (>=/c 10) 1 'badguy)
   1)
  (test/spec-passed/result
   'contract-=>flat2
   '(contract-=> (>=/c 5) (>=/c 10) 12 'badguy)
   12)
  (test/spec-failed
   'contract-=>flat3
   '(contract-=> (>=/c 5) (>=/c 10) 6 'badguy)
   "badguy")

  (test/spec-passed
   'contract-=>->1
   '(contract-=> ((>=/c 10) . -> . (>=/c 5)) ((>=/c 5) . -> . (>=/c 10)) (lambda (x) x) 'badguy))
  
  (test/spec-failed
   'contract-=>->2
   '(contract-=> ((>=/c 10) . -> . (>=/c 5)) ((>=/c 5) . -> . (>=/c 10)) 'not-a-proc 'badguy)
   "badguy")
  
  (test/spec-passed/result
   'contract-=>->3
   '((contract-=> ((>=/c 10) . -> . (>=/c 5)) ((>=/c 5) . -> . (>=/c 10)) (lambda (x) x) 'badguy)
     1)
   1)
  
  (test/spec-passed/result
   'contract-=>->4
   '((contract-=> ((>=/c 10) . -> . (>=/c 5)) ((>=/c 5) . -> . (>=/c 10)) (lambda (x) x) 'badguy)
     12)
   12)
  
  (test/spec-failed
   'contract-=>->5
   '((contract-=> ((>=/c 10) . -> . (>=/c 5)) ((>=/c 5) . -> . (>=/c 5)) (lambda (x) x) 'badguy)
     7)
   "badguy")
  
  (test/spec-failed
   'contract-=>->6
   '((contract-=> ((>=/c 10) . -> . (>=/c 5)) ((>=/c 10) . -> . (>=/c 10)) (lambda (x) 7) 'badguy)
     7)
   "badguy")

  (test/spec-passed
   'contract-=>->*1
   '(contract-=> (->* ((>=/c 10) (>=/c 20)) ((>=/c 3) (>=/c 8)))
                 (->* ((>=/c 3) (>=/c 8)) ((>=/c 10) (>=/c 20)))
                 (lambda (x y) (values x y))
                 'badguy))
  
  (test/spec-failed
   'contract-=>->*2
   '(contract-=> (->* ((>=/c 10) (>=/c 20)) ((>=/c 3) (>=/c 8)))
                 (->* ((>=/c 3) (>=/c 8)) ((>=/c 10) (>=/c 20)))
                 'not-a-proc
                 'badguy)
   "badguy")
  
  (test/spec-passed/result
   'contract-=>->*3
   '(let-values ([(r1 r2)
                  ((contract-=> (->* ((>=/c 10) (>=/c 20)) ((>=/c 3) (>=/c 8)))
                                (->* ((>=/c 3) (>=/c 8)) ((>=/c 10) (>=/c 20)))
                                (lambda (x y) (values x y))
                                'badguy)
                   1 7)])
      r1)
   1)
  
  (test/spec-passed/result
   'contract-=>->*4
   '(let-values ([(r1 r2)
                  ((contract-=> (->* ((>=/c 10) (>=/c 20)) ((>=/c 3) (>=/c 8)))
                                (->* ((>=/c 3) (>=/c 8)) ((>=/c 10) (>=/c 20)))
                                (lambda (x y) (values x y))
                                'badguy)
                   11 21)])
      r1)
   11)
  
  (test/spec-failed
   'contract-=>->*5
   '((contract-=> (->* ((>=/c 10) (>=/c 20)) ((>=/c 3) (>=/c 8)))
                  (->* ((>=/c 3) (>=/c 8)) ((>=/c 10) (>=/c 20)))
                  (lambda (x y) (values x y))
                  'badguy)
     5 21)
   "badguy")
  
  (test/spec-failed
   'contract-=>->*6
   '((contract-=> (->* ((>=/c 10) (>=/c 20)) ((>=/c 3) (>=/c 8)))
                  (->* ((>=/c 3) (>=/c 8)) ((>=/c 10) (>=/c 20)))
                  (lambda (x y) (values x y))
                  'badguy)
     11 10)
   "badguy")
  
  (test/spec-failed
   'contract-=>->*7
   '((contract-=> (->* ((>=/c 10) (>=/c 20)) ((>=/c 3) (>=/c 8)))
                  (->* ((>=/c 3) (>=/c 8)) ((>=/c 10) (>=/c 20)))
                  (lambda (x y) (values 8 25))
                  'badguy)
     11 21)
   "badguy")
  
  (test/spec-failed
   'contract-=>->*8
   '((contract-=> (->* ((>=/c 10) (>=/c 20)) ((>=/c 3) (>=/c 8)))
                  (->* ((>=/c 3) (>=/c 8)) ((>=/c 10) (>=/c 20)))
                  (lambda (x y) (values 15 10))
                  'badguy)
     11 21)
   "badguy")
  
  (test/spec-passed/result
   'contract-=>->*9
   '(let-values ([(a b)
                  ((contract-=> (->* ((>=/c 10) (>=/c 20) (>=/c 30)) ((>=/c 3) (>=/c 8)))
                                (->* ((>=/c 3) (>=/c 8) (>=/c 30)) ((>=/c 10) (>=/c 20)))
                                (lambda (x y z) (values x z))
                                'badguy)
                   101 102 103)])
      b)
   103)
  
  (test/spec-failed
   'contract-=>mismatch
   '(contract-=> (>=/c 5)
                 (-> (>=/c 3) (>=/c 8))
                 1
                 'badguy)
   "badguy")

  (test/spec-passed/result
   'contract-=>->*10
   '((contract-=> (->* ((>=/c 10)) (listof (>=/c 20)) ((>=/c 3)))
                  (->* ((>=/c 3)) (listof (>=/c 8)) ((>=/c 10)))
                  (lambda (x . y) 1)
                  'badguy)
     100
     200
     300)
   1)
  
  (test/spec-failed
   'contract-=>->*11
   '((contract-=> (->* ((>=/c 10)) (listof (>=/c 20)) ((>=/c 3)))
                  (->* ((>=/c 3)) (listof (>=/c 8)) ((>=/c 10)))
                  (lambda (x . y) 1)
                  'badguy)
     7
     200
     300)
   "badguy")
  
  (test/spec-failed
   'contract-=>->*12
   '((contract-=> (->* ((>=/c 10)) (listof (>=/c 20)) ((>=/c 3)))
                  (->* ((>=/c 3)) (listof (>=/c 8)) ((>=/c 10)))
                  (lambda (x . y) 1)
                  'badguy)
     100
     10
     300)
   "badguy")
  
  (test/spec-failed
   'contract-=>->*13
   '((contract-=> (->* ((>=/c 10)) (listof (>=/c 20)) ((>=/c 3)))
                  (->* ((>=/c 3)) (listof (>=/c 8)) ((>=/c 10)))
                  (lambda (x . y) 1)
                  'badguy)
     100
     200
     10)
   "badguy")
  
  (test/spec-failed
   'contract-=>->*14
   '((contract-=> (->* ((>=/c 10)) (listof (>=/c 20)) ((>=/c 3)))
                  (->* ((>=/c 3)) (listof (>=/c 8)) ((>=/c 10)))
                  (lambda (x . y) 5)
                  'badguy)
     100
     200
     300)
   "badguy")
  
  (test/spec-passed/result
   'contract-=>-case->1
   '((contract-=> (case-> (integer? . -> . integer?)) (case-> (integer? . -> . integer?)) (case-lambda [(x) x]) 'badguy) 1)
   1)
  
  (test/spec-passed/result
   'contract-=>-case->2
   '((contract-=> (case->
                   (-> (>=/c 10) (>=/c 3))
                   (-> (>=/c 10) (>=/c 10) (>=/c 3)))
                  (case->
                   (-> (>=/c 3) (>=/c 10))
                   (-> (>=/c 3) (>=/c 3) (>=/c 10)))
                  (case-lambda
                    [(x) x]
                    [(x y) x])
                  'badguy)
     100)
   100)
  
  (test/spec-passed/result
   'contract-=>-case->3
   '((contract-=> (case->
                   (-> (>=/c 10) (>=/c 3))
                   (-> (>=/c 10) (>=/c 10) (>=/c 3)))
                  (case->
                   (-> (>=/c 3) (>=/c 10))
                   (-> (>=/c 3) (>=/c 3) (>=/c 10)))
                  (case-lambda
                    [(x) x]
                    [(x y) x])
                  'badguy)
     100
     200)
   100)
  
  (test/spec-failed
   'contract-=>-case->4
   '((contract-=> (case->
                   (-> (>=/c 10) (>=/c 3))
                   (-> (>=/c 100) (>=/c 100) (>=/c 30)))
                  (case->
                   (-> (>=/c 3) (>=/c 10))
                   (-> (>=/c 30) (>=/c 30) (>=/c 100)))
                  (case-lambda
                    [(x) x]
                    [(x y) x])
                  'badguy)
     8)
   "badguy")
  
  (test/spec-failed
   'contract-=>-case->5
   '((contract-=> (case->
                   (-> (>=/c 10) (>=/c 3))
                   (-> (>=/c 100) (>=/c 100) (>=/c 30)))
                  (case->
                   (-> (>=/c 3) (>=/c 10))
                   (-> (>=/c 30) (>=/c 30) (>=/c 100)))
                  (case-lambda
                    [(x) x]
                    [(x y) x])
                  'badguy)
     80
     80)
   "badguy")
  
  ))

(report-errs)