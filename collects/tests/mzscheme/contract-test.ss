(load-relative "loadtest.ss")
(require (lib "contract.ss")
	 (lib "class.ss"))
  
(SECTION 'contract)

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
  #;
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
  
  (define (test/spec-failed name expression blame)
    (define (has-proper-blame? msg)
      (equal?
       blame
       (cond
         [(regexp-match ": ([^ ]*) broke" msg) => cadr]
         [(regexp-match "([^ ]+): .* does not imply" msg) => cadr]
         [else (format "no blame in error message: \"~a\"" msg)])))
    (printf "testing: ~s\n" name)
    (thunk-error-test 
     (lambda () (eval expression))
     (datum->syntax-object #'here expression)
     (lambda (exn)
       (and (exn? exn)
            (has-proper-blame? (exn-message exn))))))
  
  (define (test/well-formed stx)
    (test (void) 
          (let ([expand/ret-void (lambda (x) (expand x) (void))]) expand/ret-void)
          stx))
  
  (define (test/no-error sexp)
    (test (void)
          eval
          `(begin ,sexp (void))))
  
  (define (test-flat-contract contract pass fail)
    (let ([name (if (pair? contract)
                    (car contract)
                    contract)])
      (test/spec-failed (format "~a fail" name)
                        `(contract ,contract ',fail 'pos 'neg)
                        "pos")
      (test/spec-passed/result
       (format "~a pass" name)
       `(contract ,contract ',pass 'pos 'neg)
       pass)))
  
  (test/spec-passed
   'contract-flat1 
   '(contract not #f 'pos 'neg))
  
  (test/spec-failed
   'contract-flat2 
   '(contract not #t 'pos 'neg)
   "pos")
  
  (test/no-error '(-> integer? integer?))
  (test/no-error '(-> (flat-contract integer?) (flat-contract integer?)))
  (test/no-error '(-> integer? any))
  (test/no-error '(-> (flat-contract integer?) any))
  
  (test/no-error '(->* (integer?) (integer?)))
  (test/no-error '(->* (integer?) integer? (integer?)))
  (test/no-error '(->* (integer?) integer? any))
  (test/no-error '(->* ((flat-contract integer?)) ((flat-contract integer?))))
  (test/no-error '(->* ((flat-contract integer?)) (flat-contract integer?) ((flat-contract integer?))))
  (test/no-error '(->* ((flat-contract integer?)) (flat-contract integer?) any))
  
  (test/no-error '(->d integer? (lambda (x) integer?)))
  (test/no-error '(->d (flat-contract integer?) (lambda (x) (flat-contract integer?))))

  (test/no-error '(->d* (integer?) (lambda (x) integer?)))
  (test/no-error '(->d* ((flat-contract integer?)) (lambda (x) (flat-contract integer?))))
  (test/no-error '(->d* (integer?) integer? (lambda (x) integer?)))
  (test/no-error '(->d* ((flat-contract integer?)) (flat-contract integer?) (lambda (x) (flat-contract integer?))))
  
  (test/no-error '(opt-> (integer?) (integer?) integer?))
  (test/no-error '(opt-> ((flat-contract integer?)) ((flat-contract integer?)) (flat-contract integer?)))
  (test/no-error '(opt->* (integer?) (integer?) (integer?)))
  (test/no-error '(opt->* ((flat-contract integer?)) ((flat-contract integer?)) ((flat-contract integer?))))
  
  (test/no-error '(listof any?))
  (test/no-error '(listof (lambda (x) #t)))
  
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
  
  (test/spec-passed
   'contract-arrow-star11
   '(let-values ([(a b) ((contract (->* (integer?) 
                                        (listof integer?)
					any)
                                   (lambda (x) (values x x))
                                   'pos
                                   'neg)
                         2)])
      1))
  
  (test/spec-failed
   'contract-arrow-star12
   '((contract (->* (integer?) (listof integer?) any)
               (lambda (x) (values x x))
               'pos
               'neg)
     #f)
   "neg")
  
  (test/spec-passed
   'contract-arrow-star13
   '((contract (->* (integer?) (listof integer?) any)
               (lambda (x . y) 1)
               'pos
               'neg)
     1 2))
  
  (test/spec-failed
   'contract-arrow-star14
   '((contract (->* (integer?) (listof integer?) any)
               (lambda (x . y) 1)
               'pos
               'neg)
     1 2 'bad)
   "neg")
  
  (test/spec-passed
   'contract-arrow-star15
   '(let-values ([(a b) ((contract (->* (integer?) any)
                                   (lambda (x) (values x x))
                                   'pos
                                   'neg)
                         2)])
      1))
  
  (test/spec-passed
   'contract-arrow-star14
   '((contract (->* (integer?) any)
               (lambda (x) x)
               'pos
               'neg)
     2))
  
  (test/spec-failed
   'contract-arrow-star16
   '((contract (->* (integer?) any)
               (lambda (x) (values x x))
               'pos
               'neg)
     #f)
   "neg")

  (test/spec-passed
   'contract-arrow-values1
   '(let-values ([(a b) ((contract (-> integer? (values integer? integer?))
                                   (lambda (x) (values x x))
                                   'pos
                                   'neg)
                         2)])
      1))
  
  (test/spec-failed
   'contract-arrow-values2
   '((contract (-> integer? (values integer? integer?))
               (lambda (x) (values x x))
               'pos
               'neg)
     #f)
   "neg")
  
  (test/spec-failed
   'contract-arrow-values3
   '((contract (-> integer? (values integer? integer?))
               (lambda (x) (values 1 #t))
               'pos
               'neg)
     1)
   "pos")
  
  (test/spec-failed
   'contract-arrow-values4
   '((contract (-> integer? (values integer? integer?))
               (lambda (x) (values #t 1))
               'pos
               'neg)
     1)
   "pos")
  
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
 
  #;
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
   'define/contract7
   '(let ()
      (eval '(module contract-test-suite-define1 mzscheme
               (require (lib "contract.ss"))
               (define/contract x string? "a")
               x))
      (eval '(require contract-test-suite-define1))))
  
  (test/spec-passed
   'provide/contract1
   '(let ()
      (eval '(module contract-test-suite1 mzscheme
               (require (lib "contract.ss"))
               (provide/contract (x integer?))
               (define x 1)))
      (eval '(require contract-test-suite1))
      (eval 'x)))
  
  (test/spec-passed
   'provide/contract2
   '(let ()
      (eval '(module contract-test-suite2 mzscheme
               (require (lib "contract.ss"))
               (provide/contract)))
      (eval '(require contract-test-suite2))))
  
  (test/spec-failed
   'provide/contract3
   '(let ()
      (eval '(module contract-test-suite3 mzscheme
               (require (lib "contract.ss"))
               (provide/contract (x integer?))
               (define x #f)))
      (eval '(require contract-test-suite3))
      (eval 'x))
   "contract-test-suite3")
  
  (test/spec-passed
   'provide/contract4
   '(let ()
      (eval '(module contract-test-suite4 mzscheme
               (require (lib "contract.ss"))
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
               (require (lib "contract.ss"))
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
               (require (lib "contract.ss"))
               (provide/contract (struct s ((a any?))))
               (define-struct s (a))))
      (eval '(require contract-test-suite6))
      (eval '(define-struct (t s) ()))))

  #|
  (test/spec-passed/result
   'class-contract1
   '(send
     (make-object (contract (class-contract (public m (integer? . -> . integer?)))
                            (class object% (define/public (m x) x) (super-instantiate ()))
                            'pos
                            'neg))
     m
     1)
   1)
  
  (test/spec-failed
   'class-contract2
   '(contract (class-contract (public m (integer? . -> . integer?)))
              object%
              'pos
              'neg)
   "pos")
  
  (test/spec-failed
   'class-contract3
   '(send
     (make-object (contract (class-contract (public m (integer? . -> . integer?)))
                            (class object% (define/public (m x) x) (super-instantiate ()))
                            'pos
                            'neg))
     m
     'x)
   "neg")
  
  (test/spec-failed
   'class-contract4
   '(send
     (make-object (contract (class-contract (public m (integer? . -> . integer?)))
                            (class object% (define/public (m x) 'x) (super-instantiate ()))
                            'pos
                            'neg))
     m
     1)
   "pos")
  
  (test/spec-passed 
   'class-contract/prim
   '(make-object 
        (class (contract (class-contract/prim)
                         (class object% (init x) (init y) (init z) (super-make-object))
                         'pos-c
                         'neg-c)
          (init-rest x)
          (apply super-make-object x))
      1 2 3))
  
  (test/spec-passed/result
   'object-contract1
   '(send
     (contract (object-contract (m (integer? . -> . integer?)))
               (make-object (class object% (define/public (m x) x) (super-instantiate ())))
               'pos
               'neg)
     m
     1)
   1)
  
  (test/spec-failed
   'object-contract2
   '(contract (object-contract (m (integer? . -> . integer?)))
              (make-object object%)
              'pos
              'neg)
   "pos")
  
  (test/spec-failed
   'object-contract3
   '(send
     (contract (object-contract (m (integer? . -> . integer?)))
               (make-object (class object% (define/public (m x) x) (super-instantiate ())))
               'pos
               'neg)
     m
     'x)
   "neg")
  
  (test/spec-failed
   'object-contract4
   '(send
     (contract (object-contract (m (integer? . -> . integer?)))
               (make-object (class object% (define/public (m x) 'x) (super-instantiate ())))
               'pos
               'neg)
     m
     1)
   "pos")
  
  (test/spec-passed/result
   'object-contract=>1
   '(let* ([c% (class object% (super-instantiate ()))]
           [c (make-object c%)]
           [wc (contract (object-contract) c 'pos-c 'neg-c)]
           [d% (class c% (super-instantiate ()))]
           [d (make-object d%)]
           [wd (contract (object-contract) d 'pos-d 'neg-d)])
      (list (is-a? c c%)
            (is-a? wc c%)
            (is-a? d c%)
            (is-a? wd c%)
            (interface-extension? (object-interface d) (object-interface c))))
   (list #t #t #t #t #t))
  
  (test/spec-passed
   'recursive-object1
   '(letrec ([cc (object-contract (m (-> dd dd)))]
             [dd (object-contract (m (-> cc cc)))]
             [% (class object% (define/public (m x) x) (super-instantiate ()))]
             [c (contract cc (make-object %) 'c-pos 'c-neg)]
             [d (contract dd (make-object %) 'd-pos 'd-neg)])
      (send c m d)
      (send d m c)))
  
  (test/spec-failed
   'recursive-object2
   '(letrec ([cc (object-contract (m (-> dd dd)))]
             [dd (object-contract (n (-> cc cc)))]
             [% (class object% (define/public (m x) x) (define/public (n x) x) (super-instantiate ()))]
             [c (contract cc (make-object %) 'c-pos 'c-neg)]
             [d (contract dd (make-object %) 'd-pos 'd-neg)])
      (send c m c))
   "c-neg")

  (test/spec-passed/result
   'class-contract=>3
   '(let* ([c% (class object% (super-instantiate ()))]
           [wc% (contract (class-contract) c% 'pos-c 'neg-c)]
           [d% (class c% (super-instantiate ()))]
           [wd% (contract (class-contract) d% 'pos-d 'neg-d)])
      (list (subclass? wd% wc%)
            (implementation? wd% (class->interface wc%))
            (is-a? (make-object wd%) wc%)
            (is-a? (make-object wd%) (class->interface wc%))
            (is-a? (instantiate wd% ()) wc%)
            (is-a? (instantiate wd% ()) (class->interface wc%))))
   (list #t #t #t #t #t #t))
   
  (test/spec-passed
   'recursive-class1
   '(letrec ([cc (class-contract (public m (-> dd dd)))]
             [dd (class-contract (public n (-> cc cc)))]
             [c% (contract cc (class object% (define/public (m x) x) (super-instantiate ())) 'c-pos 'c-neg)]
             [d% (contract dd (class object% (define/public (n x) x) (super-instantiate ())) 'd-pos 'd-neg)])
      (send (make-object c%) m d%)
      (send (make-object d%) n c%)))
  
  (test/spec-failed
   'recursive-class1
   '(letrec ([cc (class-contract (public m (-> dd dd)))]
             [dd (class-contract (public n (-> cc cc)))]
             [c% (contract cc (class object% (define/public (m x) x) (super-instantiate ())) 'c-pos 'c-neg)]
             [d% (contract dd (class object% (define/public (n x) x) (super-instantiate ())) 'd-pos 'd-neg)])
      (send (make-object c%) m c%))
   "c-neg")  
|#

  (test/spec-failed
   'immutable1
   '(let ([ct (contract (list-immutableof (boolean? . -> . boolean?)) 
                        #f 
                        'pos
                        'neg)])
      ((car ct) 1))
   "pos")
  
  (test/spec-failed
   'immutable2
   '(let ([ct (contract (list-immutableof (boolean? . -> . boolean?)) 
                        (list (lambda (x) x)) 
                        'pos
                        'neg)])
      ((car ct) 1))
   "pos")
  
  (test/spec-failed
   'immutable3
   '(let ([ct (contract (list-immutableof (number? . -> . boolean?)) 
                        (list-immutable (lambda (x) 1)) 
                        'pos
                        'neg)])
      ((car ct) #f))
   "neg")
  
  (test/spec-failed
   'immutable4
   '(let ([ct (contract (list-immutableof (number? . -> . boolean?)) 
                        (list-immutable (lambda (x) 1)) 
                        'pos
                        'neg)])
      ((car ct) 1))
   "pos")
  
  (test/spec-passed
   'immutable5
   '(let ([ct (contract (list-immutableof (number? . -> . boolean?)) 
                        (list-immutable (lambda (x) #t)) 
                        'pos
                        'neg)])
      ((car ct) 1)))
  

  (test/spec-failed
   'immutable6
   '(contract (cons-immutable/c (boolean? . -> . boolean?) (boolean? . -> . boolean?)) 
              #f 
              'pos
              'neg)
   "pos")
  
  (test/spec-failed
   'immutable7
   '(contract (cons-immutable/c (boolean? . -> . boolean?) (boolean? . -> . boolean?)) 
              (cons (lambda (x) x) (lambda (x) x))
              'pos
              'neg)
   "pos")
  
  (test/spec-failed
   'immutable8
   '(let ([ct (contract (cons-immutable/c (number? . -> . boolean?) (number? . -> . boolean?)) 
                        (cons-immutable (lambda (x) 1) (lambda (x) 1))
                        'pos
                        'neg)])
      ((car ct) #f))
   "neg")
  
  (test/spec-failed
   'immutable9
   '(let ([ct (contract (cons-immutable/c (number? . -> . boolean?) (number? . -> . boolean?)) 
                        (cons-immutable (lambda (x) 1) (lambda (x) 1))
                        'pos
                        'neg)])
      ((cdr ct) #f))
   "neg")
  
  (test/spec-failed
   'immutable10
   '(let ([ct (contract (cons-immutable/c (number? . -> . boolean?) (number? . -> . boolean?)) 
                        (cons-immutable (lambda (x) 1) (lambda (x) 1)) 
                        'pos
                        'neg)])
      ((car ct) 1))
   "pos")
  
  (test/spec-failed
   'immutable11
   '(let ([ct (contract (cons-immutable/c (number? . -> . boolean?) (number? . -> . boolean?)) 
                        (cons-immutable (lambda (x) 1) (lambda (x) 1)) 
                        'pos
                        'neg)])
      ((cdr ct) 1))
   "pos")
  
  (test/spec-passed
   'immutable12
   '(let ([ct (contract (cons-immutable/c (number? . -> . boolean?) (number? . -> . boolean?)) 
                        (cons-immutable (lambda (x) #t) (lambda (x) #t)) 
                        'pos
                        'neg)])
      ((car ct) 1)))
  
  (test/spec-passed
   'immutable13
   '(let ([ct (contract (cons-immutable/c (number? . -> . boolean?) (number? . -> . boolean?)) 
                        (cons-immutable (lambda (x) #t) (lambda (x) #t)) 
                        'pos
                        'neg)])
      ((cdr ct) 1)))
  
  (test/spec-passed/result
   'immutable14
   '(contract (cons-immutable/c number? boolean?) 
              (cons-immutable 1 #t) 
              'pos
              'neg)
   (cons-immutable 1 #t))
  
  (test/spec-failed
   'immutable15
   '(contract (list-immutable/c (number? . -> . boolean?) (number? . -> . boolean?)) 
              #f
              'pos
              'neg)
   "pos")
  
  (test/spec-failed
   'immutable16
   '(contract (list-immutable/c (number? . -> . boolean?) (number? . -> . boolean?)) 
              (list (lambda (x) #t) (lambda (x) #t)) 
              'pos
              'neg)
   "pos")
  
  (test/spec-failed
   'immutable17
   '(contract (list-immutable/c (number? . -> . boolean?) (number? . -> . boolean?)) 
              (list-immutable (lambda (x) #t)) 
              'pos
              'neg)
   "pos")
  
  (test/spec-failed
   'immutable18
   '(contract (list-immutable/c (number? . -> . boolean?) (number? . -> . boolean?)) 
              (list-immutable (lambda (x) #t) (lambda (x) #t) (lambda (x) #t)) 
              'pos
              'neg)
   "pos")
  
  (test/spec-passed
   'immutable19
   '(let ([ctc (contract (list-immutable/c (number? . -> . boolean?) (number? . -> . boolean?)) 
                         (list-immutable (lambda (x) #t) (lambda (x) #t)) 
                         'pos
                         'neg)])
      (for-each (lambda (x) (x 1)) ctc)))

  (test/spec-failed
   'vector-immutable1
   '(contract (vector-immutableof (boolean? . -> . boolean?)) 
              #f 
              'pos
              'neg)
   "pos")
  
  (test/spec-failed
   'vector-immutable2
   '(contract (vector-immutableof (boolean? . -> . boolean?)) 
              (vector (lambda (x) x)) 
              'pos
              'neg)
   "pos")
  
  (test/spec-failed
   'vector-immutable3
   '(let ([ct (contract (vector-immutableof (number? . -> . boolean?)) 
                        (vector->immutable-vector (vector (lambda (x) 1)))
                        'pos
                        'neg)])
      ((vector-ref ct 0) #f))
   "neg")
  
  (test/spec-failed
   'vector-immutable4
   '(let ([ct (contract (vector-immutableof (number? . -> . boolean?)) 
                        (vector->immutable-vector (vector (lambda (x) 1)))
                        'pos
                        'neg)])
      ((vector-ref ct 0) 1))
   "pos")
  
  (test/spec-passed
   'vector-immutable5
   '(let ([ct (contract (vector-immutableof (number? . -> . boolean?)) 
                        (vector->immutable-vector (vector (lambda (x) #t)))
                        'pos
                        'neg)])
      ((vector-ref ct 0) 1)))
  
  (test/spec-failed
   'vector-immutable6
   '(contract (vector-immutable/c (number? . -> . boolean?) (number? . -> . boolean?)) 
              #f
              'pos
              'neg)
   "pos")
  
  (test/spec-failed
   'vector-immutable7
   '(contract (vector-immutable/c (number? . -> . boolean?) (number? . -> . boolean?)) 
              (vector (lambda (x) #t) (lambda (x) #t)) 
              'pos
              'neg)
   "pos")
  
  (test/spec-failed
   'vector-immutable8
   '(contract (vector-immutable/c (number? . -> . boolean?) (number? . -> . boolean?)) 
              (vector->immutable-vector (vector (lambda (x) #t)))
              'pos
              'neg)
   "pos")
  
  (test/spec-failed
   'vector-immutable9
   '(contract (vector-immutable/c (number? . -> . boolean?) (number? . -> . boolean?)) 
              (vector->immutable-vector (vector (lambda (x) #t) (lambda (x) #t) (lambda (x) #t)))
              'pos
              'neg)
   "pos")
  
  (test/spec-passed
   'vector-immutable10
   '(let ([ctc (contract (vector-immutable/c (number? . -> . boolean?) (number? . -> . boolean?)) 
                         (vector->immutable-vector (vector (lambda (x) #t) (lambda (x) #t))) 
                         'pos
                         'neg)])
      ((vector-ref ctc 0) 1)
      ((vector-ref ctc 1) 1)))

  (test/spec-passed/result
   'vector-immutable11
   '(contract (vector-immutable/c number? boolean?) 
              (vector->immutable-vector (vector 1 #t))
              'pos
              'neg)
   (vector->immutable-vector (vector 1 #t)))

  (test/spec-failed
   'box-immutable1
   '(contract (box-immutable/c (number? . -> . boolean?)) 
              #f
              'pos
              'neg)
   "pos")
  
  (test/spec-failed
   'box-immutable2
   '(contract (box-immutable/c (number? . -> . boolean?)) 
              (box (lambda (x) #t)) 
              'pos
              'neg)
   "pos")
  
  (test/spec-failed
   'box-immutable3
   '(let ([ctc (contract (box-immutable/c (number? . -> . boolean?)) 
                         (box-immutable (lambda (x) #t))
                         'pos
                         'neg)])
      ((unbox ctc) #f))
   "neg")
  
  (test/spec-failed
   'box-immutable4
   '(let ([ctc (contract (box-immutable/c (number? . -> . boolean?)) 
                         (box-immutable (lambda (x) 1))
                         'pos
                         'neg)])
      ((unbox ctc) 1))
   "pos")
  
  (test/spec-passed
   'box-immutable5
   '(let ([ctc (contract (box-immutable/c (number? . -> . boolean?)) 
                         (box-immutable (lambda (x) #t))
                         'pos
                         'neg)])
      ((unbox ctc) 1)))

  (test/spec-passed/result
   'vector-immutable6
   '(contract (box-immutable/c boolean?) 
              (box-immutable #t)
              'pos
              'neg)
   (box-immutable #t))

  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                                                        ;;
  ;;   Flat Contract Tests                                  ;;
  ;;                                                        ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (test #t flat-contract? (union))
  (test #t flat-contract? (union integer? (lambda (x) (> x 0))))
  (test #t flat-contract? (union (flat-contract integer?)
				 (flat-contract boolean?)))
  (test-flat-contract '(union (flat-contract integer?) char?) #\a #t)
  (test-flat-contract '(union (flat-contract integer?) char?) 1 #t)
  
  (test #t flat-contract? (and/c))
  (test #t flat-contract? (and/c number? integer?))
  (test #t flat-contract? (and/c (flat-contract number?)
				 (flat-contract integer?)))
  (test-flat-contract '(and/c number? integer?) 1 3/2)

  (test-flat-contract '(not/f integer?) #t 1)
  (test-flat-contract '(>=/c 5) 5 0)
  (test-flat-contract '(<=/c 5) 5 10)
  (test-flat-contract '(</c 5) 0 5)
  (test-flat-contract '(>/c 5) 10 5)
  (test-flat-contract '(integer-in 0 10) 0 11)
  (test-flat-contract '(integer-in 0 10) 10 3/2)
  (test-flat-contract '(real-in 1 10) 3/2 20)
  (test-flat-contract '(string/len 3) "ab" "abc")
  (test-flat-contract 'natural-number? 5 -1)
  (test-flat-contract 'false? #f #t)
  (test/spec-passed 'any? '(contract any? 1 'pos 'neg))
  (test-flat-contract 'printable? (vector (cons 1 (box #f))) (lambda (x) x))
  (test-flat-contract '(symbols 'a 'b 'c) 'a 'd)
  
  (let ([c% (class object% (super-new))])
    (test-flat-contract (subclass?/c c%) c% object%)
    (test-flat-contract (subclass?/c c%) (class c%) (class object%)))
  
  (let ([i<%> (interface ())])
    (test-flat-contract `(implementation?/c ,i<%>) (class* object% (i<%>) (super-new)) object%)
    (test-flat-contract `(implementation?/c ,i<%>) (class* object% (i<%>) (super-new)) #f))
  
  (let ([i<%> (interface ())]
        [c% (class object% (super-new))])
    (test-flat-contract `(is-a?/c ,i<%>) (new (class* object% (i<%>) (super-new))) (new object%))
    (test-flat-contract `(is-a?/c ,c%) (new c%) (new object%)))
  
  (test-flat-contract '(listof boolean?) (list #t #f) (list #f 3 #t))
  (test-flat-contract '(listof any?) (list #t #f) 3)
  ;(test-flat-contract '(list-immutableof boolean?) (list-immutable #t #f) (list-immutable #f 3 #t))
  ;(test-flat-contract '(list-immutableof any?) (list-immutable #t #f) 3)
  ;(test-flat-contract '(list-immutableof boolean?) (list-immutable) (list))
  ;(test-flat-contract '(list-immutableof (-> boolean? boolean?)) (list-immutable (lambda (x) x)) (list (lambda (x) x)))
  
  (test-flat-contract '(vectorof boolean?) (vector #t #f) (vector #f 3 #t))
  (test-flat-contract '(vectorof any?) (vector #t #f) 3)
  
  (test-flat-contract '(vector/p boolean? (flat-contract integer?)) (vector #t 1) (vector 1 #f))
  (test-flat-contract '(vector/p boolean? (flat-contract integer?)) (vector #t 1) #f)

  (test-flat-contract '(cons/p boolean? (flat-contract integer?)) (cons #t 1) (cons 1 #f))
  (test-flat-contract '(cons/p boolean? (flat-contract integer?)) (cons #t 1) #f)
  (test-flat-contract '(list/p boolean? (flat-contract integer?)) (list #t 1) (list 1 #f))
  (test-flat-contract '(list/p boolean? (flat-contract integer?)) (list #t 1) #f)

  ;(test-flat-contract '(cons-immutable/c boolean? (flat-contract integer?)) (cons-immutable #t 1) (cons-immutable 1 #f))
  ;(test-flat-contract '(cons-immutable/c boolean? (flat-contract integer?)) (cons-immutable #t 1) #f)
  ;(test-flat-contract '(cons-immutable/c boolean? (flat-contract integer?)) (cons-immutable #t 1) (cons #t 1))
  ;(test-flat-contract '(cons-immutable/c (-> boolean? boolean?) integer?) (cons-immutable (lambda (x) x) 1) #f)
  
  ;(test-flat-contract '(list-immutable/c boolean? (flat-contract integer?)) (list-immutable #t 1) (list-immutable 1 #f))
  ;(test-flat-contract '(list-immutable/c boolean? (flat-contract integer?)) (list-immutable #t 1) #f)
  ;(test-flat-contract '(list-immutable/c boolean? (flat-contract integer?)) (list-immutable #t 1) (list #t 1))
  ;(test-flat-contract '(list-immutable/c (-> boolean? boolean?) integer?) (list-immutable (lambda (x) x) 1) #f)
  
  (test-flat-contract '(box/p boolean?) (box #f) (box 1))
  (test-flat-contract '(box/p (flat-contract boolean?)) (box #t) #f)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                                                        ;;
  ;;   case-> arity checking tests                          ;;
  ;;                                                        ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (test/well-formed #'(case-> (-> integer? integer?)))
  (test/well-formed #'(case-> (-> integer? integer?) (-> integer? integer? integer?)))
  (test/well-formed #'(case-> (-> integer? integer?) (-> integer? integer? any)))
  (test/well-formed #'(case-> (-> integer? any) (-> integer? integer? any)))
  
  (test/well-formed #'(case-> (->d (lambda x any?)) (-> integer? integer?)))

  (test/well-formed #'(case-> (->* (any? any?) (integer?)) (-> integer? integer?)))
  (test/well-formed #'(case-> (->* (any? any?) any? (integer?)) (-> integer? integer?)))
  (test/well-formed #'(case-> (->* (any? any?) any? any) (-> integer? integer?)))
  
  (test/well-formed #'(case-> (->d* (any? any?) (lambda x any?)) (-> integer? integer?)))
  (test/well-formed #'(case-> (->d* (any? any?) any? (lambda x any?)) (-> integer? integer?)))
  
  ))
(report-errs)
