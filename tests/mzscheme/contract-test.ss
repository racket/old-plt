(load-relative "loadtest.ss")
(require (lib "contract.ss")
	 (lib "class.ss")
         (lib "etc.ss"))
  
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

  (define (test-name name contract)
    (test name contract-name contract))
  
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
   'contract-case->7
   '((contract (case-> (integer? integer? . -> . integer?) (->* (integer?) any? (boolean?)))
               (lambda x #\a)
               'pos
               'neg)
     1 2)
   "pos")
  
  (test/spec-failed
   'contract-case->8
   '((contract (case-> (integer? integer? . -> . integer?) (->* (integer?) any? (boolean?)))
               (lambda x #t)
               'pos
               'neg)
     1 2)
   "pos")
 
  (test/spec-passed
   'contract-case->8
   '((contract (case-> (integer? integer? . -> . integer?) (->* (integer?) any? (boolean?)))
               (lambda x 1)
               'pos
               'neg)
     1 2))
 
  
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

  
;                                                                                                     
;                                                                                                     
;                                                                                                     
;           ;       ;                                                                                 
;           ;                                                                                         
;           ;                         ;                                   ;                       ;   
;    ;;;    ; ;;    ;    ;;;    ;;;  ;;;;           ;;;    ;;;    ; ;;   ;;;;  ; ;  ;;;     ;;;  ;;;; 
;   ;   ;   ;;  ;   ;   ;   ;  ;   ;  ;            ;   ;  ;   ;   ;;  ;   ;    ;;  ;   ;   ;   ;  ;   
;  ;     ;  ;    ;  ;  ;    ; ;       ;           ;      ;     ;  ;   ;   ;    ;       ;  ;       ;   
;  ;     ;  ;    ;  ;  ;;;;;; ;       ;    ;;;;;; ;      ;     ;  ;   ;   ;    ;    ;;;;  ;       ;   
;  ;     ;  ;    ;  ;  ;      ;       ;           ;      ;     ;  ;   ;   ;    ;   ;   ;  ;       ;   
;   ;   ;   ;;  ;   ;   ;      ;   ;  ;            ;   ;  ;   ;   ;   ;   ;    ;   ;   ;   ;   ;  ;   
;    ;;;    ; ;;    ;    ;;;;   ;;;    ;;           ;;;    ;;;    ;   ;    ;;  ;    ;;;;;   ;;;    ;; 
;                   ;                                                                                 
;                   ;                                                                                 
;                 ;;                                                                                  

  
  (test/spec-passed
   'object-contract0
   '(contract (object-contract)
              (new object%)
              'pos
              'neg))

  (test/spec-failed
   'object-contract/field1
   '(contract (object-contract (field x integer?))
              (new object%)
              'pos
              'neg)
   "pos")
  
  (test/spec-failed
   'object-contract/field2
   '(contract (object-contract (field x integer?))
              (new (class object% (field [x #t]) (super-new)))
              'pos
              'neg)
   "pos")
  
  (test/spec-passed/result
   'object-contract/field3
   '(get-field
     x
     (contract (object-contract (field x integer?))
               (new (class object% (field [x 12]) (super-new)))
               'pos
               'neg))
   12)
  
  (test/spec-failed
   'object-contract/field4
   '(contract (object-contract (field x boolean?) (field y boolean?))
              (new (class object% (field [x #t] [y 'x]) (super-new)))
              'pos
              'neg)
   "pos")
  
  (test/spec-failed
   'object-contract/field5
   '(contract (object-contract (field x symbol?) (field y symbol?))
              (new (class object% (field [x #t] [y 'x]) (super-new)))
              'pos
              'neg)
   "pos")
  
  (test/spec-passed/result
   'object-contract->1
   '(send
     (contract (object-contract (m (integer? . -> . integer?)))
               (new (class object% (define/public (m x) x) (super-new)))
               'pos
               'neg)
     m
     1)
   1)
  
  (test/spec-failed
   'object-contract->2
   '(contract (object-contract (m (integer? . -> . integer?)))
              (make-object object%)
              'pos
              'neg)
   "pos")
  
  (test/spec-failed
   'object-contract->3
   '(send
     (contract (object-contract (m (integer? . -> . integer?)))
               (make-object (class object% (define/public (m x) x) (super-instantiate ())))
               'pos
               'neg)
     m
     'x)
   "neg")
  
  (test/spec-failed
   'object-contract->4
   '(send
     (contract (object-contract (m (integer? . -> . integer?)))
               (make-object (class object% (define/public (m x) 'x) (super-instantiate ())))
               'pos
               'neg)
     m
     1)
   "pos")
  
  (test/spec-failed
   'object-contract->5
   '(contract (object-contract (m (integer? integer? . -> . integer?)))
              (make-object (class object% (define/public (m x) 'x) (super-instantiate ())))
              'pos
              'neg)
   "pos")
  
  (test/spec-failed
   'object-contract-case->1
   '(contract (object-contract (m (case-> (boolean? . -> . boolean?)
                                          (integer? integer? . -> . integer?))))
              (new object%)
              'pos
              'neg)
   "pos")
  
  (test/spec-failed
   'object-contract-case->2
   '(contract (object-contract (m (case-> (boolean? . -> . boolean?)
                                          (integer? integer? . -> . integer?))))
              (new (class object% (define/public (m x) x) (super-new)))
              'pos
              'neg)
   "pos")
  
  (test/spec-failed
   'object-contract-case->3
   '(contract (object-contract (m (case-> (boolean? . -> . boolean?)
                                          (integer? integer? . -> . integer?))))
              (new (class object% (define/public (m x y) x) (super-new)))
              'pos
              'neg)
   "pos")
  
  (test/spec-passed
   'object-contract-case->4
   '(contract (object-contract (m (case-> (boolean? . -> . boolean?)
                                          (integer? integer? . -> . integer?))))
              (new (class object% 
                     (define/public m
                       (case-lambda
                         [(b) (not b)]
                         [(x y) (+ x y)]))
                     (super-new)))
              'pos
              'neg))
  
  (test/spec-passed/result
   'object-contract-case->5
   '(send (contract (object-contract (m (case-> (boolean? . -> . boolean?)
                                                (integer? integer? . -> . integer?))))
                    (new (class object% 
                           (define/public m
                             (case-lambda
                               [(b) (not b)]
                               [(x y) (+ x y)]))
                           (super-new)))
                    'pos
                    'neg)
          m 
          #t)
   #f)
  
  (test/spec-passed/result
   'object-contract-case->6
   '(send (contract (object-contract (m (case-> (boolean? . -> . boolean?)
                                                (integer? integer? . -> . integer?))))
                    (new (class object% 
                           (define/public m
                             (case-lambda
                               [(b) (not b)]
                               [(x y) (+ x y)]))
                           (super-new)))
                    'pos
                    'neg)
          m 
          3
          4)
   7)
  
  (test/spec-failed
   'object-contract-opt->*1
   '(contract (object-contract (m (opt->* (integer?) (symbol? boolean?) (number?))))
              (new (class object%
                     (define/public m
                       (opt-lambda (x [y 'a])
                         x))
                     (super-new)))
              'pos
              'neg)
   "pos")
  
  (test/spec-failed
   'object-contract-opt->*2
   '(contract (object-contract (m (opt->* (integer?) (symbol? boolean?) (number?))))
              (new (class object%
                     (define/public m
                       (opt-lambda (x y [z #t])
                         x))
                     (super-new)))
              'pos
              'neg)
   "pos")
  
  (test/spec-passed
   'object-contract-opt->*3
   '(contract (object-contract (m (opt->* (integer?) (symbol? boolean?) (number?))))
              (new (class object%
                     (define/public m
                       (opt-lambda (x [y 'a] [z #t])
                         x))
                     (super-new)))
              'pos
              'neg))
  
  (test/spec-passed/result
   'object-contract-opt->*4
   '(send (contract (object-contract (m (opt->* (integer?) (symbol? boolean?) (number?))))
                    (new (class object%
                           (define/public m
                             (opt-lambda (x [y 'a] [z #t])
                               x))
                           (super-new)))
                    'pos
                    'neg)
          m
          1)
   1)
  
  (test/spec-passed/result
   'object-contract-opt->*5
   '(send (contract (object-contract (m (opt->* (integer?) (symbol? boolean?) (number?))))
                    (new (class object%
                           (define/public m
                             (opt-lambda (x [y 'a] [z #t])
                               x))
                           (super-new)))
                    'pos
                    'neg)
          m
          2
          'z)
   2)
  
  (test/spec-passed/result
   'object-contract-opt->*7
   '(send (contract (object-contract (m (opt->* (integer?) (symbol? boolean?) (number?))))
                    (new (class object%
                           (define/public m
                             (opt-lambda (x [y 'a] [z #t])
                               x))
                           (super-new)))
                    'pos
                    'neg)
          m
          3
          'z
          #f)
   3)
  
  (test/spec-failed
   'object-contract-opt->*8
   '(send (contract (object-contract (m (opt->* (integer?) (symbol? boolean?) (number?))))
                    (new (class object%
                           (define/public m
                             (opt-lambda (x [y 'a] [z #t])
                               x))
                           (super-new)))
                    'pos
                    'neg)
          m
          #f)
   "neg")
  
  (test/spec-failed
   'object-contract-opt->*9
   '(send (contract (object-contract (m (opt->* (integer?) (symbol? boolean?) (number?))))
                    (new (class object%
                           (define/public m
                             (opt-lambda (x [y 'a] [z #t])
                               x))
                           (super-new)))
                    'pos
                    'neg)
          m
          2
          4)
   "neg")
  
  (test/spec-failed
   'object-contract-opt->*10
   '(send (contract (object-contract (m (opt->* (integer?) (symbol? boolean?) (number?))))
                    (new (class object%
                           (define/public m
                             (opt-lambda (x [y 'a] [z #t])
                               x))
                           (super-new)))
                    'pos
                    'neg)
          m
          3
          'z
          'y)
   "neg")
  
  (test/spec-failed
   'object-contract-opt->*11
   '(send (contract (object-contract (m (opt->* (integer?) (symbol? boolean?) (number?))))
                    (new (class object%
                           (define/public m
                             (opt-lambda (x [y 'a] [z #t])
                               'x))
                           (super-new)))
                    'pos
                    'neg)
          m
          3
          'z
          #f)
   "pos")
  
  (test/spec-passed/result
   'object-contract-opt->*12
   '(let-values ([(x y)
                  (send (contract (object-contract (m (opt->* (integer?) (symbol? boolean?) (number? symbol?))))
                                  (new (class object%
                                         (define/public m
                                           (opt-lambda (x [y 'a] [z #t])
                                             (values 1 'x)))
                                         (super-new)))
                                  'pos
                                  'neg)
                        m
                        3
                        'z
                        #f)])
      (cons x y))
   (cons 1 'x))
  
  (test/spec-failed
   'object-contract-opt->*13
   '(send (contract (object-contract (m (opt->* (integer?) (symbol? boolean?) (number? symbol?))))
                    (new (class object%
                           (define/public m
                             (opt-lambda (x [y 'a] [z #t])
                               (values 'x 'x)))
                           (super-new)))
                    'pos
                    'neg)
          m
          3
          'z
          #f)
   "pos")
  
  (test/spec-failed
   'object-contract-opt->*14
   '(send (contract (object-contract (m (opt->* (integer?) (symbol? boolean?) (number? symbol?))))
                    (new (class object%
                           (define/public m
                             (opt-lambda (x [y 'a] [z #t])
                               (values 1 1)))
                           (super-new)))
                    'pos
                    'neg)
          m
          3
          'z
          #f)
   "pos")
   
  
;                                                                     
;                                                                     
;                                                                     
;   ;                                               ;       ;         
;                                                   ;       ;         
;                                       ;           ;       ;         
;   ;   ; ;;  ;;    ; ;;  ;;    ;   ;  ;;;;  ;;;    ; ;;    ;    ;;;  
;   ;   ;;  ;;  ;   ;;  ;;  ;   ;   ;   ;   ;   ;   ;;  ;   ;   ;   ; 
;   ;   ;   ;   ;   ;   ;   ;   ;   ;   ;       ;   ;    ;  ;  ;    ; 
;   ;   ;   ;   ;   ;   ;   ;   ;   ;   ;    ;;;;   ;    ;  ;  ;;;;;; 
;   ;   ;   ;   ;   ;   ;   ;   ;   ;   ;   ;   ;   ;    ;  ;  ;      
;   ;   ;   ;   ;   ;   ;   ;   ;  ;;   ;   ;   ;   ;;  ;   ;   ;     
;   ;   ;   ;   ;   ;   ;   ;    ;; ;    ;;  ;;;;;  ; ;;    ;    ;;;; 
;                                                                     
;                                                                     
;                                                                     

  
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
  
  (test-flat-contract '(flat-rec-contract sexp (cons/p sexp sexp) number?) '(1 2 . 3) '(1 . #f))
  
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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                                                        ;;
  ;;   Contract Name Tests                                  ;;
  ;;                                                        ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (test-name "integer?" (flat-contract integer?))
  (test-name "boolean?" (flat-contract boolean?))
  (test-name "char?" (flat-contract char?))
  (test-name "any?" any?)
  (test-name "(-> integer? integer?)" (-> integer? integer?))
  (test-name "(-> integer? any)" (-> integer? any))
  (test-name "(-> integer? (values boolean? char?))" (-> integer? (values boolean? char?)))
  (test-name "(->* (integer? boolean?) (char? any?))" (->* (integer? boolean?) (char? any?)))
  (test-name "(->* (integer? boolean?) any)" (->* (integer? boolean?) any))
  (test-name "(->* (integer?) boolean? (char? any?))" (->* (integer?) boolean? (char? any?)))
  (test-name "(->* (integer? char?) boolean? any)" (->* (integer? char?) boolean? any))
  (test-name "(->d integer? boolean? ...)" (->d integer? boolean? (lambda (x y) char?)))
  (test-name "(->d* (integer? boolean?) ...)" (->d* (integer? boolean?) (lambda (x y) char?)))
  (test-name "(->d* (integer? boolean?) any? ...)" (->d* (integer? boolean?) any? (lambda (x y) char?)))
    
  (test-name "(case-> (-> integer? integer?) (-> integer? integer? integer?))"
             (case-> (-> integer? integer?) (-> integer? integer? integer?)))
  
  (test-name "(union)" (union))
  (test-name "(union integer? gt0?)" (union integer? (let ([gt0? (lambda (x) (> x 0))]) gt0?)))
  (test-name "(union integer? boolean?)"
             (union (flat-contract integer?)
                    (flat-contract boolean?)))
  (test-name "(union (-> (>=/c 5) (>=/c 5)) boolean?)"
             (union (-> (>=/c 5) (>=/c 5)) boolean?))
  
  (test-name "any?" (and/c))
  (test-name "and/c-contract?" (and/c number? integer?))
  (test-name "and/c-contract?" (and/c (flat-contract number?)
                                      (flat-contract integer?)))
  (test-name "(and/c number? (-> integer? integer?))" (and/c number? (-> integer? integer?)))

  (test-name "(not/f integer?)" (not/f integer?))
  (test-name "(>=/c 5)" (>=/c 5))
  (test-name "(<=/c 5)" (<=/c 5))
  (test-name "(</c 5)" (</c 5))
  (test-name "(>/c 5)" (>/c 5))
  (test-name "(integer-in 0 10)" (integer-in 0 10))
  (test-name "(real-in 1 10)" (real-in 1 10))
  (test-name "(string/len 3)" (string/len 3))
  (test-name "natural-number?" natural-number?)
  (test-name "false?" false?)
  (test-name "printable?" printable?)
  (test-name "(symbols 'a 'b 'c)"(symbols 'a 'b 'c))
  
  (let ([c% (class object% (super-new))])
    (test-name "(subclass?/c class:c%)" (subclass?/c c%)))
  
  (let ([i<%> (interface ())])
    (test-name "(implementation?/c interface:i<%>)" (implementation?/c i<%>)))
  
  (let ([i<%> (interface ())]
        [c% (class object% (super-new))])
    (test-name "(is-a?/c interface:i<%>)" (is-a?/c i<%>))
    (test-name "(is-a?/c class:c%)" (is-a?/c c%)))
  
  (test-name "(listof boolean?)" (listof boolean?))  
  (test-name "(listof any?)" (listof any?))
  (test-name "(list-immutableof boolean?)" (list-immutableof boolean?))
  (test-name "(list-immutableof any?)" (list-immutableof any?))
  (test-name "(list-immutableof boolean?)" (list-immutableof boolean?))
  (test-name "(list-immutableof (-> boolean? boolean?))" (list-immutableof (-> boolean? boolean?)))
  
  (test-name "(vectorof boolean?)" (vectorof boolean?))
  (test-name "(vectorof any?)" (vectorof any?))
  
  (test-name "(vector/p boolean? integer?)" (vector/p boolean? integer?))
  (test-name "(vector/p boolean? integer?)" (vector/p boolean? (flat-contract integer?)))

  (test-name "(cons/p boolean? integer?)" (cons/p boolean? (flat-contract integer?)))
  (test-name "(cons/p boolean? integer?)" (cons/p boolean? (flat-contract integer?)))
  (test-name "(cons/p boolean? (cons/p integer? null?))" (list/p boolean? (flat-contract integer?)))
  (test-name "(cons/p boolean? (cons/p integer? null?))" (list/p boolean? (flat-contract integer?)))

  (test-name "(cons-immutable/c boolean? integer?)" (cons-immutable/c boolean? (flat-contract integer?)))
  (test-name "(cons-immutable/c boolean? integer?)" (cons-immutable/c boolean? (flat-contract integer?)))
  (test-name "(cons-immutable/c boolean? integer?)" (cons-immutable/c boolean? (flat-contract integer?)))
  (test-name "(cons-immutable/c (-> boolean? boolean?) integer?)" (cons-immutable/c (-> boolean? boolean?) integer?))
  
  (test-name "(cons-immutable/c boolean? (cons-immutable/c integer? null?))"
             (list-immutable/c boolean? (flat-contract integer?)))
  (test-name "(cons-immutable/c boolean? (cons-immutable/c integer? null?))" 
             (list-immutable/c boolean? (flat-contract integer?)))
  (test-name "(cons-immutable/c boolean? (cons-immutable/c integer? null?))"
             (list-immutable/c boolean? (flat-contract integer?)))
  (test-name "(cons-immutable/c (-> boolean? boolean?) (cons-immutable/c integer? null?))"
             (list-immutable/c (-> boolean? boolean?) integer?))
  
  (test-name "(box/p boolean?)" (box/p boolean?))
  (test-name "(box/p boolean?)" (box/p (flat-contract boolean?)))
  (test-name "the-name" (flat-rec-contract the-name))

  (test-name "(object-contract)" (object-contract))
  (test-name "(object-contract (field x integer?))" (object-contract (field x integer?)))
  (test-name "(object-contract (m (-> integer? integer?)))"
             (object-contract (m (-> integer? integer?))))
  (test-name "(object-contract (m (-> integer? any)))"
             (object-contract (m (-> integer? any))))
  (test-name "(object-contract (m (-> integer? (values integer? integer?))))" 
             (object-contract (m (-> integer? (values integer? integer?)))))
  (test-name "(object-contract (m (case-> (-> integer? integer? integer?) (-> integer? (values integer? integer?)))))"
             (object-contract (m (case-> 
                                  (-> integer? integer? integer?)
                                  (-> integer? (values integer? integer?))))))
  (test-name "(object-contract (m (case-> (-> integer? (values symbol?)) (-> integer? boolean? (values symbol?)) (-> integer? boolean? number? (values symbol?)))))"
             (object-contract (m (opt->* (integer?) (boolean? number?) (symbol?)))))
  
  ))
(report-errs)
