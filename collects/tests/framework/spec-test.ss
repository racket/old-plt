(module spec-test mzscheme
  (require "test-suite-utils.ss")
  
  (load-framework-automatically #f)
  (send-sexp-to-mred '(require (lib "specs.ss" "framework")))
  (send-sexp-to-mred '(require (lib "pretty.ss")))
  
  ;; test/spec-passed : symbol sexp -> void
  ;; tests a passing specification
  (define (test/spec-passed name expression)
    (test name
          (lambda (x) (eq? x 'passed))
          (lambda ()
            (send-sexp-to-mred `(begin ,expression 'passed)))))
  
  ;; test/spec-failed : symbol sexp string -> void
  ;; tests a failing specification with blame assigned to `blame'
  (define (test/spec-failed name expression blame)
    (test name
          (lambda (x) 
            (and (string? x)
                 (let ([m (regexp-match "blame ([^:]*):" x)])
                   (equal? (cadr m) blame))))
          (lambda ()
            (send-sexp-to-mred `(with-handlers ([(lambda (x) (and (not-break-exn? x) (exn? x)))
                                                 exn-message])
                                  ,expression
                                  'failed/expected-exn-got-normal-termination)))))

  (test/spec-passed
   'contract-flat1 
   '(contract not #f 'pos 'neg))
  
  (test/spec-failed
   'contract-flat2 
   '(contract not #t 'pos 'neg)
   "pos")
  
  (test/spec-passed
   'contract-flat3 
   '(contract (integer? . -> . integer?) (lambda (x) x) 'pos 'neg))
  
  (test/spec-failed
   'contract-flat4
   '(contract (integer? . -> . integer?) (lambda (x y) x) 'pos 'neg)
   "pos")
  
  (test/spec-failed
   'contract-ho1
   '((contract (integer? . -> . integer?) (lambda (x) #f) 'pos 'neg) #t)
   "neg")
  
  (test/spec-failed
   'contract-ho2
   '((contract (integer? . -> . integer?) (lambda (x) #f) 'pos 'neg) 1)
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

  (test/spec-failed
   'contract-d-protect-shared-state
   '(let ([x 1])
      ((contract (((lambda () #t) . ->d . (lambda () (let ([pre-x 1]) (lambda (res) (= x pre-x)))))
                  . -> .
                  (lambda (x) #t))
                 (lambda (thnk) (thnk))
                 'pos
                 'neg)
       (lambda () (set! x 2))))
   "neg")
  
  )
  
  
 