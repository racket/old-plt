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
   'contract-flat5
   '(contract (case-> (integer? integer? . -> . integer?) (integer? . -> . integer?))
              (lambda (x) x)
              'pos
              'neg)
   "pos")
  
  (test/spec-failed
   'contract-ho1
   '((contract (integer? . -> . integer?) (lambda (x) #f) 'pos 'neg) #t)
   "neg")
  
  (test/spec-failed
   'contract-ho2
   '((contract (integer? . -> . integer?) (lambda (x) #f) 'pos 'neg) 1)
   "pos")
  
  )
  
  
 