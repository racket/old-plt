(module test-case mzscheme
  (provide test-case)
  
  (define-syntax (test-case stx)
    (syntax-case stx ()
      [(_ test to-test-stx exp-stx record set-actuals)
       (case (syntax-local-context)
         [(module top-level)
          #'(define-values ()
              (let ([to-test-values (call-with-values (lambda () to-test-stx) list)]
                    [exp-values (call-with-values (lambda () exp-stx) list)])
                (record (and (= (length to-test-values) (length exp-values))
                             (andmap equal? to-test-values exp-values)))
                (set-actuals to-test-values)
                (values)))]
         [else (raise-syntax-error #f "Test case box not at top level"
                                   (syntax/loc stx (test-case to-test-stx exp-stx)))])])))