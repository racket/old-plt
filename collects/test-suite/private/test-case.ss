(module test-case mzscheme
  (provide test-case)
  (define-syntax (test-case stx)
    (case (syntax-local-context)
      [(module top-level)
       (syntax-case stx ()
         [(_ test call-stx exp-stx record set-actuals)
          #'(define-values ()
              (let ([call-values (call-with-values (lambda () call-stx) list)]
                    [exp-values (call-with-values (lambda () exp-stx) list)])
                (record (andmap equal? call-values exp-values))
                (set-actuals call-values)
                (values)))])]
      [else (raise-syntax-error #f "Test case box not at top level")])))