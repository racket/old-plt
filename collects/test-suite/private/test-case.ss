(module test-case mzscheme
  (provide test-case)
  ;; NOTES: Putting the values of the call and expected into lists with call-with-values
  ;;        may be a missleading semantic for the those writing their own test predicates
  ;;        I wrap the calls to the predicate with my own function.
  (define-syntax (test-case stx)
    (case (syntax-local-context)
      [(module top-level)
       (syntax-case stx ()
         [(_ test call-stx exp-stx record set-actuals)
          #'(let ([call-values (call-with-values (lambda () call-stx) list)]
                  [exp-values (call-with-values (lambda () exp-stx) list)])
              (record (equal? call-values exp-values))
              (set-actuals call-values)
              (void))])]
      [else (raise-syntax-error #f "Test case box not at top level")])))