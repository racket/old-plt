(module test-case mzscheme
  
  (provide test-case)
    
  (define-syntax (test-case stx)
    (syntax-case stx ()
      [(_ test to-test-stx exp-stx record set-actuals)
       (case (syntax-local-context)
         [(module top-level)
          (syntax-property
           #`(define-values ()
               (let ([to-test-values (call-with-values (lambda () #,(syntax-property #`to-test-stx
                                                                                     'stepper-test-suite-hint
                                                                                     #t)) 
                                                       list)]
                     [exp-values (call-with-values (lambda () exp-stx) list)])
                 (record (and (= (length to-test-values) (length exp-values))
                              (andmap test to-test-values exp-values)))
                 (set-actuals to-test-values)
                 (values)))
           'stepper-skipto
           (list ;define-values
            syntax-e cdr cdr car 
            ; let-values
            syntax-e cdr car 
            ; clauses
            syntax-e car syntax-e cdr car
            ; call-with-values
            syntax-e cdr syntax-e cdr car 
            ; lambda
            syntax-e cdr cdr car
            ))]
         [else (raise-syntax-error #f
                                   "test case not at toplevel"
                                   (syntax/loc stx (test-case to-test-stx exp-stx)))])])))
