(module testing mzscheme 
  
  (provide
   test== ;; (test== <test expression> <expected-value> [<optional message>])
   test-eq ;; (test== <test expression> <expected-value> <equality> [<optional message>])
   test-p ;; (test-p <test expression> <expected-value> [<optional message>])
   test-e ;; (test-e <test expression> <expected-exception> [<optional message>])
   )
  
  (require-for-syntax (file "aux.scm"))
  
  (print-struct #t)

  ;; N, number of test cases that worked 
  (define test-case-succeeded 0)
  
  (define (success)
    (set! test-case-succeeded (+ 1 test-case-succeeded)))
  
  (define test-case-failed 0)
  
  (define (fail expected yield)
    (printf "FAILURE: --- expected ---~n~a~n"expected)
    (printf " --- produced ---~n~a~n" yield)
    (set! test-case-failed (+ 1 test-case-failed)))
  
  (define (fl lft expected msg)
    (fail expected lft)
    (printf "~a~n" msg))
  
  (define-syntax test==
    (syntax-rules ()
      [(_ test-expression expected-value) 
       (test== test-expression expected-value "")]
      [(_ lft expected msg) 
       (with-handlers ([void (lambda (e)
                               (fail expected (format "~e" e))
                               (printf "~a~n" msg))])
         (let ([l lft][r expected]) 
           (if (equal? l r) (success) (fl l r msg))))]))
  
  (define-syntax test-eq
    (syntax-rules ()
      [(_ test-expression expected-value equal?) 
       (test-eq test-expression expected-value equal? "")]
      [(_ lft expected equal? msg) 
       (with-handlers ([void (lambda (e)
                               (fail expected (format "~e" e))
                               (printf "~a~n" msg))])
         (let ([l lft][r expected]) 
           (if (equal? l r) (success) (fl l r msg))))]))
  
  (define-syntax test-p
    (syntax-rules ()
      [(_ test-expression expected-pred) 
       (test-p test-expression expected-pred "")]
      [(_ lft expected-pred msg) 
       (with-handlers ([void (lambda (e)
                               (fail expected-pred (format "~e" e))
                               (printf "~a~n" msg))])
         (let ([l lft][r? expected-pred]) 
           (if (r? l) (success) (fl l r? msg))))]))
  
  ;(define (add? e)
  ;    (datum->syntax-object e (string->symbol (format "~a?" (syntax-e e)))))
  
  (define-syntax (test-e e)
    (syntax-case e () 
      [(_ to-be-tested expected-exception)  
       (syntax (test-e to-be-tested expected-exception ""))]
      [(_ to-be-tested expected-e optional-message)
       (identifier? (syntax expected-e))
       (let ([expected-e? (add? (syntax expected-e))])
         ;; at this point we don't know whether expected-e is indeed a predicate
         #`(with-handlers
               ([#,expected-e? (lambda (e) (success))]
                [void (lambda (e) 
                        (fail (format "~e exception expected" 'expected-e) e)
                        (printf "~a~n" optional-message))])
             (let ([actual-result to-be-tested])
               (fail (format "~e exception expected" 'expected-e) actual-result)
               (printf "~a~n" optional-message))))]))
  )
