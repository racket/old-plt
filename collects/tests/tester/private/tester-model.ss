(module tester-model mzscheme
  (require (lib "class.ss"))
  (require (file "growable-vector.ss"))
  (require (file "tester-structs.ss"))
  
  (provide model%)

(define model%
  (class* object% ()
    (public (register-test test) 
            run-all-tests
            run-test-groups
            next-error?
            prev-error?
            get-first-error
            get-next-error
            get-prev-error)
    
    (init-field 
       report-load-phase        ;; natnum -> void
       report-new-load          ;; module-path-datum -> void
       report-finished-load     ;; -> void
       report-test-phase        ;; natnum -> void
       report-new-test-group    ;; str x natnum -> void
       report-failure           ;; -> void
       report-success           ;; -> void
       wait-for-user-exit       ;; -> void
       )
    
    (field 
     (tests-to-run null)             ;; (listof test-group)
     (failed-tests (make-gvector 0)) ;; gvector[(list test expect output-spec)]
     (curr-failed-test #f))
    
    (define get-first-error
      (lambda ()
        (if (> (gvector-length failed-tests) 0)
            (gvector-ref failed-tests 0)
            (error 'get-first-error "No errors"))))
    
    (define get-next-error
      (lambda ()
        (if (next-error?)
            (begin
              (set! curr-failed-test (add1 curr-failed-test))
              (gvector-ref failed-tests curr-failed-test))
            (error 'get-next-error "No next error exists"))))
    
    (define get-prev-error
      (lambda ()
        (if (prev-error?)
            (begin
              (set! curr-failed-test (sub1 curr-failed-test))
              (gvector-ref failed-tests curr-failed-test))
            (error 'get-prev-error "No previous error exists"))))
    
    (define next-error?
      (lambda ()
        (and curr-failed-test 
             (< curr-failed-test (- (gvector-length failed-tests) 1)))))
    
    (define prev-error?
      (lambda ()
        (and curr-failed-test
             (> curr-failed-test 0))))
    
    (define register-test
      (lambda (test-group)
        (set! tests-to-run (cons test-group tests-to-run))))
        
    ;; test: test-group -> void
    ;; side effect: updates the GUI with test results for the given
    ;; spec
    (define test
       (lambda (test-group)
         (begin
           (report-new-test-group (test-group-description test-group)
                                  (length (test-group-tests test-group)))
           (let ((initializer (test-group-initializer test-group)))
             (if initializer
                 (initializer)))
           (for-each (lambda (x) (run-test x))
                     (test-group-tests test-group)))))
      
    ;; (-> value) -> received
    ;; produces an output specification for a given thunk
    (define get-result
      (lambda (thunk)
        (let ([op (open-output-string)])
	  (begin0
            (let ((result-val
                   (parameterize ([current-output-port op])
                     (with-handlers ([exn? (lambda (e)
                                             (make-error (exn-message e)))])
                       (let ([result-val (thunk)])
                         (make-finish result-val))))))
              (make-received result-val 
                             (get-output-string op)))
            (close-output-port op)))))

    ;; received x expectation -> bool
    ;; determines if the output matches the expectation. There are a few cases:
    ;; 1. The expectation is not a function. Hand off to result-match-int.
    ;; 2. The expectation is a function ...
    ;;    a. ... and expect-print is a str. Compare output with string=?, call the 
    ;;                              function on the value produced.
    ;;    b. ... and expect-print is #f. Call the function with the value and the string.
    (define result-matches-expectation?
      (lambda (resl expect)
        (if (procedure? (expect-output-criterion expect))
            (let ((pass? (expect-output-criterion expect)))
              (if (finish? (received-value resl))
                  (let ((val (finish-value (received-value resl))))
                    (if (expect-print expect)
                        (and (output-matches? resl expect)
                             (pass? val))
                        (pass? 
                         val
                         (received-print resl))))
                  #f))
            (result-match-int resl expect))))
    
    ;; output-matches? : received x expectation -> boolean
    ;; determines if the produced output printed to stdout corresponds to the expected output
    (define (output-matches? result expect)
      (or (not (expect-print expect))
          (string=? (expect-print expect)
                    (received-print result))))
    
    ;; result-match-int : received x expectation -> bool
    (define result-match-int
      (lambda (resl expect)
        (and 
         (or (not (expect-print expect))
             (string=? (expect-print expect)
                       (received-print resl)))
         (cond
           [(error? (received-value resl))
            (and (error? (expect-output-criterion expect))
                 (string=? (error-exception (received-value resl))
                           (error-exception (expect-output-criterion expect))))]
           [(finish? (received-value resl))
            (and (finish? (expect-output-criterion expect))
                 (equal? (finish-value (received-value resl))
                         (finish-value (expect-output-criterion expect))))]))))
    
    ;; run-test : test -> void
    ;; side-effect: updates gui
    ;; runs the specified test and updates the gui with the result
    (define run-test
      (lambda (test)
        (let ((expect (test-expectation test))
	      (resl   (get-result (test-thunk test))))
	  (if (result-matches-expectation? resl expect)
	      (do-report-success test expect resl)
	      (do-report-failure test expect resl)))))
    
    ;; do-report-success : test x expect x output-spec -> void
    ;; reports successful completion of the test
    (define do-report-success
      (lambda (test expect resl)
        (report-success)))
    
    ;; do-report-failure : test x expect x output-spec -> void
    ;; reports a failure
    (define do-report-failure
      (lambda (test expect resl)
        (begin
          (gvector-add! failed-tests (list test expect resl))
          (if (not curr-failed-test)
              (begin
                (set! curr-failed-test 0)
                (report-failure (list test expect resl)))
              (report-failure)))))
    
    ;; run-all-tests : (listof module-path-datum) -> void
    ;; side-effect : dynamic-require's all module specs
    ;; this is the main function. it runs all the tests.
    (define run-all-tests
      (lambda (manifest)
        (begin
          (report-load-phase (length manifest))
          (for-each 
           (lambda (spec) (begin
                            (report-new-load spec)
                            (with-handlers
                                ([exn? (lambda (e) (report-load-error e))])
                              ((dynamic-require spec 'test-main))
                              (report-finished-load))))
           manifest)
          (run-test-groups tests-to-run)
          ;;(wait-for-user-exit)
          
          )))
    
    ;; for now ...
    (define (report-load-error e)
      (printf "Oh no! ~a~n" (exn-message e))
      (report-finished-load))
    
    ;; get-size-and-nonempties : (listof test-group) -> (values natnum (listof test-group))
    ;; given a listof test-groups, produces the total number of tests in all groups and the
    ;; sublist of groups that have at least one test. defined as one function with 2 return
    ;; values here to avoid O(n) extra work in recurring twice down the same structure.
    (define (get-size-and-nonempties tgs)
      (cond
        [(null? tgs) (values 0 null)]
        [else (let-values ([(size)               (length (test-group-tests (car tgs)))]
                           [(rest-size rest-tgs) (get-size-and-nonempties (cdr tgs))])
                (values (+ size rest-size)
                        (if (> size 0)
                            (cons (car tgs) rest-tgs)
                            rest-tgs)))]))
    
    ;; run-test-groups : (listof test-group) -> void
    ;; side effect: runs the tests. Skips over groups with no tests (doesn't even run
    ;; the initializers)
    (define run-test-groups
      (lambda (tgs)
        (let-values (((total-size nonempty-tgs) (get-size-and-nonempties tgs)))
          (begin
            (report-test-phase total-size)
            (for-each test nonempty-tgs)))))
        
    
    (super-instantiate ()))))