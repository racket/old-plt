(module tester-model mzscheme
  (require (lib "class.ss"))
  (require (lib "growable-vector.ss" "tests" "tester" "private"))
  (require (lib "tester-structs.ss"  "tests" "tester" "private"))

  (provide model%)
  
  ; (forward-to object method-name ...)
  ; defines functions for each given method name that simply
  ; invoke the method on the given object with all of their
  ; arguments, in effect forwarding a method callin one object
  ; to another 
  (define-syntax (forward-to stx)
    (syntax-case stx ()
      [(_ obj method ...)
       (syntax 
        (define-values (method ...)
          (values
           (lambda args (send obj method . args)) ...)))]))
  
  (define model%
    (class* object% ()
      (public load-tests run-tests restart rechoose
              next-error? prev-error? get-first-error 
              get-next-error get-prev-error 
              (register-test-group test))
      (init
       gui:in-init
       gui:in-choose
       gui:in-done
       gui:init->choose
       gui:loading-new-module
       gui:successful-module-load
       gui:failed-module-load
       gui:choose->done
       gui:testing-new-group
       gui:report-test-passed
       gui:report-test-failed)
      
      (define curr-state #f)
      
      (define load-tests 
        (lambda args 
          (transition-to (send curr-state load-tests . args))))
      
      (define run-tests
        (lambda args
          (transition-to (send curr-state run-tests . args))))
      
      (define restart
        (lambda args
          (transition-to (send curr-state restart . args))))
      
      (define rechoose
        (lambda args
          (transition-to (send curr-state rechoose . args))))
      
      (define transition-to
        (lambda (a-state)
          (set! curr-state a-state)
          (send curr-state on-enter)
          (void)))
      
      (define state<%>
        (interface () load-tests run-tests restart rechoose on-enter))
      
      (define abstract-state%
        (class* object% (state<%>)
          (public on-enter load-tests run-tests restart rechoose)
          
          (define on-enter (lambda () (void)))
          (define load-tests (lambda () this))
          (define run-tests (lambda () this))
          (define restart (lambda () this))
          (define rechoose (lambda () this))
          
          (super-instantiate ())))
      
      (define init<S>%
        (class* abstract-state% (state<%>)
          (override on-enter load-tests)
          
          (define on-enter (lambda () (gui:in-init)))
          (define load-tests
            (lambda (man)
              (gui:init->choose (length man))
              (load-tests-internal man)
              (make-object choose<S>%)))
          
          (super-instantiate ())))
      
      (define choose<S>%
        (class* abstract-state% (state<%>)
          (override on-enter run-tests)
          (define on-enter (lambda () (gui:in-choose (gvector->list queued-test-groups))))
          
          ;; run-tests : '((group (test ...)) ...)
          (define run-tests 
            (lambda (tests)
              (gui:choose->done (apply + (map (lambda (x) (length (cadr x))) tests)))
              (run-chosen-tests tests)
              (make-object done<S>%)))
          
          (super-instantiate ())))
      
      (define done<S>%
        (class* abstract-state% (state<%>)
          (override on-enter rechoose restart)
          (define on-enter (lambda () (gui:in-done)))
          (define rechoose (lambda () (make-object choose<S>%)))
          (define restart (lambda () (make-object init<S>%)))
          
          (super-instantiate ())))
              
      (transition-to (make-object init<S>%))
      
;      (define state-machine 
;        (make-object
;            (fsm 
;             ; init state
;             init<S>
;             ; alphabet
;             (load-tests run-tests restart rechoose)
;             ; state definitions
;             [init<S>   gui:in-init
;                        (load-tests choose<S> (lambda (man)
;                                                (gui:init->choose (length man))
;                                                (load-tests-internal man)))]
;             [choose<S> (lambda () (gui:in-choose (gvector->list queued-test-groups)))
;                        (run-tests  done<S>   (lambda (tests)
;                                                (gui:choose->done (apply + (map length (cadr tests))))
;                                                (run-chosen-tests tests)))]
;             [done<S>   gui:in-done
;                        (rechoose   choose<S> (lambda () (void)))
;                        (restart    init<S>   (lambda () (void)))])))
;      
;      (define load-tests
;        (lambda args (send state-machine load-tests args)))
;      
;      (define run-tests
;        (lambda args (send state-machine run-tests args)))
;      
;      (define restart
;        (lambda args (send state-machine restart args)))
;      
;      (define rechoose
;        (lambda args (send state-machine rechoose args)))
      
    ;; ======================================================================
    ;; LOADING
    ;; This section contains definitions related to loading tests. Its goal is
    ;; to initialize the gvector tests-to-run.
      
      ; queued-test-groups : gvector[test-group]
      (define queued-test-groups (make-gvector 0))
      
      ; load-tests : (listof module-path-datum) -> void
      ; side-effect : dynamic-require's all module specs
      ; this function loads each test so that it can be subsequently
      ; selected and run.
      (define load-tests-internal
        (lambda (manifest)
          (for-each
           (lambda (spec)
             (printf "loading ~v~n" spec)
             (begin
               (gui:loading-new-module spec)
               (with-handlers
                   ([exn? (lambda (e) (gui:failed-module-load e))])
                 ((dynamic-require spec 'test-main))
                 (gui:successful-module-load))))
           manifest)))
      
      ; register-test-group : test-group -> void
      ; side-effect: updates tests-to-run to include this test-group
      (define register-test-group
        (lambda (test-group)
          (gvector-add! queued-test-groups test-group)))
      
      ; ======================================================================
      ; RUNNING
      ; This section handles actually running the tests
      
      ; run-chosen-tests : (listof (list test-group (listof test))) -> void
      ; for each item in the given list, runs the tests, which are a subset 
      ; of the tests that the test-group provides
      (define run-chosen-tests
        (lambda (chosen-tests)
          (for-each (lambda (x) (test (car x) (cadr x))) chosen-tests)))
      
      ; test: test-group x (listof test) -> void
      ; side effect: informs the GUI of the new test group, runs the
      ; group's initializer, and tests the given tests.
      (define test
        (lambda (test-group tests)
          (begin
            (gui:testing-new-group (test-group-description test-group)
                                   (length tests))
            (let ((initializer (test-group-initializer test-group)))
              (if initializer (initializer)))
            (for-each run-test tests))))
      
      ; run-test : test -> void
      ; side-effect: updates gui
      ; runs the specified test and updates the gui with the result
      (define run-test
        (lambda (test)
          (let ((expect (test-expectation test))
                (resl   (get-result (test-thunk test))))
            (if (result-matches-expectation? resl expect)
                (do-report-success test expect resl)
                (do-report-failure test expect resl)))))
      
      ; (-> value) -> received
      ; produces an output specification for a given thunk
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
      
      ; received x expectation -> bool
      ; determines if the output matches the expectation. There are a few cases:
      ; 1. The expectation is not a function. Hand off to result-match-int.
      ; 2. The expectation is a function ...
      ;    a. ... and expect-print is a str. Compare output with string=?, call the 
      ;                              function on the value produced.
      ;    b. ... and expect-print is #f. Call the function with the value and the string.
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
      
;; do-report-success : test x expect x output-spec -> void
;; reports successful completion of the test
      (define do-report-success
        (lambda (test expect resl)
          (gui:report-test-passed test)))
      
;; do-report-failure : test x expect x output-spec -> void
;; reports a failure
      (define do-report-failure
        (lambda (test expect resl)
          (begin
            (gvector-add! failed-tests (list test expect resl))
            (if (not curr-failed-test)
                (set! curr-failed-test 0))
            (gui:report-test-failed test expect resl))))
      
      ; ==================================================================
      ; ERROR REPORTING CODE    
      
      (define failed-tests (make-gvector 0)) ;; gvector[(list test expect output-spec)]
      (define curr-failed-test #f)
      
      (define next-error?
        (lambda ()
          (and curr-failed-test 
               (< curr-failed-test (- (gvector-length failed-tests) 1)))))
      
      (define prev-error?
        (lambda ()
          (and curr-failed-test
               (> curr-failed-test 0))))
      
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
      
      (super-instantiate ()))))