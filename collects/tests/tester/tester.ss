(module tester mzscheme
  (require "private/tester-gui.ss")
  (require "private/tester-structs.ss")
  (require (lib "class.ss"))
  
  (provide run-tests run-tests/initialize
           test test-error test-function
           test-manifest)
  
;; =======================================================================
;; Program interface -- provides syntax and functions for attaching the
;; test suite to particular tests
  
;; test-struct : (make-test str sexp (-> value) expectation)
;; test : (syntax (_ str sexp expectation)) -> (syntax test-struct)
;;        (syntax (_ str sexp expectation (union str #f))) -> (syntax test-struct)
;; test-error : (syntax (_ str sexp str))  -> (syntax test-struct)
;; test-function : (syntax (_ str sexp (value -> bool))) -> (syntax test-struct)
(define-syntaxes (test test-error test-function)
  (let ((syntax->test-struct
         (lambda (x)
           (syntax-case x ()
             [(test-type descrip body compare out ...)
              (with-syntax
                  ([expect-criterion
                    (syntax-case (syntax test-type) (test test-error test-function)
                      [test 
                       (syntax (make-finish compare))]
                      [test-error
                       (syntax (make-error compare))]
                      [test-function
                       (syntax compare)])]
                   [printed-output
                    (syntax-case (syntax (out ...)) ()
                      [() (syntax #f)]
                      [(str) (syntax str)])])
                (syntax (make-test descrip
                                   (quote body)
                                   (lambda () body)
                                   (make-expect expect-criterion printed-output))))]))))
    (values syntax->test-struct syntax->test-struct syntax->test-struct)))

;; current-tester : (union #f controller%)
;; the currently-registered tester gui.
(define current-tester (make-parameter #f)) 
  
;; run-tests : str x test ... -> void
;; side effect: runs all the tests.
(define run-tests
  (lambda (name . tests)
    (apply run-tests/initialize (list* name #f tests))))

;; run-tests/initialize : str x (-> void) x test ... -> void
;; side effect: queues a test-group that, is evaluated by running the provided 
;; initializer thunk, then running all the given tests in order.
;; Runs the tests in either the current-tester (if one is defined, meaning that 
;; this run-tests/initialize is being loaded from a complete manifest) or a
;; newly-created tester.
(define run-tests/initialize
  (lambda (name init-thunk . tests)
    (let ((the-group (make-test-group name init-thunk tests)))
      (if (current-tester)
          (send (current-tester) run-test-group the-group)
          (send (get-tester-gui) test-one-group the-group)))))
  
;; test-manifest : (listof require-spec-datum) -> thread
;; side effect: tests the given manifest. returns the thread on which
;; the gui is running.
(define (test-manifest man)
  (parameterize ([current-tester (get-tester-gui)])
    (send (current-tester) test-manifest man))))