;; This is a dummy version of the test-harness that turns every instance of test
;; and tests into comments. This is the one that normally gets required.
;; To actually run the tests, follow these steps:
;; 1. At the scheme prompt type (require "the-real-test-harness.ss")
;; 2. Then type (run-suites <file-name> ...)
;;    where <file-name> is the name of a module that contains actual instances of test, and tests.
(module test-harness mzscheme
  (provide tests test)
  
  (define-syntax tests
    (lambda (stx)
      (syntax-case stx ()
        [(_ rest ...) (syntax (void))])))
  
  (define-syntax test
    (lambda (stx)
      (syntax-case stx ()
        [(_ rest ...) (syntax (void))]))))