(module classic-java-tests mzscheme

  (require "environment-tests.ss"
           "store-tests.ss"
           "parser-tests.ss"
           "program-tests.ss"
           "elaboration-tests.ss"
           "reduction-tests.ss"
           (lib "test.ss" "schemeunit"))

  (provide classic-java-tests)

  (define classic-java-tests
    (make-test-suite "Classic Java Implementation"
      environment-tests
      store-tests
      parser-tests
      program-tests
      elaboration-tests
      reduction-tests)))
