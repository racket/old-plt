(module classic-java-tests mzscheme

  (require "store-tests.ss"
           "parser-tests.ss"
           "program-tests.ss"
           "elaboration-tests.ss"
           "reduction-tests.ss"
           (planet "test.ss" ("schematics" "schemeunit.plt" 1)))

  (provide classic-java-tests)
  
  (print-struct #t)

  (define classic-java-tests
    (make-test-suite "Classic Java Implementation"
      store-tests
      parser-tests
      program-tests
      elaboration-tests
      reduction-tests)))
