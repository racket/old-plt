(require (planet "graphical-ui.ss" ("schematics" "schemeunit.plt" 1))
         (planet "test.ss" ("schematics" "schemeunit.plt" 1 1))
         "test-normalizer.ss"
         "closure-tests.ss"
         "labels-tests.ss")
         
(test/graphical-ui
 (make-test-suite
  "Main Tests for Prototype Web Server"
  test-normalizer-suite
  closure-tests-suite
  labels-tests-suite))

