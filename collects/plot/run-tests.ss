; run the low level tests before anything else
(display "running low level tests")
(load "tests/run-low-level-tests.ss")
(newline)

(display "runing rest of test suite")
(newline)
(require 
 (lib "math-tests.ss" "plot" "tests")
 (lib "renderer-helpers-tests.ss" "plot" "tests"))
(test/text-ui
 (make-test-suite
   "plot package tests"
   math-tests
   renderer-helpers-tests))
           
     