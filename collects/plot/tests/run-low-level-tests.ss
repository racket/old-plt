; runs the low level tests


(require
 "fit-low-level-tests.ss"
 "plplot-low-level-tests.ss"
 (lib "test.ss" "schemeunit")
 (lib "text-ui.ss" "schemeunit"))

(test/text-ui
 (make-test-suite
   "low-level-suite"
   fit-low-level-tests
   plplot-low-level-tests))

   