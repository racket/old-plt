;;
;; $Id$
;;
;; Signatures for gui tester.
;;

(define-signature mred:test:active-frame^
  (get-active-frame
   testable-frame%
   testable-dialog-box%))

(define-signature mred:test:global^
  ((struct event (thunk))))

(define-signature mred:test:run^
  (run-single
   run-multiple))

(define-signature mred:test:primitives^
  (menu-select menu-select-now))

(define-signature mred:self-test^
  ((open mred:test:active-frame^)
   (open mred:test:run^)
   (open mred:test:primitives^)))

(define-signature mred:self-test-export^ mred:self-test^)
