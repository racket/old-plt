;;
;; $Id: stsigs.ss,v 1.15 1998/02/26 23:54:57 steck Exp $
;;
;; Signatures for gui tester.
;;

(define-signature mred:testable-window^
  (test:get-active-frame
   test:get-focused-window
   test:current-get-eventspaces
   testable-panel%
   testable-canvas%
   testable-media-canvas%
   testable-text-window%
   testable-button%
   testable-check-box%
   testable-choice%
   testable-gauge%
   testable-list-box%
   testable-message%
   testable-radio-box%
   testable-slider%
   testable-text%
   testable-multi-text%
   testable-frame%
   testable-dialog-box% ))

(define-signature mred:test:run-export^
  (run-interval
   number-pending-actions
   reraise-error))

(define-signature mred:test:run-internal^
  (run-one))

(define-signature mred:test:run^
  ((open mred:test:run-export^)
   (open mred:test:run-internal^)))

(define-signature mred:test:primitives^
  (button-push
   set-check-box!
   set-choice!
   keystroke  
   menu-select
   mouse-click
   new-window))

(define-signature mred:test:drscheme^
  (get-defns-canvas 
   get-repl-canvas
   get-save-button
   get-check-syntax-button
   get-execute-button))

(define-signature mred:self-test^
  ((open mred:test:run^)
   (open mred:test:primitives^)
   (unit drs : mred:test:drscheme^)))

(define-signature mred:self-test-export^
  ((open mred:test:run-export^)
   (open mred:test:primitives^)
   (unit drs : mred:test:drscheme^)))
