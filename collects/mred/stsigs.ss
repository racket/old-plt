;;
;; $Id: stsigs.ss,v 1.4 1997/07/07 19:25:58 krentel Exp krentel $
;;
;; Signatures for gui tester.
;;

(define-signature mred:testable-window^
  (test:get-active-frame
   test:get-focused-window
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

(define-signature mred:test:struct^
  ((struct event (thunk))))

(define-signature mred:test:globals^
  (top-frame
   top-panel
   frame->menu-bar
   menu-bar->item-id
   frame->active-canvas))    

(define-signature mred:test:run^
  (run-single
   run-multiple))

(define-signature mred:test:primitives^
  (button-push   button-push-now
   keystroke     keystroke-now
   menu-select   menu-select-now
   mouse-click   mouse-click-now))

(define-signature mred:test:drscheme^
  (get-defns-canvas 
   get-repl-canvas
   get-save-button
   get-check-syntax-button
   get-execute-button))

(define-signature mred:self-test^
  ((open mred:test:struct^)
   (open mred:test:run^)
   (open mred:test:primitives^)
   (unit drs : mred:test:drscheme^)))

(define-signature mred:self-test-export^ mred:self-test^)
