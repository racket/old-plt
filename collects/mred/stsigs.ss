;;
;; $Id: stsigs.ss,v 1.1 1997/07/02 21:23:24 krentel Exp krentel $
;;
;; Signatures for gui tester.
;;

(define-signature mred:test:active-frame^
  (get-active-frame
   testable-frame%
   testable-dialog-box%))

(define-signature mred:test:struct^
  ((struct event (thunk))))

(define-signature mred:test:globals^
  (top-frame
   frame->menu-bar
   menu-bar->item-id
   frame->active-canvas))    

(define-signature mred:test:run^
  (run-single
   run-multiple))

(define-signature mred:test:primitives^
  (menu-select menu-select-now
   keystroke keystroke-now))

(define-signature mred:test:drscheme^
  (get-defns-canvas 
   get-repl-canvas))

(define-signature mred:self-test^
  ((open mred:test:active-frame^)
   (open mred:test:struct^)
   (open mred:test:run^)
   (open mred:test:primitives^)
   (open mred:test:drscheme^)))

(define-signature mred:self-test-export^ mred:self-test^)
