;;
;; $Id: stsigs.ss,v 1.17 1998/05/12 21:52:54 steck Exp $
;;
;; Signatures for gui tester.
;;

(define-signature framework:test:run-export^
  (run-interval
   number-pending-actions
   reraise-error
   current-eventspaces))

(define-signature framework:test:run-internal^
  (get-active-frame
   get-focused-window
   run-one))

(define-signature framework:test:run^
  ((open framework:test:run-export^)
   (open framework:test:run-internal^)))

(define-signature framework:test:primitives^
  (button-push
   set-check-box!
   set-choice!
   keystroke  
   menu-select
   mouse-click
   new-window))

(define-signature framework:test^
  ((open framework:test:run-export^)
   (open framework:test:primitives^)))
