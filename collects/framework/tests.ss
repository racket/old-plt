(require-library "refer.ss")

(require-library "mred-interfaces.ss" "framework")

(define-signature framework:keys^
  (shifted-key-list
   get-shifted-key-list))

(define-signature framework:test^
  (run-interval
   number-pending-actions
   reraise-error
   run-one

   current-eventspaces

   button-push
   set-check-box!
   set-choice!
   keystroke  
   menu-select
   mouse-click
   new-window))
