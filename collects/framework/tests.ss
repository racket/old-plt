(require-library "refer.ss")

(require-relative-library "mred-interfacess.ss")

(define-signature framework:keys^
  (get-shifted-key-list))

(define-signature framework:test^
  (run-interval
   number-pending-actions
   reraise-error
   run-one

   current-get-eventspaces
   
   close-top-level-window
   
   button-push
   set-check-box!
   set-choice!
   keystroke  
   menu-select
   mouse-click
   new-window))
