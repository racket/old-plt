;; note:   It bothers me that this file references the frame:standard-menus as it is merely a base
;;       window class and should allow the layout mixin to define the fact that it has standard menus
;;         The get-file call should use the teachpack directory as a default
(module window mzscheme
  
  (require
   (lib "unitsig.ss")
   (lib "class.ss")
   (lib "etc.ss")
   (lib "framework.ss" "framework")
   (lib "tool.ss" "drscheme")
   (lib "mred.ss" "mred")
   (lib "list.ss")
   "signatures.ss"
   "interfaces.ss")
  
  (provide window@)
  
  (define window@
    (unit/sig window^
      (import drscheme:tool^ model^)
      (define window%
        (class* frame:standard-menus% (test-suite:window<%>)
          
          (init-field
           (filename false)
           (program false))
          
          (field [model (instantiate model% ()
                          (window this))]
                 [save (lambda () (send model save-file))]
                 [save-as (lambda () (send model save-file ""))]
                 [break (lambda () (send model break))]
                 [execute (lambda () (send model execute))]
                 [delete (lambda () (send model delete-case))]
                 [new (lambda () (send model insert-case))]
                 [show-tests (lambda (show?) (send model show-tests show?))])
          
          ;; load-file (string? . -> . void?)
          ;; loads a file to the model
          (define/public (load-file filename)
            (send model load-file filename))
          
          ;; update-modified (boolean? . -> . void?)
          ;; called by the model when it has been modified
          (define/public (update-modified modified?)
            (void))
          
          ;; update-executing (boolean? . -> . void?)
          ;; called by the model when it is executing
          (define/public (update-executing executing?)
            (void))
          
          ;; get-error-display-handler (-> (string? exn? . -> . void?))
          ;; the error handler that is used to display errors to the window
          (define/public (get-error-handler)
            (lambda (message error)
              (update-executing false)))
          
          (super-instantiate ()
            (label "Test Suite"))
          
          (when program (send model set-program program))
          ;;why doesn't this line work? sending filename as an init
          ;;field yields an undefined class
          (when filename (send model load-file filename))
          ))
      ))
  )
