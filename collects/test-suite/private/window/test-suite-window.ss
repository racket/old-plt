(module test-suite-window mzscheme
  
  (require
   (lib "class.ss")
   (lib "etc.ss")
   (lib "mred.ss" "mred")
   (lib "contracts.ss")
   (lib "framework.ss" "framework")
   (lib "aligned-pasteboard.ss" "mrlib")
   "../model/test-suite-model.ss"
   "itest-suite-window.ss"
   "language-menu.ss"
   "error-panel.ss"
   "button-panel.ss"
   "program-panel.ss")
  
  (provide/contract
   (test-suite-window% class?))
  
  (define (core-test-window-mixin super%)
    (class* super% (test-suite-window<%>)
      
      (inherit get-area-container)
      
      (super-instantiate ()
        (label "Test Suite")
        (height 500))
      
      (init-field
       tools
       [filename false])
      
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
          (void)))
      
      (field [model (instantiate test-suite-model% ()
                      (window this)
                      (tools tools))]
             [main-panel (instantiate vertical-panel% ()
                           (parent (send this get-area-container))
                           (stretchable-height false))])
      
      ;; status:
      ;; the following code when coupled with a filename init-field
      ;; causes the object to be undefined in test-suite-tool.ss I'll
      ;; figure out why later.
      ;(when filename
      ;  (send model load-file filename))
      
      (instantiate aligned-editor-canvas% ()
        (parent (get-area-container))
        (editor model)
        (style '(no-hscroll)))
      ))
     
  ;; a window class for displaying test-suites
  (define test-suite-window%
    (language-menu-mixin
     (error-panel-mixin
      (program-panel-mixin
       (interface-mixin
        (core-test-window-mixin
         frame:standard-menus%))))))
  )
