(module error-panel mzscheme
  
  (require
   (lib "class.ss")
   (lib "list.ss")
   (lib "etc.ss")
   (lib "mred.ss" "mred")
   (lib "contracts.ss"))
  
  (provide/contract
   (error-panel-mixin mixin-contract))
  
  ;; make an error display panel in the window
  (define (error-panel-mixin super%)
    (class super% 
      
      (super-instantiate ())
      (inherit-field main-panel)
      
      ;; get-error-handler (-> (string? exn? . -> . void?))
      ;; an error handler for displaying errors to the window
      (define/override (get-error-handler)
        (lambda (msg exn)
          (show-error-panel)
          (update-executing false)
          (send* error-text
            (lock false)
            (erase)
            (insert msg)
            (lock true))))
      
      ;; update-executing (boolean? . -> . void?)
      ;; called when the model is being executed
      (rename [super-update-executing update-executing])
      (define/override (update-executing executing?)
        (when executing? (hide-error-panel))
        (super-update-executing executing?))
      
      (define/private (hide-error-panel)
        (when (member error-panel (send main-panel get-children))
          (send main-panel change-children
                (lambda (l) (remq error-panel l)))))
      
      (define/private (show-error-panel)
        (unless (member error-panel (send main-panel get-children))
          (send main-panel change-children
                (lambda (l) (append l (list error-panel))))))
      
      (field
       [error-panel (instantiate horizontal-panel% ()
                      (parent main-panel)
                      (style '(border))
                      (stretchable-height false))]
       [label-panel (instantiate vertical-panel% ()
                      (parent error-panel)
                      (stretchable-width false)
                      (stretchable-height false))]
       [error-text (instantiate text% ()
                     (auto-wrap true))]
       [ec (instantiate editor-canvas% ()
             (parent error-panel)
             (editor error-text)
             (style '(no-hscroll))
             (stretchable-height true)
             (stretchable-width true))])
      
      (instantiate message% ()
        (parent label-panel)
        (label "Test suite")
        (stretchable-width true))
      
      (instantiate message% ()
        (parent label-panel)
        (label "Error Message")
        (stretchable-width true))
      
      (instantiate button% ()
        (parent error-panel)
        (label "Hide")
        (callback
         (lambda (button event)
           (hide-error-panel)))
        (stretchable-height true))
      
      (send error-text lock true)
      (send ec set-line-count 3)
      (hide-error-panel)
      ))
  )