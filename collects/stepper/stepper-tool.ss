(module stepper-tool mzscheme
  (require (lib "tool.ss" "drscheme")
           (lib "mred.ss" "mred")
           (lib "unitsig.ss")
           (lib "class.ss")
           (lib "list.ss"))
  
  (provide tool@)
  
  (define tool@
    (unit/sig ()
      (import drscheme:tool^)
      
      (define stepper-bitmap
        (drscheme:unit:make-bitmap
         "Step"
         (build-path (collection-path "icons") "foot.bmp")))
      
      (drscheme:get/extend:extend-unit-frame
       (lambda (super%)
         (class super% ()
           
           (inherit get-button-panel)
           (rename [super-disable-evaluation disable-evaluation]
                   [super-enable-evaluation enable-evaluation])
           
           (override enable-evaluation disable-evaluation)
           
           (super-instantiate ())
           
           (define stepper-button 
             (make-object button%
               (stepper-bitmap this)
               (get-button-panel)
               (lambda (button evt) 
                 '(invoke-stepper this))))
           
           (define (enable-evaluation)
             (send stepper-button enable #t)
             (super-enable-evaluation))
           
           (define (disable-evaluation)
             (send stepper-button enable #f)
             (super-disable-evaluation))
           
           (define frame-internal #f)
           
           (define stepper-frame
               (case-lambda
                 (() frame-internal)
                 ((new-val) (set! frame-internal new-val))))
           
           (send (get-button-panel) change-children
                 (lambda (l)
                   (cons stepper-button (remq stepper-button l))))))))))