(module tool mzscheme
  
  (provide test-case-box-tool@)
  
  (require
   (lib "class.ss")
   (lib "mred.ss" "mred")
   (lib "unitsig.ss")
   (lib "tool.ss" "drscheme")
   (lib "framework.ss" "framework")
   "private/test-case-box.ss")
  
  (define test-case-box-tool@
    (unit/sig drscheme:tool-exports^
     (import drscheme:tool^)
      
     (define (phase1) (void))
     (define (phase2) (void))
      
      (define test-case-mixin
        (mixin (drscheme:unit:frame<%> top-level-window<%>) ()
          (inherit get-special-menu get-edit-target-object)
          (super-instantiate ())
          (instantiate menu-item% ()
            (label "Insert Test Case")
            (parent (get-special-menu))
            (callback
             (lambda (menu event)
               (let ([test-box (new test-case-box%)]
                     [text (get-edit-target-object)])
                 (send text insert test-box)
                 (send test-box resize 600 100)
                 ;(send test-box set-aligned-min-sizes)
                 ;(send test-box resize
                 ;      (send test-box get-aligned-min-width)
                 ;      (send test-box get-aligned-min-height))
                 (send text set-caret-owner test-box 'global)))))))
      
      (drscheme:get/extend:extend-unit-frame test-case-mixin)
     ))
  )