(module case-tab mzscheme
  
  (require
   (lib "unitsig.ss")
   (lib "class.ss")
   (lib "mred.ss" "mred")
   (lib "etc.ss")
   "signatures.ss"
   "interfaces.ss")
  
  (provide case-tab@)
  
  (define case-tab@
    (unit/sig case^
      (import (super : case^))
      (define case%
        (class* super:case% (test-suite:item<%>)
          (inherit previous next)
          (inherit-field call expected actual test test-showing?)
          
          ;; focus-first (-> void?)
          ;; give keyboard focus to the call
          (define/public (focus-first)
            (send call set-caret-owner false 'global))
          
          ;; focus-last (-> void?)
          ;; give focus to the last text box
          (define/public (focus-last)
            (if test-showing?
                (send test set-caret-owner false 'global)
                (send expected set-caret-owner false 'global)))
          
          (define/private (register-keymap text back ahead)
            (let ([keymap (send text get-keymap)])
              (send keymap add-function "skip-ahead"
                    (lambda (ignored event) (ahead)))
              (send keymap add-function "skip-back"
                    (lambda (ignored event) (back)))
              (send keymap map-function "tab" "skip-ahead")
              (send keymap map-function "s:tab" "skip-back")
              (send text set-keymap keymap)))
          
          (super-instantiate ())
          
          (register-keymap
           call
           (lambda () (when (previous) (send (previous) focus-last)))
           (lambda () (send expected set-caret-owner false 'global)))
          (register-keymap
           expected
           (lambda () (send call set-caret-owner false 'global))
           (lambda () (if test-showing?
                          (send test set-caret-owner false 'global)
                          (when (next) (send (next) focus-first)))))
          (register-keymap
           test
           (lambda () (send expected set-caret-owner false 'global))
           (lambda () (when (next) (send (next) focus-first))))
          ))
      ))
  )