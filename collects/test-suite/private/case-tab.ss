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
          (inherit get-editor previous next)
          (inherit-field call expected actual test test-showing?)
          
          ;; focus-first (-> void?)
          ;; give keyboard focus to the call
          (define/public (focus-first)
            (send call set-caret-owner))
          
          ;; focus-last (-> void?)
          ;; give focus to the last text box
          (define/public (focus-last)
            (if test-showing?
                (send (get-editor) set-caret-owner test)
                (send (get-editor) set-caret-owner expected)))
          
          (define/private (register-keymap text back ahead)
            (let ([keymap (send text get-keymap)])
              (send keymap add-function "skip-ahead"
                    (lambda (ignored event)
                      (ahead)))
              (send keymap add-function "skip-back"
                    (lambda (ignored event)
                      (back)))
              (send keymap map-function "tab" "skip-ahead")
              (send keymap map-function "s:tab" "skip-back")
              (send text set-keymap keymap)))
          
          (super-instantiate ())
          
          (register-keymap
           call
           (lambda () (when (previous) (send (previous) focus-last)))
           (lambda () (send (get-editor) set-caret-owner (editor-parent expected) 'global)))
          (register-keymap
           expected
           (lambda () (send (get-editor) set-caret-owner (editor-parent call) 'global))
           (lambda () (if test-showing?
                          (send (get-editor) set-caret-owner (editor-parent test) 'global)
                          (when (next) (send (next) focus-last)))))
          (register-keymap
           test
           (lambda () (send (get-editor) set-caret-owner (editor-parent expected) 'global))
           (lambda () (when (next) (send (next) focus-last))))
          ))
      ))

  ;; editor-parent ((is-a?/c editor<%>) . -> . (union (is-a?/c cavas%) (is-a?/c snip%)))
  ;; gets the canvas or snip that the pasteboard is displayed in
  (define (editor-parent ed)
    (let ([admin (send ed get-admin)])
      (cond
        [(is-a? admin editor-snip-editor-admin<%>)
         (send admin get-snip)]
        [(is-a? admin editor-admin%)
         (send ed get-canvas)]
        [else false])))
  )