(module tool mzscheme
  
  (provide tool@)
  
  (require
   (lib "class.ss")
   (lib "mred.ss" "mred")
   (lib "unitsig.ss")
   (lib "tool.ss" "drscheme")
   "private/example-box.ss"
   "private/interactions-box.ss")
  
  (define extentions@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^ example-box^ interactions-box^)
      
      (define (phase1) (void))
      (define (phase2) (void))
      
      (define (frame-mixin %)
        (class %
          (inherit get-edit-target-object get-special-menu)
          
          ;; this function is copied from the drscheme/private/unit.ss file
          (define (has-editor-on-demand menu-item)
            (let ([edit (get-edit-target-object)])
              (send menu-item enable (and edit (is-a? edit editor<%>)))))
          
          (super-new)
          
          (new menu-item%
               (label "Insert Java Examples")
               (parent (get-special-menu))
               (callback
                (lambda (menu event)
                  (let ([box (new example-box%)]
                        [text (get-edit-target-object)])
                    (when text
                      (send text begin-edit-sequence)
                      (send text insert box)
                      #;(send box take-caret)
                      (send text end-edit-sequence)))))
               (demand-callback has-editor-on-demand))
          
          (new menu-item%
               (label "Insert Java Interactions")
               (parent (get-special-menu))
               (callback
                (lambda (menu event)
                  (let ([box (new interactions-box%)]
                        [text (get-edit-target-object)])
                    (when text
                      (send text begin-edit-sequence)
                      (send text insert box)
                      #;(send box take-caret)
                      (send text end-edit-sequence)))))
               (demand-callback has-editor-on-demand))
          ))

      (drscheme:get/extend:extend-unit-frame frame-mixin)))
  
  (define tool@
    (compound-unit/sig
      (import (TOOL : drscheme:tool^))
      (link (EXT : drscheme:tool-exports^ (extentions@ TOOL EXAMPLES INTERACTIONS))
            (EXAMPLES : example-box^ (example-box@ TOOL))
            (INTERACTIONS : interactions-box^ (interactions-box@ TOOL)))
      (export (var (EXT phase1))
              (var (EXT phase2)))))
  )
