(module tool mzscheme
  
  (provide tool@)
  
  (require
   (lib "etc.ss")
   (lib "class.ss")
   (lib "mred.ss" "mred")
   (lib "unitsig.ss")
   (lib "tool.ss" "drscheme")
   (lib "framework.ss" "framework")
   #;(lib "string-constant.ss" "string-constants")
   "private/java-class-box.ss"
   "private/java-union-box.ss")
  
  ;; Temp
  (define-syntax (string-constant stx)
    (syntax-case stx ()
      [(_ s)
       #'(case (quote s)
           [(profj-insert-class-box) "Insert Java Class Box"]
           [(profj-insert-union-box) "Insert Java Union Box"])]))
  
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      
      (define (phase1) (void))
      (define (phase2) (void))
      
      ;; Adds two menu items to the special menu for inserting java class and union boxes
      ;; STATUS: use the mixin macro
      (define (java-boxes-menu-mixin %)
        (class %
          (inherit get-definitions-text get-edit-target-object get-menu-bar
                   get-special-menu)
          
          ;; this function is copied from the drscheme/private/unit.ss file
          (define (has-editor-on-demand menu-item)
            (let ([edit (get-edit-target-object)])
              (send menu-item enable (and edit (is-a? edit editor<%>)))))
          
          (super-new)
          
          (new menu-item%
               (label (string-constant profj-insert-class-box))
               (parent (get-special-menu))
               (callback
                (lambda (menu event)
                  (let ([class-box (new java-class-box%)]
                        [text (get-edit-target-object)])
                    (when text
                      (send text begin-edit-sequence)
                      (send text insert class-box)
                      #;(send class-box take-caret)
                      (send text end-edit-sequence)))))
               (demand-callback has-editor-on-demand))
          
          (new menu-item%
               (label (string-constant profj-insert-union-box))
               (parent (get-special-menu))
               (callback
                (lambda (menu event)
                  (let ([union-box (new java-union-box%)]
                        [text (get-edit-target-object)])
                    (when text
                      (send text begin-edit-sequence)
                      (send text insert union-box)
                      #;(send union-box take-caret)
                      (send text end-edit-sequence)))))
               (demand-callback has-editor-on-demand))
          ))

      (drscheme:get/extend:extend-unit-frame java-boxes-menu-mixin)
      ))
  )
