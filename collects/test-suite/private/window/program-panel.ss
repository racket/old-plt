(module program-panel mzscheme
  
  (require
   (lib "class.ss")
   (lib "mred.ss" "mred")
   (lib "etc.ss")
   (lib "contracts.ss")
   "make-bitmap.ss")
  
  (provide/contract
   (program-panel-mixin mixin-contract))
  
  ;; make a panel to control the program to be tested of the given test suite in the given frame
  (define (program-panel-mixin super%)
    (class super%
      
      (super-instantiate ())
      
      (inherit-field model main-panel)
      
      (field
       [program-panel
        (instantiate horizontal-panel% ()
          (parent main-panel)
          (style '(border))
          (stretchable-height false))])
      
      (instantiate button% ()
        (label ((make-bitmap "Browse") main-panel))
        (parent program-panel)
        (callback 
         (lambda (button event)
           (let ([filename (get-file)])
             (when filename
               (send model set-program filename))))))
      (field
       [program-canvas
        (instantiate editor-canvas% ()
          (parent program-panel)
          (editor (send model get-program))
          (style '(no-hscroll no-vscroll))
          (stretchable-width true))])
      
      (send program-canvas set-line-count 1)
      ))
  )