(module window-menus mzscheme
  
  (require
   (lib "unitsig.ss")
   (lib "unit.ss")
   (lib "class.ss")
   (lib "etc.ss")
   (lib "mred.ss" "mred")
   (lib "framework.ss" "framework")
   "signatures.ss"
   "interfaces.ss")
  
  (provide window-menus@)
  
  (define window-menus@
    (unit/sig window^
       (import (super : window^))
       (define window%
         (class* super:window% (test-suite:window<%>)
           (inherit get-menu-bar)
           (inherit-field new delete execute break save-as save show-tests)
      
           ;; file-menu:create-save? (-> boolean?)
           ;; called to check whether the save menu should be created
           (define/override (file-menu:create-save?)
             true)
           
           ;; file-menu:create-save-as? (-> boolean?)
           ;; called to check wether the save-as menu should be created
           (define/override (file-menu:create-save-as?)
             true)
           
           ;; file-menu:save-callback ((is-a?/c menu-item%) (is-a?/c event%) . -> . void?)
           ;; called when the save menu item is clicked
           (define/override (file-menu:save-callback item event)
             (save))
           
           ;; file-menu:save-as-callback ((is-a?/c menu-item%) (is-a?/c event%) . -> . void?)
           ;; called when the save-as menu item is clicked
           (define/override (file-menu:save-as-callback item event)
             (save-as))
           
           (super-instantiate ())
           
           (let ([test-menu (instantiate menu% ()
                              (label "Test")
                              (parent (get-menu-bar)))])
             (instantiate menu-item% ()
               (label "New Test Case")
               (parent test-menu)
               (callback (lambda (m e) (new))))
             (instantiate menu-item% ()
               (label "Delete Test Case")
               (parent test-menu)
               (callback (lambda (m e) (delete))))
             (instantiate menu-item% ()
               (label "Execute")
               (parent test-menu)
               (callback (lambda (m e) (execute)))
               (shortcut #\t))
             (instantiate menu-item% ()
               (label "Break")
               (parent test-menu)
               (callback (lambda (m e) (break)))
               (shortcut #\b))
             (letrec ([show/hide
                       (instantiate menu-item% ()
                         (label "Show Equality Tests")
                         (parent test-menu)
                         (callback
                          (let ([tests-showing? false])
                            (lambda (menu event)
                              (if tests-showing?
                                  (begin
                                    (set! tests-showing? false)
                                    (send show/hide set-label "Show Equality Tests")
                                    (show-tests false))
                                  (begin
                                    (set! tests-showing? true)
                                    (send show/hide set-label "Hide Equality Tests")
                                    (show-tests true)))))))])
               (void)))
      
           (frame:reorder-menus this)
           ))
      ))
  )