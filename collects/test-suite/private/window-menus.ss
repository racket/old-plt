(module window-menus mzscheme
  
  (require
   (lib "unitsig.ss")
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
      
          ;; file-menu:between-new-and-open ((is-a?/c file-menu%) . -> . void?)
          ;; called when contructing the menu between new and open
           ;; status: this code is duplicated in the test-suite-tool
          (rename [super-file-menu:between-new-and-open
                   file-menu:between-new-and-open])
          (define/override (file-menu:between-new-and-open file-menu)
            (instantiate menu-item% ()
              (label "New Test Suite")
              (parent file-menu)
              (callback
               (lambda (menu event)
                 (send (instantiate window% ())
                       show true))))
            (super-file-menu:between-new-and-open file-menu))

           ;; file-menu:between-open-and-revert ((is-a?/c file-menu%) . -> . void?)
           ;; called when contructing the menu between open and revert
           ;; status: this code is duplicated in the test-suite-tool
           (rename [super-file-menu:between-open-and-revert
                    file-menu:between-open-and-revert])
           (define/override (file-menu:between-open-and-revert file-menu)
             (instantiate menu-item% ()
               (label "Open Test Suite...")
               (parent file-menu)
               (callback
                (lambda (menu event)
                  (let ([f (get-file false this)])
                    (when f
                      (let ([w (instantiate super:window% ())])
                        (if (file-exists? f)
                            (with-handlers
                                ([exn? (lambda (exn?)
                                         (message-box "Error" "Not a valid test-suite")
                                         (send w close))])
                              (send w load-file f)
                              (send w show true))
                            (begin
                              (message-box "Error" "That file does not exist")
                              (send w close)))))))))
             (super-file-menu:between-open-and-revert file-menu))
           
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
           
           (inherit-field model)
           (define/override (file-menu:print-callback item event)
             (send model print))
           
           (define/override (file-menu:create-print?) #t)
           
           ;; update-executing (boolean? . -> . void?)
           ;; called by the model when it is executing
           (rename [super-update-executing update-executing])
           (define/override (update-executing executing?)
             (if executing?
                 (begin
                   (send new-menu enable false)
                   (send delete-menu enable false)
                   (send execute-menu enable false))
                 (begin
                   (send new-menu enable true)
                   (send delete-menu enable true)
                   (send execute-menu enable true)))
             (super-update-executing executing?))
           
           (super-instantiate ())
           
           (field [test-menu (instantiate menu% ()
                               (label "Test")
                               (parent (get-menu-bar)))]
                  [new-menu (instantiate menu-item% ()
                              (label "New Test Case")
                              (parent test-menu)
                              (callback (lambda (m e) (new))))]
                  [delete-menu (instantiate menu-item% ()
                                 (label "Delete Test Case")
                                 (parent test-menu)
                                 (callback (lambda (m e) (delete))))]
                  [execute-menu (instantiate menu-item% ()
                                  (label "Execute")
                                  (parent test-menu)
                                  (callback (lambda (m e) (execute)))
                                  (shortcut #\t))]
                  [break-menu (instantiate menu-item% ()
                                (label "Break")
                                (parent test-menu)
                                (callback (lambda (m e) (break)))
                                (shortcut #\b))]
                  [show/hide-menu
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
                     (void))])
      
           (frame:reorder-menus this)
           ))
      ))
  )