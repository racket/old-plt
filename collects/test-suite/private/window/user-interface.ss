(module user-interface mzscheme
  
  (require
   (lib "class.ss")
   (lib "etc.ss")
   (lib "mred.ss" "mred")
   (lib "framework.ss" "framework")
   (lib "contracts.ss")
   "make-bitmap.ss")
  
  (provide/contract
   (user-interface-mixin mixin-contract))
  
  ;; callbacks-mixin mixin-contract
  ;; a mixin that contains fields of the callbacks used in the buttons and menus
  (define (callbacks-mixin super%)
    (class super%
      (super-instantiate ())
      (inherit get-error-handler update-executing)
      (inherit-field model)
      
      (field
       [save-callback
        (lambda (button event)
          (send model save-file))]
       [break-callback
        (lambda (button event)
          (send model break))]
       [execute-callback
        (lambda (button event)
          (send model execute))]
       [delete-callback
        (lambda (button event)
          (send model delete-case))]
       [new-callback
        (lambda (button event)
          (send model insert-case))])
       ))
  
  ;; test-menu-mixin mixin-contract
  ;; make a menu item that contains the test suite specific tasks
  (define (test-menu-mixin super%)
    (class super%
      
      (super-instantiate ())
      (inherit get-menu-bar)
      (inherit-field new-callback delete-callback execute-callback break-callback save-callback model)
      
      (let ([test-menu (instantiate menu% ()
                         (label "Test")
                         (parent (get-menu-bar)))])
        (instantiate menu-item% ()
          (label "New Test Case")
          (parent test-menu)
          (callback new-callback)
          (shortcut #\n))
        (instantiate menu-item% ()
          (label "Delete Test Case")
          (parent test-menu)
          (callback delete-callback)
          (shortcut #\d)
        (instantiate menu-item% ()
          (label "Execute")
          (parent test-menu)
          (callback execute-callback)
          (shortcut #\e))
        (instantiate menu-item% ()
          (label "Break")
          (parent test-menu)
          (callback break-callback)
          (shortcut #\b))
        (letrec ([show/hide
                  (instantiate menu-item% ()
                    (label "Show Equality Tests")
                    (parent test-menu)
                    (callback
                     (let ([tests-showing? false])
                       (lambda (button event)
                         (if tests-showing?
                             (begin
                               (set! tests-showing? false)
                               (send show/hide set-label "Show Equality Tests")
                               (send model show-tests false))
                             (begin
                               (set! tests-showing? true)
                               (send show/hide set-label "Hide Equality Tests")
                               (send model show-tests true))))))
                    (shortcut #\t))])
          (void)))
      
      (frame:reorder-menus this)
      ))
  
  ;; button-panel-mixin mixin-contract
  ;; make a button panel in the given frame that performs actions on the given test suite
  (define (button-panel-mixin super%)
    (class super%
      
      (super-instantiate ())
      (inherit get-error-handler)
      (inherit-field model main-panel new-callback delete-callback
                     execute-callback break-callback save-callback)
      
      ;; update-executing (boolean? . -> . void?)
      ;; called when the model changes execution modes
      (rename [super-update-executing update-executing])
      (define/override (update-executing executing?)
        (if executing?
            (begin
              (send new-button enable false)
              (send delete-button enable false)
              (send execute-button enable false))
            (begin
              (send new-button enable true)
              (send delete-button enable true)
              (send execute-button enable true)))
        (super-update-executing executing?))
      
      ;; update-modified (boolean? . -> . void?)
      ;; called when the model is modified or saved
      (rename [super-update-modified update-modified])
      (define/override (update-modified modified?)
        (if modified?
            (send save-button show true)
            (send save-button show false))
        (super-update-modified modified?))
      
      (field
       [button-panel
        (instantiate horizontal-panel% ()
          (parent main-panel)
          (stretchable-height false))]
       [save-button
        (instantiate button% ()
          (label
           ((make-bitmap
             "Save"
             (build-path
              (collection-path "icons")
              "save.bmp"))
            main-panel))
          (parent button-panel)
          (callback save-callback))]
       [spacer
        (instantiate horizontal-panel% ()
          (stretchable-width true)
          (parent button-panel))]
       [new-button
        (instantiate button% ()
          (label ((make-bitmap "New") main-panel))
          (parent button-panel)
          (callback new-callback))]
       [delete-button
        (instantiate button% ()
          (label ((make-bitmap "Delete") main-panel))
          (parent button-panel)
          (callback delete-callback))]
       [execute-button
        (instantiate button% ()
          (label
           ((make-bitmap
             "Execute"
             (build-path
              (collection-path "icons")
              "execute.bmp"))
            main-panel))
          (parent button-panel)
          (callback execute-callback))]
       [break-button
        (instantiate button% ()
          (label
           ((make-bitmap
             "Break"
             (build-path
              (collection-path "icons")
              "break.bmp"))
            main-panel))
          (parent button-panel)
          (callback break-callback))])
      
      (send save-button show false)
      ))
  
  ;; save-menu-items-mixin mixin-contract
  ;; adds the save and save as menu items to the window
  (define (save-menu-items-mixin super%)
    (class super%
      (super-instantiate ())
      (inherit-field model)
      
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
        (send model save-file))
      
      ;; file-menu:save-as-callback ((is-a?/c menu-item%) (is-a?/c event%) . -> . void?)
      ;; called when the save-as menu item is clicked
      (define/override (file-menu:save-as-callback item event)
        (send model save-file ""))
      ))
  
  ;; interface-mixin mixin-contract
  ;; add an interface for test suite functions to the frame
  (define (user-interface-mixin super%)
    (save-menu-items-mixin
     (button-panel-mixin
      (test-menu-mixin
       (callbacks-mixin
        super%)))))
  )