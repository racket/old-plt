(module language-menu mzscheme
  
  (require
   (lib "class.ss")
   (lib "etc.ss")
   (lib "mred.ss" "mred")
   (lib "framework.ss" "framework")
   (lib "contracts.ss"))
  
  (provide/contract
   (language-menu-mixin mixin-contract))
  
  (define (language-menu-mixin super%)
    (class super%
      (inherit get-menu-bar)
      (inherit-field model tools)
      
      (super-instantiate ())
      
      ;; choose-language (-> void?)
      ;; presents the user with a language dialog and sets the language to the language picked
      (define/private (choose-language)
        (let ([language
               ((tools 'drscheme:language-configuration:language-dialog)
                false (send model get-language) this)])
          (send model set-language language)
          ((tools 'preferences:set)
           ((tools 'drscheme:language-configuration:get-settings-preferences-symbol))
           language)))
        
      (let ([language-menu
             (instantiate menu% ()
               (label "Language")
               (parent (get-menu-bar)))])
        (instantiate menu-item% ()
          (label "Choose Language ...")
          (parent language-menu)
          (callback
           (lambda (button event)
             (choose-language)))))
      
      (frame:reorder-menus this)
      ))
  )