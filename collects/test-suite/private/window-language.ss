(module window-language mzscheme
  
  (require
   (lib "unitsig.ss")
   (lib "class.ss")
   (lib "list.ss")
   (lib "etc.ss")
   (lib "mred.ss" "mred")
   (lib "framework.ss" "framework")
   (lib "tool.ss" "drscheme")
   "signatures.ss"
   "interfaces.ss")
  
  (provide window-language@)
  
  (define window-language@
    (unit/sig window^
      (import drscheme:tool^ (super : window^))
      (define window%
        (class super:window%
          (inherit get-menu-bar)
          (inherit-field model)
          
          ;; fill-menu ((is-a?/c menu%) . -> . void?)
          ;; fill the menu with the teachpacks
          (define (fill-menu menu)
            (for-each
             (lambda (item)
               (send item delete))
             (send menu get-items))
            (send choose-language restore)
            (send seperator restore)
            (send add-teachpack restore)
            (send clear-teachpacks restore)
            (let ([teachpacks (send model get-teachpacks)])
              (when (empty? teachpacks)
                (send clear-teachpacks enable false))
              (for-each
               (lambda (tp)
                 (instantiate menu-item% ()
                   (label (format "Clear ~a Teachpack" tp))
                   (parent menu)
                   (callback
                    (lambda (item menu)
                      (send model remove-teachpack tp)))))
               teachpacks)))
  
          (super-instantiate ())
          
          (field [language-menu
                  (instantiate menu% ()
                    (label "Language")
                    (parent (get-menu-bar))
                    (demand-callback fill-menu))]
                 [choose-language
                  (instantiate menu-item% ()
                    (label "Choose Language ...")
                    (parent language-menu)
                    (callback
                     (lambda (item event)
                       (send model set-language
                             (drscheme:language-configuration:language-dialog
                              false (send model get-language) this))))
                    (shortcut #\l))]
                 [seperator (instantiate separator-menu-item% ()
                              (parent language-menu))]
                 [add-teachpack
                  (instantiate menu-item% ()
                    (label "Add Teachpack...")
                    (parent language-menu)
                    (callback
                     (lambda (item event)
                       (let ([tp (get-file false this)])
                         (when tp (send model add-teachpack tp))))))]
                  [clear-teachpacks
                   (instantiate menu-item% ()
                     (label "Clear All Teachpacks")
                     (parent language-menu)
                     (callback
                      (lambda (item event)
                        (send model clear-teachpacks))))])
                    
           (frame:reorder-menus this)
          ))
      ))
  )