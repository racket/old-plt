(module htdp-langs mzscheme
  (require (lib "string-constant.ss" "string-constants")
           (lib "unitsig.ss")
           (lib "class.ss")
           (lib "tool.ss" "drscheme")
           (lib "macro.ss" "userspce")
           (lib "mred.ss" "mred"))
  
  (provide tool@)
  
  (define tool@
    (unit/sig () 
      (import [drscheme:frame^ : drscheme:frame^]
              [drscheme:unit^ : drscheme:unit^]
              [drscheme:rep : drscheme:rep^]
              [drscheme:get/extend : drscheme:get/extend^]
              [drscheme:language-tower : drscheme:language-tower^]
              [drscheme:language : drscheme:language^])
      
      (define htdp-language<%>
        (interface ()
          get-module
          get-language-position
          get-teachpack-names
          sharing-printing
          abbreviate-cons-as-list))
            
      ;; module-based-language-extension :    (implements drscheme:language-tower:module-based-language<%>) 
      ;;                                   -> (implements drscheme:language-tower:module-based-language<%>)
      ;; changes the default settings and sets a few more paramters during `on-execute'
      (define (module-based-language-extension super%)
        (class* super% ()
          (override default-settings on-execute)
          (rename [super-on-execute on-execute])
          (inherit sharing-printing abbreviate-cons-as-list)
          
          (define (default-settings)
            (drscheme:language-tower:make-simple-settings/parse
             `((case-sensitive #t)
               (printing-style constructor)
               (show-sharing ,(sharing-printing))
               (insert-newlines #t))))
          
          (define (on-execute settings run-in-user-thread)
            (super-on-execute settings run-in-user-thread))
          
          (super-instantiate ())))

      ;; add-htdp-language : (implements htdp-language<%>) -> void
      (define (add-htdp-language class%)
        (let ([% (drscheme:language-tower:module-based-language->language-mixin
                  (module-based-language-extension
                   (drscheme:language-tower:simple-module-based-language->module-based-language-mixin
                    class%)))])
          (drscheme:language:add-language
           (make-object %))))

      (add-htdp-language
       (class* object% (htdp-language<%>)
         (public get-module
                 get-language-position
                 sharing-printing
                 abbreviate-cons-as-list
                 get-teachpack-names)
         (define (get-module) '(lib "beginner.ss" "lang"))
         (define (get-language-position) '("How to Design Programs" "Beginning Student"))
         (define (sharing-printing) #f)
         (define (abbreviate-cons-as-list) #f)
         (define (get-teachpack-names) '(make-posn posn-x posn-y posn?))
         (super-instantiate ())))
      
      (add-htdp-language
       (class* object% (htdp-language<%>) 
         (public get-module
                 get-language-position
                 sharing-printing
                 abbreviate-cons-as-list
                 get-teachpack-names)
         (define (get-module) '(lib "intermediate.ss" "lang"))
         (define (get-language-position) '("How to Design Programs" "Intermediate Student"))
         (define (sharing-printing) #f)
         (define (abbreviate-cons-as-list) #t)
         (define (get-teachpack-names) '(make-posn posn-x posn-y posn?))
         (super-instantiate ())))
      
      (add-htdp-language
       (class* object% (htdp-language<%>) 
         (public get-module
                 get-language-position
                 sharing-printing
                 abbreviate-cons-as-list
                 get-teachpack-names)
         (define (get-module) '(lib "advanced.ss" "lang"))
         (define (get-language-position) '("How to Design Programs" "Advanced Student"))
         (define (sharing-printing) #t)
         (define (abbreviate-cons-as-list) #t)
         (define (get-teachpack-names) '(make-posn posn-x posn-y posn? set-posn-x! set-posn-y!))
         (super-instantiate ()))))))
