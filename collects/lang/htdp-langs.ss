(module htdp-langs mzscheme
  (require (lib "string-constant.ss" "string-constants")
           (lib "pretty.ss")
           (prefix pc: (lib "pconvert.ss"))
           (lib "unitsig.ss")
           (lib "class.ss")
           (lib "tool.ss" "drscheme")
           (lib "macro.ss" "userspce")
           (lib "mred.ss" "mred"))
  
  (provide tool@)
  
  (define tool@
    (unit/sig () 
      (import drscheme:tool^)
      
      (define htdp-language<%>
        (interface ()
          get-module
          get-language-position
          get-teachpack-names
          sharing-printing
          abbreviate-cons-as-list))
            
      ;; module-based-language-extension :    (implements drscheme:language:module-based-language<%>) 
      ;;                                   -> (implements drscheme:language:module-based-language<%>)
      ;; changes the default settings and sets a few more paramters during `on-execute'
      (define (module-based-language-extension super%)
        (class* super% ()
          (override default-settings on-execute render-value/format render-value)
          (rename [super-on-execute on-execute]
                  [super-render-value/format render-value/format]
                  [super-render-value render-value])
          (inherit sharing-printing abbreviate-cons-as-list)
          
          (define (default-settings)
            (drscheme:language:make-simple-settings/parse
             `((case-sensitive #t)
               (printing-style constructor)
               (show-sharing ,(sharing-printing))
               (insert-newlines #t))))
          
          (define (on-execute settings run-in-user-thread)
            (run-in-user-thread
             (lambda ()
               (read-decimal-as-inexact #f)
               (read-dot-as-symbol #t)))
            (super-on-execute settings run-in-user-thread))

          (define (set-printing-parameters thunk)
            (parameterize ([pc:booleans-as-true/false #t]
                           [pc:abbreviate-cons-as-list (abbreviate-cons-as-list)]
                           [pretty-print-show-inexactness #t]
                           [pretty-print-.-symbol-without-bars #t])
              (thunk)))
          
          (define (render-value/format value settings port put-snip)
            (set-printing-parameters
             (lambda ()
               (super-render-value/format value settings port put-snip))))
          
          (define (render-value value settings port put-snip)
            (set-printing-parameters
             (lambda ()
               (super-render-value value settings port put-snip))))
          
          (super-instantiate ())))

      ;; add-htdp-language : (implements htdp-language<%>) -> void
      (define (add-htdp-language class%)
        (let ([% (drscheme:language:module-based-language->language-mixin
                  (module-based-language-extension
                   (drscheme:language:simple-module-based-language->module-based-language-mixin
                    class%)))])
          (drscheme:language-configuration:add-language
           (make-object %)
           #t)))

      (add-htdp-language
       (class* object% (htdp-language<%> drscheme:language:simple-module-based-language<%>) 
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
         (super-instantiate ())))
      
      (add-htdp-language
       (class* object% (htdp-language<%> drscheme:language:simple-module-based-language<%>) 
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
       (class* object% (htdp-language<%> drscheme:language:simple-module-based-language<%>)
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
         (super-instantiate ()))))))
