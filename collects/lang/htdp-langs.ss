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
          sharing-printing
          abbreviate-cons-as-list))
      
      ;; settings structure
      (define-struct/parse 
       setting
       (case-sensitive
        printing-style
        sharing-printing
        booleans-as-true/false
        use-pretty-printer))

      (define htdp-language->module-based-language%
        (class* object% (drscheme:language-tower:module-based-language<%>)
          (init-field htdp-language)
          (public marshall-settings unmarshall-settings
                  default-settings default-settings?
                  get-module config-panel on-execute
                  get-teachpack-names
                  render-value/format render-value
                  get-language-position)

          (define (get-language-position) (send htdp-language get-language-position))

          (define (marshall-settings settings)
            (setting/unparse settings))
          (define (unmarshall-settings printable)
            (and (list? printable)
                 (= (length printable)
                    (procedure-arity make-setting))
                 (andmap (lambda (x) (and (list? x) 
                                          (= 2 (length x))
                                          (symbol? (car x))))
                         printable)
                 (make-setting/parse printable)))
          
          (define (default-settings? s) (equal? (default-settings) s))

          (define (default-settings)
            (make-setting/parse
             `((case-sensitive #t)
               (printing-style 'constructor-style)
               (use-pretty-printer #t)
               (sharing-printing ,(send htdp-language sharing-printing))
               (abbreviate-cons-as-list ,(send htdp-language abbreviate-cons-as-list))
               (booleans-as-true/false #t))))
          
          (define (get-module) (send htdp-language get-module))
          
          (define (config-panel parent settings)
            (htdp-language-config-panel parent settings))
          
          (define (get-teachpack-names) (send htdp-language get-teachpack-names))
          
          (define (on-execute settings run-in-user-thread)
            (void))
      
          (define (render-value/format val settings port dump-snip)
            (display "value" port))
          (define (render-value val settings port dump-snip)
            (display "value" port))
          
          (super-instantiate ())))

      ;; htdp-language-config-panel : ((instanceof panel<%>) -> (-> setting))
      ;; constrcts the standard settings panel
      (define (htdp-language-config-panel parent settings)
        (let* ([make-sub-panel
                (lambda (name panel)
                  (let* ([p (make-object vertical-pane% panel)]
                         [message (make-object message% name p)])
                    (make-object vertical-panel% parent '(border))))]
               [input-syntax-panel (make-sub-panel (string-constant input-syntax) parent)]
               [output-syntax-panel (make-sub-panel (string-constant output-syntax) parent)]
               [right-align
                (lambda (mo panel)
                  (let* ([hp (make-object horizontal-pane% panel)])
                    (begin0
                      (mo hp)
                      (make-object horizontal-pane% hp))))]
               [make-check-box
                (lambda (name panel)
                  (right-align
                   (lambda (hp)
                     (make-object check-box% name hp void))
                   panel))]
               [symbol->printer-number
                (lambda (printing-setting)
                  (case printing-setting
                    [(constructor-style) 0]
                    [(quasi-style) 1]
                    [(r4rs-style) 2]
                    [else (error 'htdp-langs.ss:symbol->printer-number
                                 "got: ~a as printing style"
                                 printing-setting)]))]
               [printer-number->symbol
                (lambda (which)
                  (case which
                    [(0) 'constructor-style]
                    [(1) 'quasi-style]
                    [(2) 'r4rs-style]
                    [else 'constructor-style]))]
               
               [printing-style-rb
                (right-align
                 (lambda (main)
                   (make-object radio-box%
                     (string-constant output-style-label)
                     (list (string-constant constructor-printing-style)
                           (string-constant quasiquote-printing-style)
                           (string-constant write-printing-style))
                     main
                     void))
                 output-syntax-panel)]
               
               
               [case-sensitive-cb 
                (make-check-box (string-constant case-sensitive-label) input-syntax-panel)]               
               [sharing-printing-cb
                (make-check-box (string-constant sharing-printing-label) output-syntax-panel)]
               [booleans-as-true/false-cb
                (make-check-box (string-constant booleans-as-true/false-label) output-syntax-panel)]
               [use-pretty-printer-cb
                (make-check-box (string-constant use-pretty-printer-label) output-syntax-panel)])
          
          (send printing-style-rb set-selection (symbol->printer-number (setting-printing-style settings)))
          (send case-sensitive-cb set-value (setting-case-sensitive settings))
          (send sharing-printing-cb set-value (setting-sharing-printing settings))
          (send booleans-as-true/false-cb set-value (setting-booleans-as-true/false settings))
          (send use-pretty-printer-cb set-value (setting-use-pretty-printer settings))
          
          (lambda ()
            (make-setting/parse 
             `((case-sensitive ,(send case-sensitive-cb get-value))
               (printing-style ,(printer-number->symbol (send printing-style-rb get-selection)))
               (sharing-printing ,(send sharing-printing-cb get-value))
               (booleans-as-true/false ,(send booleans-as-true/false-cb get-value))
               (use-pretty-printer ,(send use-pretty-printer-cb get-value)))))))
      
      
      ;; add-htdp-language : (implements htdp-language<%>) -> void
      (define (add-htdp-language class%)
        (drscheme:language:add-language
         (make-object drscheme:language-tower:module-based-language->language%
           (make-object htdp-language->module-based-language%
             (make-object class%)))))

      (add-htdp-language
       (class* object% (htdp-language<%>)
         (public get-module
                 get-language-position
                 sharing-printing
                 abbreviate-cons-as-list)
         (define (get-module) '(lib "beginner.ss" "langs"))
         (define (get-language-position) '("How to Design Programs" "Beginning Student"))
         (define (sharing-printing) #f)
         (define (abbreviate-cons-as-list) #f)
         (super-instantiate ())))
      
      (add-htdp-language
       (class* object% (htdp-language<%>) 
         (public get-module
                 get-language-position
                 sharing-printing
                 abbreviate-cons-as-list)
         (define (get-module) '(lib "intermediate.ss" "langs"))
         (define (get-language-position) '("How to Design Programs" "Intermediate Student"))
         (define (sharing-printing) #f)
         (define (abbreviate-cons-as-list) #t)
         (super-instantiate ())))
      
      (add-htdp-language
       (class* object% (htdp-language<%>) 
         (public get-module
                 get-language-position
                 sharing-printing
                 abbreviate-cons-as-list)
         (define (get-module) '(lib "advanced.ss" "langs"))
         (define (get-language-position) '("How to Design Programs" "Advanced Student"))
         (define (sharing-printing) #t)
         (define (abbreviate-cons-as-list) #t)
         (super-instantiate ()))))))
