(module debugger-tool mzscheme
  (require (lib "contracts.ss")
           (lib "tool.ss" "drscheme")
           (lib "mred.ss" "mred")  
           (prefix frame: (lib "framework.ss" "framework"))
           (lib "unitsig.ss")
           (lib "class.ss")
           (lib "etc.ss")
           (lib "list.ss")
           (prefix model: "private/debugger-model.ss")
	   "private/my-macros.ss"
           "private/shared.ss"
           (prefix x: "private/mred-extensions.ss")
           (lib "string-constant.ss" "string-constants"))

  (provide tool@)
  
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)

      (define (phase1) (void))
      (define (phase2) (void))
      
      (define debugger-initial-width 500)
      (define debugger-initial-height 500)
      
      (define image? x:image?)
  
      (define debugger-bitmap
        (drscheme:unit:make-bitmap
         "Debug"
         (build-path (collection-path "icons") "foot.bmp")))

      (define debugger-unit-frame<%>
        (interface ()
          on-debugger-close))
      
      (define (debugger-unit-frame-mixin super%)
        (class* super% (debugger-unit-frame<%>)
          
          (inherit get-button-panel get-interactions-text get-definitions-text)
          (rename [super-on-close on-close])
          
          (define debugger-exists #f)
          (define/public (on-debugger-close)
            (set! debugger-exists #f))
          
          (super-instantiate ())
          
          (define program-expander
            (contract
             (-> (-> void?) ; init
                 (-> (union eof-object? syntax? (cons/p string? any?)) (-> void?) void?) ; iter
                 void?)
             (lambda (init iter)
               (let* ([lang-settings 
                       (frame:preferences:get
                        (drscheme:language-configuration:get-settings-preferences-symbol))]
                      [lang (drscheme:language-configuration:language-settings-language lang-settings)]
                      [settings (drscheme:language-configuration:language-settings-settings lang-settings)])
                 (drscheme:eval:expand-program
                  (drscheme:language:make-text/pos (get-definitions-text) 
                                                   0
                                                   (send (get-definitions-text)
                                                         last-position)) 
                  lang-settings
                  #f
                  (lambda ()
                    (init)
                    (error-value->string-handler
                     (lambda (val len)
                       (let ([sp (open-output-string)])
                         (send lang render-value val settings sp #f)
                         (let ([str (get-output-string sp)])
                           (if ((string-length str) . <= . len)
                               str
                               (string-append (substring str 0 (max 0 (- len 3))) "..."))))))
                    (drscheme:teachpack:install-teachpacks 
                     (frame:preferences:get 'drscheme:teachpacks))) ; this belongs in model, but I'd need a unit rewrite
                  void ; kill
                  iter)))
             'program-expander
             'caller))
          
          (define debugger-button 
            (make-object button%
              (debugger-bitmap this)
              (get-button-panel)
              (lambda (button evt)
                (if debugger-exists
                    (message-box/custom "Debugger Exists"
                                        "There is already a debugger window open for this program."
                                        "OK"
                                        #f
                                        #f
                                        #f
                                        '(default=1))
                    (begin
                      (set! debugger-exists #t)
                      (model:go program-expander))))))
          
          (rename [super-enable-evaluation enable-evaluation])
          (define/override (enable-evaluation)
            (send debugger-button enable #t)
            (super-enable-evaluation))
          
          (rename [super-disable-evaluation disable-evaluation])
          (define/override (disable-evaluation)
            (send debugger-button enable #f)
            (super-disable-evaluation))
          
          (send (get-button-panel) change-children
                (lx (cons debugger-button (remq debugger-button _))))))
      
      (drscheme:get/extend:extend-unit-frame debugger-unit-frame-mixin))))
