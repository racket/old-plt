(module main mzscheme
  (require (lib "class.ss")
           (lib "unitsig.ss")
           (lib "list.ss")
           (lib "tool.ss" "drscheme")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           "sigs.ss"
           "script-param.ss"
           "gui-unit.ss"
           "script-unit.ss"
           "file-system.ss")
  
  (provide tool@)
  
  (define state (make-object file-system-state%))

  (define-syntax (make-unit stx)
    (syntax-case stx ()
      ((_ script^ names-mac)
       (let ((names 
              (vector->list 
               (syntax-object->datum 
                (cadr 
                 (syntax->list (local-expand (syntax names-mac) 
                                             'expression 
                                             null)))))))
         (with-syntax (((f-int  ...) (map (lambda (x) (gensym)) names))
                       ((name ...) (datum->syntax-object stx names)))
           (syntax
            (unit/sig script^
              (import)
              (rename (f-int name) ...)
              (define f-int name) ...)))))))

  
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      
      (define active? #f)
      
      (define user-namespace #f)
      (define user-thread #f)
      
      (define activate-bitmap
        (drscheme:unit:make-bitmap "Browse Files"
                                   (build-path (collection-path "icons") "file.gif")))
      
      (define code-engine@
        (unit/sig code-engine^
          (import script^)
          
          (script-unit-param
           (make-unit script^ (signature->symbols script^)))
          
          (setup-namespace user-thread)
          
          (define (get-user-value sym)
            (parameterize ((current-namespace user-namespace))
              (namespace-variable-value sym)))
          
          (define (user-thread-eval code-string callback)
            (user-thread
             (lambda ()
               (callback (eval (read (open-input-string code-string)))))))))
        
      (drscheme:get/extend:extend-unit-frame
       (lambda (frame%)
         (class frame%
           (inherit get-button-panel get-area-container)
           
           (rename (super-enable-evaluation enable-evaluation))
           (define/override (enable-evaluation)
             (send button enable #t)
             (super-enable-evaluation))
           
           (rename (super-disable-evaluation disable-evaluation))
           (define/override (disable-evaluation)
             (send button enable #f)
             (super-disable-evaluation))

           (rename (super-make-root-area-container make-root-area-container))
           (define browser-panel 100)
           
           (define/public (set-bp x)
             (set! browser-panel x))
           
           (define/override (make-root-area-container % parent)
             (set! browser-panel
                   (super-make-root-area-container panel:vertical-dragable% parent))
             (let ((root (make-object % browser-panel)))
               root))

           (super-instantiate ())
           
           (define button
             (make-object button% (activate-bitmap this) (get-button-panel)
               (lambda (a b)
                 (cond
                   (active? (void))
                   (else
                    (set! active? #t)
                    (let ((container browser-panel))
                      (send container begin-container-sequence)
                      (invoke-unit/sig 
                       (compound-unit/sig
                         (import)
                         (link (FS : file-system^ ((make-file-system@ state)))
                               (SCRIPT : script^ (script@ GUI FS))
                               (CODE : code-engine^ (code-engine@ SCRIPT))
                               (GUI : gui^ ((make-gui@ state container)
                                            SCRIPT CODE)))
                         (export)))
                      (send container change-children
                            (lambda (c) (cons (cadr c) (cons (car c) null))))
                      (send container end-container-sequence)))))))
           
           (send (get-button-panel) change-children
                 (lambda (x) (cons button (remq button x)))))))
      
      
      (define (setup-namespace run-thread)
        (let ((module-name ((current-module-name-resolver) 
                            '(lib "script-param.ss" "file-browser")
			    'script-param #f))
              (prog-namespace (current-namespace))
              (script (script-unit-param)))
          (run-thread
           (lambda ()
	     (with-handlers ((void (lambda (x) (printf "~a~n" (exn-message x)))))
               (script-unit-param script)
	       (namespace-attach-module prog-namespace module-name)
	       (namespace-require `(lib "script.ss" "file-browser")))
             (set! user-namespace (current-namespace))))))
      
      (define (phase1)
        (drscheme:language:extend-language-interface
         drscheme:language:language<%>
         (lambda (%)
           (class %
             (super-instantiate ())
             (rename (super-on-execute on-execute))
             (define/override (on-execute settings run-in-user-thread)
               (super-on-execute settings run-in-user-thread)
               (set! user-thread run-in-user-thread)
               (if active?
                   (setup-namespace run-in-user-thread)))))))
      
      (define (phase2) (void)))))
