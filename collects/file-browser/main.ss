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
  
  (define orig-output (current-output-port))
  (define trace? #t)
  (define-syntax trace
    (syntax-rules ()
      ((_ str arg ...)
       (cond (trace? (fprintf orig-output str arg ...) (newline orig-output))))))
  
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      
      (define activate-bitmap
        (drscheme:unit:make-bitmap "Browse Files"
                                   (build-path (collection-path "icons") "file.gif")))
      

      (define active? (make-parameter #f))
      
      (drscheme:get/extend:extend-unit-frame
       (lambda (frame%)
         (class frame%
           (inherit get-button-panel get-area-container get-execute-button get-interactions-text)

           (define code-engine@
             (unit/sig code-engine^
               (import script^)
               
               (trace "invoking code-engine")
               
               (script-unit-param
                (make-unit script^ (signature->symbols script^)))
               
               (setup-namespace (lambda (t)
                                  (send (get-interactions-text) run-in-evaluation-thread t))
                                (script-unit-param))
               
               (trace "code-engine through")
               
               (define (get-user-value sym)
                 (parameterize ((current-namespace
                                 (send (get-interactions-text) get-user-namespace)))
                   (namespace-variable-value sym)))
               
               (define (user-thread-eval code-string callback)
                 (send (get-interactions-text) run-in-evaluation-thread
                       (lambda ()
                         (callback (eval (read (open-input-string code-string)))))))))
           
           (rename (super-enable-evaluation enable-evaluation))
           (define/override (enable-evaluation)
             (send button enable #t)
             (super-enable-evaluation))
           
           (rename (super-disable-evaluation disable-evaluation))
           (define/override (disable-evaluation)
             (send button enable #f)
             (super-disable-evaluation))
           
           (rename (super-make-root-area-container make-root-area-container))
           (define browser-panel #f)
           
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
                   ((active?)
                    (active? #f)
                    (script-unit-param null)
                    (send browser-panel delete-child (car (send browser-panel get-children)))
                    (send (get-execute-button) command b))
                   (else
                    (active? #t)
                    (send (get-execute-button) command b)
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
      
      
      (define (setup-namespace run-thread unit)
        (trace "entering setup-namespace")
        (if (not (null? unit))
            (let ((s (make-semaphore)))
              (trace "starting setup-namespace user-thread")
              (run-thread
               (lambda () 
                 (trace "in setup-namespace user-thread")
                 (with-handlers ((void (lambda (x) (printf "~a~n" (exn-message x)))))
                   (script-unit-param unit)
                   (namespace-require `(lib "script.ss" "file-browser"))
                   (load (build-path (find-system-path 'pref-dir) ".file-browser.ss"))
                   (trace "no exception in setup-namespace user-thread"))
                 (semaphore-post s)
                 (trace "semaphore posted")))
              (yield s)
              (trace "finished setup-namespace user-thread")))
        (trace "leaving setup-namespace"))
      
      (define (phase1)
        (drscheme:language:extend-language-interface
         drscheme:language:language<%>
         (lambda (%)
           (class %
             (rename (super-on-execute on-execute))
             (define/override (on-execute settings run-in-user-thread)
               (trace "entering on-execute")
               (if (active?)
                   (let ((module-name ((current-module-name-resolver) 
                                       '(lib "script-param.ss" "file-browser")
                                       'script-param #f))
                         (prog-namespace (current-namespace))
                         (s (make-semaphore)))
                     (trace "starting on-execute user-thread")
                     (run-in-user-thread
                      (lambda ()
                        (trace "in on-execute user-thread")
                        (with-handlers ((void (lambda (x) (printf "~a~n" (exn-message x)))))
                          (namespace-attach-module prog-namespace module-name))
                        (semaphore-post s)))
                     (yield s)
                     (trace "finish on-execute user-thread")
                     (setup-namespace run-in-user-thread (script-unit-param))))
               (trace "calling super-on-execute")
               (super-on-execute settings run-in-user-thread))
             (super-instantiate ())))))
      
      (define (phase2) (void)))))
