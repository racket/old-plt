(module main mzscheme
  (require (lib "class.ss")
           (lib "unitsig.ss")
           (lib "unit.ss")
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
  (define trace? #f)
  (define-syntax trace
    (syntax-rules ()
      ((_ str arg ...)
       (cond (trace? (fprintf orig-output str arg ...) (newline orig-output))))))
  
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)

      (define frame #f)
      
      (define-values/invoke-unit
       (new-file-window open-window)
       (unit
         (import)
         (export new-file-window open-window)
         (define counter 0)
         (define lock (make-semaphore 1))
         (define (new-file-window)
           (semaphore-wait lock)
           (set! counter (add1 counter))
           (semaphore-post lock))
         (define (open-window)
           (semaphore-wait lock)
           (cond
             ((> counter 0)
              (set! counter (sub1 counter))
              (semaphore-post lock)
              #t)
             (else
              (semaphore-post lock)
              #f)))))

      (drscheme:get/extend:extend-unit-frame
       (lambda (frame%)
         (class frame%
           (inherit get-button-panel get-interactions-text)
           (public get-script-unit is-active?)
           
           (trace "building-frame")
           
           (define code-engine@
             (unit/sig code-engine^
               (import script^)
               
               (trace "invoking code-engine")
               
               (set! script-unit (make-unit script^ (signature->symbols script^)))
                              
               (trace "code-engine through")
               
               (define (open-drscheme file)
                 (drscheme:unit:open-drscheme-window file))
               
               (define (get-user-value sym)
                 (parameterize ((current-namespace
                                 (send (get-interactions-text) get-user-namespace)))
                   (namespace-variable-value sym)))
               
               (define (user-eval-no-disable code callback)
                 (parameterize ((current-eventspace 
                                 (send (get-interactions-text) get-user-eventspace)))
                   (trace "in user-eval-no-disable")
                   (queue-callback
                    (lambda ()
                      (trace "user callback running: ~a" code)
                      (callback (eval code)))
                    #t)))
               
               (define (user-eval code callback)
                 (parameterize ((current-eventspace 
                                 (send (get-interactions-text) get-user-eventspace)))
                   (trace "in user-eval")
                   (queue-callback
                    (lambda ()
                      (trace "user callback running: ~a" code)
                      (disable-evaluation)
                      (with-handlers ((exn?
                                       (lambda (ex)
                                         (enable-evaluation)
                                         (raise ex))))
                        (callback (eval code))
                        (enable-evaluation)))
                    #t)))))
           
           (define script-unit #f)
           (define (get-script-unit) script-unit)
           
           (define active? (open-window))
           (define (is-active?) active?)

           (rename (super-enable-evaluation enable-evaluation))
           (define/override (enable-evaluation)
             (if active?
                 (send (car (send container get-children)) enable #t))
             (super-enable-evaluation))
           
           (rename (super-disable-evaluation disable-evaluation))
           (define/override (disable-evaluation)
             (if active?
                 (send (car (send container get-children)) enable #f))
             (super-disable-evaluation))
           
           (rename (super-execute-callback execute-callback))
           (define/override (execute-callback)
             (set! frame this)       ;; Relies on the fact that executes are atomic with respect
                                     ;; to other executes
             (super-execute-callback))
           
           (rename (super-make-root-area-container make-root-area-container))
           (define container #f)
           
           (define/override (make-root-area-container % parent)
             (set! container
                   (super-make-root-area-container panel:vertical-dragable% parent))
             (let ((root (make-object % container)))
               root))
           
           
           (define/override (file-menu:between-open-and-revert fm)
             (make-object menu-item% "Open in file browser..."
               fm
               (lambda (a b)
                 (let ((file (finder:get-file)))
                   (new-file-window)
                   (drscheme:unit:open-drscheme-window file)))))

           (define/override (file-menu:between-new-and-open fm)
             (make-object menu-item% "New file browser"
               fm
               (lambda (a b)
                 (new-file-window)
                 (drscheme:unit:open-drscheme-window))))
           
           (define start
             (cond
               (active?
                (invoke-unit/sig 
                 (compound-unit/sig
                   (import)
                   (link (FS : file-system^ ((make-file-system@ state) GUI))
                         (SCRIPT : script^ (script@ GUI FS CODE))
                         (CODE : code-engine^ (code-engine@ SCRIPT))
                         (GUI : gui^ ((make-gui@ state)
                                      SCRIPT CODE)))
                   (export))))))
           (set! frame this)

           (trace "super-frame start")
           (super-instantiate ())  ;; on-execute happens in here
           (trace "super-frame finish")           
           
           (cond
             (active?
              (send container begin-container-sequence)
              (trace "opening gui")
              (start container)
              (send container change-children
                    (lambda (c) (cons (cadr c) (cons (car c) null))))
              (send container end-container-sequence))))))
           
           
      
      (define (phase1)
        (drscheme:language:extend-language-interface
         drscheme:language:language<%>
         (lambda (%)
           (class %
             (rename (super-on-execute on-execute))
             (define/override (on-execute settings run-in-user-thread)
               (trace "entering on-execute")
	       (if (send frame is-active?)
		   (let ((module-name ((current-module-name-resolver) 
				       '(lib "script-param.ss" "file-browser")
				       'script-param #f))
			 (prog-namespace (current-namespace)))
		     (trace "starting on-execute user-thread")
		     (run-in-user-thread
		      (lambda ()
			(trace "in on-execute user-thread")
			(with-handlers ((void (lambda (x) (printf "~a~n" (exn-message x)))))
			  (namespace-attach-module prog-namespace module-name)
                          (script-unit-param (send frame get-script-unit))
                          (namespace-require `(lib "script.ss" "file-browser")))))
		     (trace "finish on-execute user-thread")))
               (trace "calling super-on-execute")
               (super-on-execute settings run-in-user-thread))
             (super-instantiate ())))))
      
      (define (phase2) (void)))))
