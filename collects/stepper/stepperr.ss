(unit/sig stepper:settings^
  (import [z : zodiac:system^]
          mzlib:pretty-print^
          mred^
          [d : drscheme:export^]
          [p : mzlib:print-convert^]
          [e : stepper:error^]
          [gui : stepper:gui^]
          [a : stepper:annotate^]
          [r : stepper:reconstruct^]
          [f : framework^]
          stepper:shared^)
  
  (define beginner-level-name "Beginning Student")
  
  (define (send-to-other-eventspace eventspace thunk)
    (parameterize ([current-eventspace eventspace])
      (queue-callback thunk)))
       

  (define drscheme-eventspace (current-eventspace))

  (define (send-to-drscheme-eventspace thunk)
    (send-to-other-eventspace drscheme-eventspace thunk))
  
  (define par-constructor-style-printing #f)
  (define (constructor-style-printing?)
    par-constructor-style-printing)
  
  (define par-abbreviate-cons-as-list #f)
  (define (abbreviate-cons-as-list?)
    par-abbreviate-cons-as-list)
  
  (define par-cons #f)
  (define (user-cons? val)
    (eq? val par-cons))
  
  (define par-vector #f)
  (define (user-vector? val)
    (eq? val par-vector))
  
  (define user-pre-defined-vars #f)
  
  (define (check-pre-defined-var identifier)
    (memq identifier user-pre-defined-vars))
  
  (define user-namespace #f)

  (define (check-global-defined identifier)
    (with-handlers
        ([exn:variable? (lambda args #f)])
      (global-lookup identifier)
      #t))
  
  (define (global-lookup identifier)
    (parameterize ([current-namespace user-namespace])
      (global-defined-value identifier)))
    
  (define (image? val)
   (is-a? val snip%))
  
  (define print-convert #f)
  
  (define (setup-print-convert settings)
    (let ([print-convert-space (make-eventspace)]
          [print-convert-result #f]
          [print-convert-semaphore (make-semaphore)])
      (send-to-other-eventspace
       print-convert-space
       (lambda ()
         (d:basis:initialize-parameters (make-custodian) settings)
         (d:rep:invoke-library)
         (current-namespace user-namespace)
         (p:current-print-convert-hook 
          (lambda (v basic-convert sub-convert)
            (if (image? v)
                v
                (basic-convert v))))))
      (set! print-convert
            (lambda (val)
              (send-to-other-eventspace
               print-convert-space
               (lambda ()
                 (set! print-convert-result
                       (p:print-convert val))
                 (semaphore-post print-convert-semaphore)))
              (semaphore-wait print-convert-semaphore)
              print-convert-result))))

  (define finished-exprs #f)

  (define (stepper-go text settings)
    (local
        (; PUT THIS WHERE IT BELONGS: (define text (ivar drscheme-frame definitions-text))
         (define stepper-semaphore (make-semaphore))
         (define history null)
         
         (define current-expr #f)
         (define packaged-envs a:initial-env-package)
         
         (define user-eventspace (make-eventspace))
         
         (define (send-to-user-eventspace thunk)
           (send-to-other-eventspace user-eventspace thunk))
         
         (define user-primitive-eval #f)
         (define user-vocabulary #f)
         
         (define _1 ; install settings & library:
           (begin
             (send-to-user-eventspace 
              (lambda ()
                (set! user-primitive-eval (current-eval))
                (d:basis:initialize-parameters (make-custodian) settings)
                (d:rep:invoke-library)
                (set! user-namespace (current-namespace))
                (set! user-pre-defined-vars (map car (make-global-value-list)))
                (set! user-vocabulary (d:basis:current-vocabulary))
                (set! par-constructor-style-printing (p:constructor-style-printing))
                (set! par-abbreviate-cons-as-list (p:abbreviate-cons-as-list))
                (set! par-cons (global-defined-value 'cons))
                (set! par-vector (global-defined-value 'vector))
                (semaphore-post stepper-semaphore)))
             (semaphore-wait stepper-semaphore)))
         
         (define begin-next-expr
           (let* ([reader (z:read (f:gui-utils:read-snips/chars-from-buffer text)
                                  (z:make-location 1 1 0 "stepper-text"))]
                  [zodiac-eventspace (make-eventspace)]
                  [send-to-zodiac-eventspace 
                   (lambda (thunk)
                     (send-to-other-eventspace zodiac-eventspace thunk))]
                  [zodiac-semaphore (make-semaphore)])
             
             ;; ALL OF THIS ZODIAC-EVENTSPACE STUFF HAS TO GO--- WHAT ABOUT LANGUAGE LEVELS
             ;; WHICH CAN DEFINE MACROS? THEY WON'T WORK UNDER THIS MODEL.
             
             (send-to-zodiac-eventspace
              (lambda ()
                (d:basis:initialize-parameters (make-custodian) settings)
                (d:rep:invoke-library)
                (semaphore-post zodiac-semaphore)))
             (semaphore-wait zodiac-semaphore)
             (lambda ()
               (send-to-zodiac-eventspace
                (lambda ()
                  (let/ec k
                    (let ([inner-handler
                           (lambda (exn)
                             (send-to-drscheme-eventspace
                              (lambda ()
                                (handle-exception exn)))
                             (k))]
                          [return-handler
                           (lambda (read parsed)
                             (send-to-drscheme-eventspace
                              (lambda ()
                                (continue-next-expr read parsed))))])
                      (d:interface:set-zodiac-phase 'reader)
                      (let* ([new-expr (with-handlers
                                           ((exn:read? inner-handler))
                                         (reader))])
                        (return-handler new-expr
                                        (if (z:eof? new-expr)
                                            #f
                                            (begin
                                              (d:interface:set-zodiac-phase 'expander)
                                              (with-handlers
                                                  ((exn:syntax? inner-handler))
                                                (z:scheme-expand new-expr 'previous user-vocabulary)))))))))))))

         
         (define (continue-next-expr read parsed)
           (let/ec k
             (let ([exn-handler (make-exception-handler k)])
               (if (z:eof? read)
                   (construct-final-step)
                   (let*-values ([(annotated-list envs) (a:annotate (list read) (list parsed) packaged-envs break)]
                                 [(annotated) (car annotated-list)])
                     (set! packaged-envs envs)
                     (set! current-expr parsed)
                     (check-for-repeated-names parsed exn-handler)
                     (send-to-user-eventspace
                      (lambda ()
                        (let/ec k
                          (current-exception-handler (make-exception-handler k))
                          (user-primitive-eval annotated)
                          (send-to-drscheme-eventspace
                           (lambda ()
                             (add-finished-expr)
                             (begin-next-expr)))))))))))
         
         
         (define (check-for-repeated-names expr exn-handler)
           (with-handlers
               ((exn:user? exn-handler))
             (when (z:define-values-form? expr)
               (for-each (lambda (name) 
                           (when (check-global-defined name)
                             (error 'check-for-repeated-names
                                    "name is already bound: ~s" name)))
                         (map z:varref-var (z:define-values-form-vars expr))))))
         
         (define (add-finished-expr)
           (let ([reconstructed (r:reconstruct-completed current-expr)])
             (set! finished-exprs (append finished-exprs (list reconstructed)))))
         
         (define view-currently-updating #f)
         (define final-view #f)
         
         (define view 0)
             
         (define (home)
           (update-view 0))
         
         (define (next)
           (gui:prev-enable #f)
           (gui:next-enable #f)
           (gui:home-enable #f)
           (if (= view (- (length history) 1))
               (update-view/next-step (+ view 1))
               (update-view (+ view 1))))
         
         (define (previous)
           (update-view (- view 1)))
             
         (define s-frame (make-object stepper-frame% drscheme-frame))
             
         (define button-panel (make-object horizontal-panel% (send s-frame get-area-container)))
         (define home-button (make-object button% "Home" button-panel
                                          (lambda (_1 _2) (home))))
         (define previous-button (make-object button% "<< Previous" button-panel
                                              (lambda (_1 _2) (previous))))
         (define next-button (make-object button% "Next >>" button-panel (lambda
                                                                             (_1 _2) (next))))
         (define canvas (make-object stepper-canvas% (send s-frame get-area-container)))

         (define (update-view new-view)
           (set! view new-view)
           (let ([e (list-ref history view)])
             (send e reset-pretty-print-width canvas)
             (send canvas set-editor e))
           (send previous-button enable (not (zero? view)))
           (send home-button enable (not (zero? view)))
           (send next-button enable (not (eq? final-view view))))
         
         (define held-expr no-sexp)
         (define held-redex no-sexp)

         (define (break mark-list break-kind returned-value-list)
           (let ([reconstruct-helper
                  (lambda (finish-thunk)
                    (parameterize ([current-eventspace drscheme-eventspace])
                      (queue-callback
                       (lambda ()
                         (let* ([reconstruct-pair
                                 (r:reconstruct-current current-expr 
                                                        mark-list
                                                        break-kind
                                                        returned-value-list)]
                                [reconstructed (car reconstruct-pair)]
                                [redex (cadr reconstruct-pair)])
                           (finish-thunk reconstructed redex))))))])                    
             (case break-kind
               [(normal) 
                (when (not (r:skip-redex-step? mark-list))
                  (reconstruct-helper 
                   (lambda (reconstructed redex)
                     (set! held-expr reconstructed)
                     (set! held-redex redex)
                     (semaphore-post stepper-semaphore)))
                  (semaphore-wait stepper-semaphore))]
               [(result-break)
                (when (if (not (null? returned-value-list))
                          (not (r:skip-redex-step? mark-list))
                          (and (not (eq? held-expr no-sexp))
                               (not (r:skip-result-step? mark-list))))
                  (reconstruct-helper 
                   (lambda (reconstructed redex)
                     (let ([step-text (make-object stepper-text% 
                                                   held-expr
                                                   held-redex
                                                   reconstructed
                                                   redex
                                                   break-kind
                                                   #f)])
                       (set! held-expr no-sexp)
                       (set! held-redex no-sexp)
                       (set! history (append history (list step-text)))
                       (update-view view-currently-updating))))
                  (semaphore-wait stepper-semaphore))])))

         (define (construct-final-step)
           (let ([step-text (make-object stepper-text% no-sexp no-sexp no-sexp no-sexp #f #f)])
             (set! history (append history (list step-text)))
             (set! final-view view-currently-updating)
             (update-view view-currently-updating)))

         (define (handle-exception exn)
           (let ([step-text (if held-expr
                                (make-object stepper-text% held-expr held-redex no-sexp no-sexp #f (exn-message exn))
                                (make-object stepper-text% no-sexp no-sexp no-sexp no-sexp #f (exn-message exn)))])
             (set! history (append history (list step-text)))
             (set! final-view view-currently-updating)
             (update-view view-currently-updating)))
         
         
         (define (make-exception-handler k)
           (lambda (exn)
             (parameterize ([current-eventspace drscheme-eventspace])
               (queue-callback
                (lambda ()
                  (handle-exception exn))))
             (k)))
             
             
         (define (update-view/next-step new-view)
           (set! view-currently-updating new-view)
           (semaphore-post stepper-semaphore)))
      
      (send drscheme-frame stepper-frame s-frame)
      (setup-print-convert settings)
      (set! finished-exprs null)
      (set! view-currently-updating 0)
      (begin-next-expr)
      (send button-panel stretchable-width #f)
      (send button-panel stretchable-height #f)
      (send canvas stretchable-height #t)
      (send canvas min-width 500)
      (send canvas min-height 500)
      (send previous-button enable #f)
      (send home-button enable #f)
      (send next-button enable #f)
      (send (send s-frame edit-menu:get-undo-item) enable #f)
      (send (send s-frame edit-menu:get-redo-item) enable #f)
      (send s-frame show #t)))
  
  (lambda (frame)
    (let ([settings (f:preferences:get 'drscheme:settings)])
      (if (not (string=? (d:basis:setting-name settings) beginner-level-name))
          (message-box "Stepper" 
                       (format (string-append "Language level is set to \"~a\".~n"
                                              "The Foot only works for the \"~a\" language level.~n")
                               (d:basis:setting-name settings)
                               beginner-level-name)
                       #f 
                       '(ok))
          (stepper-go frame settings)))))
