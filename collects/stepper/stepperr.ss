(unit/sig stepper:settings^
  (import mzlib:pretty-print^
          mred^
          [d : drscheme:export^]
          [p : mzlib:print-convert^]
          [a : stepper:annotate^]
          [r : stepper:reconstruct^]
          [f : framework^])
  
  (define drscheme-eventspace (current-eventspace))

  ;; exported parameters of the user's thread:
  
  (define par-namespace #f)
  (define (get-namespace) par-namespace)
  
  (define par-global-defined-vars #f)
  (define (get-global-defined-vars) par-global-defined-vars)
  
  (define par-constructor-style-printing #f)
  (define (get-constructor-style-printing) par-constructor-style-printing)
  
  (define par-abbreviate-cons-as-list #f)
  (define (get-abbreviate-cons-as-list) par-abbreviate-cons-as-list)
  
  (define par-empty-list-name #f)
  (define (get-empty-list-name) par-empty-list-name)
  
  (define par-show-sharing #f)
  (define (get-show-sharing) par-show-sharing)
  
  (define par-cons #f)
  (define (get-cons) par-cons)
  
  (define par-vector #f)
  (define (get-vector) par-vector)
  
  (define par-vocabulary #f)
  (define (get-vocabulary) par-vocabulary)
  
  (define stepper-frame%
    (d:frame:basics-mixin (f:frame:standard-menus-mixin f:frame:basic%)))

  (define stepper-canvas%
    (class editor-canvas% (parent (editor #f) (style null) (scrolls-per-page 100))
      (rename (super-on-size on-size))
      (inherit get-editor get-client-size show)
      (override 
        (on-size 
         (lambda (width height)
           (super-on-size width height)
           (reset-pretty-print-width))))
      (public (reset-pretty-print-width
               (lambda ()
                 (let ([editor (get-editor)])
                   (when editor
                     (let-values ([(client-width client-height) (get-client-size)])
                       (send editor reset-pretty-print-width client-width (lambda (x) (show x)))))))))
      (sequence (super-init parent editor style scrolls-per-page))))
  
  (define stepper-text%
    (class f:text:basic% (sexp redex error-msg (line-spacing 1.0) (tabstops null))
      (inherit get-dc find-snip insert change-style highlight-range last-position lock erase)
      (public (pretty-printed-width -1)
              (clear-highlight-thunk (lambda () #f))
              (reset-pretty-print-width 
               (lambda (width show-proc)
                 (when (= (last-position) 0)
                   (show-proc #f)
                   (insert "a")
                   (change-style result-delta 0 1))
                 (let* ([style (send (find-snip 0 'before) get-style)]
                        [char-width (send style get-text-width (get-dc))]
                        [min-columns 50]
                        [new-columns (max min-columns 
                                          (- (floor (/ width char-width)) 3))])
                   (pretty-print-columns new-columns)
                   (reformat-sexp show-proc))))
              (reformat-sexp
               (lambda (show-proc)
                 (when (not (= pretty-printed-width (pretty-print-columns)))
                   (set! pretty-printed-width (pretty-print-columns))
                   (show-proc #f)
                   (format-sexp)
                   (show-proc #t))))
              (format-sexp
               (lambda ()
                 (lock #f)
                 (clear-highlight-thunk)
                 (erase)
                 (when (not (eq? sexp no-sexp))
                   (let ([result (open-output-string)]
                         [output (open-output-string)]
                         [real-print-hook (pretty-print-print-hook)]
                         [redex-begin #f]
                         [redex-end #f])
                     (parameterize ([current-output-port output]
                                    [pretty-print-pre-print-hook
                                     (lambda (value p)
                                       (when (eq? value redex)
                                         (set! redex-begin (file-position result))))]
                                    [pretty-print-post-print-hook
                                     (lambda (value p)
                                       (when (eq? value redex)
                                         (set! redex-end (file-position result))))])
                       (for-each
                        (lambda (expr)
                          (pretty-print expr result)
                          (fprintf result "~n"))
                        sexp))
                     (insert (get-output-string result))
                     (insert #\newline)
                     (let ([between (last-position)])
                       (insert (get-output-string output))
                       (change-style result-delta 0 between)
                       (change-style output-delta between (last-position))
                       (when (and redex-begin redex-end)
                         (set! clear-highlight-thunk
                               (highlight-range redex-begin redex-end highlight-color #f #f))))))
                 (when error-msg
                   (let ([error-begin (last-position)])
                     (insert error-msg)
                     (change-style error-delta error-begin (last-position))))
                 (lock #t))))
      (sequence (super-init line-spacing tabstops))))

  (define output-delta (make-object style-delta% 'change-family 'modern))
  (define result-delta (make-object style-delta% 'change-family 'modern))
  (define error-delta (make-object style-delta% 'change-style 'italic))
  (send error-delta set-delta-foreground "RED")

  (define highlight-color (make-object color% 193 251 181))
             
  (define no-sexp (gensym "no-sexp-"))
  
  (define (stepper-go frame settings)
    (letrec ([edit (ivar frame definitions-text)]
             [text (send edit get-text)]
             [stepper-semaphore (make-semaphore)]
             [expr-list #f]
             
             [history null]
             
             [store-step
              (lambda (reconstructed redex)
                (let ([step-text (make-object stepper-text% reconstructed redex #f)])
                  (set! history (append history (list step-text)))))]
             
             [view-currently-updating #f]
             [final-view #f]
             [view 0]
             
             [home
              (lambda ()
                (update-view 0))]
             
             [next
              (lambda ()
                (if (= view (- (length history) 1))
                    (update-view/next-step (+ view 1))
                    (update-view (+ view 1))))]
             
             [previous
              (lambda ()
                (update-view (- view 1)))]
             
             [s-frame (make-object stepper-frame% "Stepper")]
             
             [button-panel (make-object horizontal-panel% (send s-frame get-area-container))]
             [home-button (make-object button% "Home" button-panel
                                       (lambda (_1 _2) (home)))]
             [previous-button (make-object button% "<< Previous" button-panel
                                           (lambda (_1 _2) (previous)))]      
             
             [next-button (make-object button% "Next >>" button-panel (lambda
                                                                          (_1 _2) (next)))]
             [canvas (make-object stepper-canvas% (send s-frame get-area-container))]
             
             [update-view
              (lambda (new-view)
                (set! view new-view)
                (send canvas set-editor (list-ref history view))
                (send canvas reset-pretty-print-width)
                (send previous-button enable (not (zero? view)))
                (send home-button enable (not (zero? view)))
                (send next-button enable (not (eq? final-view view))))]
                         
             [break 
              (lambda (mark-list all-defs current-def)
                (when (r:stop-here? mark-list)
                  (parameterize ([current-eventspace drscheme-eventspace])
                    (queue-callback (lambda () 
                                      (apply store-step (r:reconstruct expr-list mark-list all-defs current-def))
                                      (when (r:final-mark-list? mark-list)
                                        (set! final-view view-currently-updating))
                                      (update-view view-currently-updating))))
                  (semaphore-wait stepper-semaphore)))]
             
             [user-eventspace (make-eventspace)]
             
             [make-exception-handler
              (lambda (k)
                (lambda (exn)
                  (parameterize ([current-eventspace drscheme-eventspace])
                    (queue-callback
                     (lambda ()
                       (let ([step-text (make-object stepper-text% no-sexp no-sexp (exn-message exn))])
                         (set! history (append history (list step-text)))
                         (set! final-view view-currently-updating)
                         (update-view view-currently-updating)))))
                  (k)))]
             
             [stepper-start
              (lambda ()
                (parameterize ([current-eventspace user-eventspace])
                  (queue-callback 
                   (lambda ()
                     (call-with-current-continuation
                      (lambda (k)
                        (let ([primitive-eval (current-eval)]
                              [primitive-vector vector])
                          (d:basis:initialize-parameters (make-custodian) null settings)
                          (d:rep:invoke-library)
                          (set! par-namespace (current-namespace))
                          (set! par-global-defined-vars (map car (make-global-value-list)))
                          (set! par-constructor-style-printing (p:constructor-style-printing))
                          (set! par-abbreviate-cons-as-list (p:abbreviate-cons-as-list)) 
                          (set! par-empty-list-name (p:empty-list-name))
                          (set! par-show-sharing (p:show-sharing))
                          (set! par-cons (global-defined-value 'cons))
                          (set! par-vector (global-defined-value 'vector))
                          (set! par-vocabulary (d:basis:current-vocabulary))
                          (let-values ([(annotated exprs)
                                        (a:annotate text break (make-exception-handler k))])
                              (set! expr-list exprs)
                            (current-exception-handler
                             (make-exception-handler k))
;                            (for-each (lambda (expr)
;                                        (queue-callback
;                                         (lambda ()
;                                           (eval expr))))
;                                      annotated)
                            (for-each primitive-eval annotated)))))))))]
             
             [update-view/next-step
              (lambda (new-view)
                (set! view-currently-updating new-view)
                (semaphore-post stepper-semaphore))])
      
      
      
      (set! view-currently-updating 0)
      (stepper-start)
      (send button-panel stretchable-width #f)
      (send button-panel stretchable-height #f)
      (send canvas stretchable-height #t)
      (send canvas min-width 500)
      (send canvas min-height 500)
      (send previous-button enable #f)
      (send home-button enable #f)
      (send (send s-frame edit-menu:get-undo-item) enable #f)
      (send (send s-frame edit-menu:get-redo-item) enable #f)
      (send s-frame show #t)))
  
  (lambda (frame)
    (let ([settings (f:preferences:get 'drscheme:settings)])
      (if (not (string=? (d:basis:setting-name settings) "Beginner"))
          (message-box "Stepper" 
                       (format "Language level is set to \"~a\".~nPlease set the language level to \"Beginner\"" 
                               (d:basis:setting-name settings))
                       #f 
                       '(ok))
          (stepper-go frame settings)))))
