(unit/sig stepper:settings^
  (import [z : zodiac:system^]
          mzlib:pretty-print^
          mred^
          [d : drscheme:export^]
          [p : mzlib:print-convert^]
          [e : stepper:error^]
          [bc : stepper:beginner-checker^]
          [a : stepper:annotate^]
          [r : stepper:reconstruct^]
          [f : framework^]
          stepper:shared^)
  
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
  
  (define (image? val)
   (is-a? val image-snip%))
  
  (define print-convert #f)
  
  (define (setup-print-convert settings)
    (let ([print-convert-space (make-eventspace)]
          [print-convert-result #f]
          [print-convert-semaphore (make-semaphore)])
      (parameterize ([current-eventspace print-convert-space])
        (queue-callback
         (lambda ()
           (d:basis:initialize-parameters (make-custodian) settings)
           (d:rep:invoke-library)
           (p:current-print-convert-hook 
               (lambda (v basic-convert sub-convert)
                 (if (image? v)
                     v
                     (basic-convert v)))))))
      (set! print-convert
            (lambda (val)
              (parameterize ([current-eventspace print-convert-space])
                (queue-callback
                 (lambda ()
                   (set! print-convert-result
                         (p:print-convert val))
                   (semaphore-post print-convert-semaphore))))
              (semaphore-wait print-convert-semaphore)
              print-convert-result))))

      
  
  (define stepper-frame%
    (d:frame:basics-mixin (f:frame:standard-menus-mixin f:frame:basic%)))

  (define stepper-canvas%
    (class editor-canvas% (parent (editor #f) (style null) (scrolls-per-page 100))
      (rename (super-on-size on-size))
      (inherit get-editor)
      (override 
        [on-size 
         (lambda (width height)
           (super-on-size width height)
           (let ([editor (get-editor)])
             (when editor
               (send editor reset-pretty-print-width this))))])
      (sequence (super-init parent editor style scrolls-per-page))))
  
  (define stepper-text%
    (class f:text:basic% (sexp redex break-kind error-msg (line-spacing 1.0) (tabstops null))
      (inherit find-snip insert change-style highlight-range last-position lock erase
               begin-edit-sequence end-edit-sequence get-start-position)
      (public (pretty-printed-width -1)
              (char-width 0)
              (clear-highlight-thunk (lambda () #f))
              (reset-pretty-print-width 
               (lambda (canvas)
                 (begin-edit-sequence)
                 (when (= (last-position) 0)
                   (insert "a")
                   (change-style result-delta 0 1))
                 (let* ([style (send (find-snip 0 'before) get-style)]
                        [_ (set! char-width (send style get-text-width (send canvas get-dc)))]
                        [canvas-width (let-values ([(client-width client-height)
                                                    (send canvas get-client-size)])
                                        (- client-width 18))] ; 12 border pixels + 6 for wrap char
                        [min-columns 30]
                        [new-columns (max min-columns 
                                          (floor (/ canvas-width char-width)))])
                   (pretty-print-columns new-columns)
                   (reformat-sexp)
                   (end-edit-sequence))))
              (reformat-sexp
               (lambda ()
                 (when (not (= pretty-printed-width (pretty-print-columns)))
                   (set! pretty-printed-width (pretty-print-columns))
                   (format-sexp))))
              (format-sexp
               (lambda ()
                 (lock #f)
                 (begin-edit-sequence)
                 (clear-highlight-thunk)
                 (erase)
                 (when (not (eq? sexp no-sexp))
                   (let ([result (open-output-string)]
                         [output (open-output-string)]
                         [real-print-hook (pretty-print-print-hook)]
                         [redex-begin #f]
                         [redex-end #f]
                         [placeholder-present? #f])
                     (parameterize ([current-output-port output]
                                    [pretty-print-size-hook
                                     (lambda (value display? port)
                                       (if (eq? value highlight-placeholder)
                                           (begin
                                             (set! placeholder-present? #t)
                                             (string-length (format "~s" redex)))
                                           (if (image? value)
                                               1   ; if there was a good way to calculate a image widths ...
                                               #f)))]
                                    [pretty-print-print-hook
                                     (lambda (value display? port)
                                       (if (eq? value highlight-placeholder)
                                           (insert (format "~s" redex))
                                           ; next occurs if value is an image:
                                           (insert (send value copy))))]
                                    [pretty-print-display-string-handler
                                     (lambda (string port)
                                       (insert string))]
                                    [pretty-print-print-line
                                     (lambda (number port old-length dest-columns)
                                       (when (not (eq? number 0))
                                         (insert #\newline))
                                       0)]
                                    [pretty-print-pre-print-hook
                                     (lambda (value p)
                                       (when (or (and (not placeholder-present?)
                                                      (eq? value redex))
                                                 (eq? value highlight-placeholder))
                                         (set! redex-begin (get-start-position))))]
                                    [pretty-print-post-print-hook
                                     (lambda (value p)
                                       (when (or (and (not placeholder-present?)
                                                      (eq? value redex))
                                                 (eq? value highlight-placeholder))
                                         (set! redex-end (get-start-position))))])
                       (for-each
                        (lambda (expr)
                          (pretty-print expr result)
                          (insert #\newline))
                        sexp))
                     (let ([between (last-position)]
                           [highlight-color (if (eq? break-kind 'result-break)
                                                result-highlight-color
                                                redex-highlight-color)])
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
                 (end-edit-sequence)
                 (lock #t))))
      (sequence (super-init line-spacing tabstops))))
  
  (define (read-n-parse text vocabulary handler)
    (let ([reader (z:read (f:gui-utils:read-snips/chars-from-buffer text)
                            (z:make-location 1 1 0 "stepper-text"))])
      (let read-n-parse-loop ()
        (let ([new-expr (begin
                          (d:interface:set-zodiac-phase 'reader)
                          (with-handlers 
                              ((exn:read? handler))
                            (reader)))])
          (if (z:eof? new-expr)
              (values null null)
              (let ([expanded (begin
                                (d:interface:set-zodiac-phase 'expander)
                                (with-handlers ((exn:syntax? handler))
                                  (z:scheme-expand new-expr 'previous vocabulary)))])
                (let-values ([(red-exprs parsed-exprs) (read-n-parse-loop)])
                  (values (cons new-expr red-exprs) (cons expanded parsed-exprs)))))))))

  (define output-delta (make-object style-delta% 'change-family 'modern))
  (define result-delta (make-object style-delta% 'change-family 'modern))
  (define error-delta (make-object style-delta% 'change-style 'italic))
  (send error-delta set-delta-foreground "RED")

  (define test-dc (make-object bitmap-dc% (make-object bitmap% 1 1)))
  (define result-highlight-color (make-object color% 255 255 255))
  (define redex-highlight-color (make-object color% 255 255 255))
  (send test-dc try-color (make-object color% 212 159 245) result-highlight-color)
  (send test-dc try-color (make-object color% 193 251 181) redex-highlight-color)
             
  (define no-sexp (gensym "no-sexp-"))
  
  (define (stepper-go frame settings)
    (letrec ([text (ivar frame definitions-text)]
             [stepper-semaphore (make-semaphore)]
             [expr-list #f]
             
             [history null]
             
             [store-step
              (lambda (break-kind reconstructed redex)
                (let ([step-text (make-object stepper-text% reconstructed redex break-kind #f)])
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
                (let ([e (list-ref history view)])
                  (send e reset-pretty-print-width canvas)
                  (send canvas set-editor e))
                (send previous-button enable (not (zero? view)))
                (send home-button enable (not (zero? view)))
                (send next-button enable (not (eq? final-view view))))]
                         
             [break 
              (lambda (mark-list all-defs current-def break-kind returned-value-list)
                (when (case break-kind
                        [(result-break)
                         (if (not (null? returned-value-list))
                             (not (r:skip-redex-step? mark-list))
                             (not (r:skip-result-step? mark-list)))]
                        [(normal)
                         (not (r:skip-redex-step? mark-list))]
                        [else 
                         (e:internal-error 'break "unknown break type: ~s~n" break-kind)])
                  (parameterize ([current-eventspace drscheme-eventspace])
                    (queue-callback (lambda () 
                                      (apply store-step
                                             break-kind
                                             (r:reconstruct expr-list 
                                                            mark-list
                                                            all-defs
                                                            current-def
                                                            break-kind
                                                            returned-value-list))
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
                       (let ([step-text (make-object stepper-text% no-sexp no-sexp #f (exn-message exn))])
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
                              [primitive-vector vector]
                              [exn-handler (make-exception-handler k)])
                          (d:basis:initialize-parameters (make-custodian) settings)
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
                          (let*-values ([(red-exprs parsed-exprs)
                                         (read-n-parse text (d:basis:current-vocabulary) exn-handler)]
                                        [(_) (with-handlers
                                                 ((exn:user? exn-handler))
                                               (bc:check-variable-duplication parsed-exprs par-global-defined-vars))]
                                        [(annotated exprs)
                                         (a:annotate red-exprs parsed-exprs break)])
                            (set! expr-list exprs)
                            (current-exception-handler exn-handler)
                            (for-each primitive-eval annotated)))))))))]
             
             [update-view/next-step
              (lambda (new-view)
                (set! view-currently-updating new-view)
                (semaphore-post stepper-semaphore))])
      
      
      (setup-print-convert settings)
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
                       (format (string-append "Language level is set to \"~a\".~n"
                                              "The Foot only works for the \"Beginner\" language level.~n")
                               (d:basis:setting-name settings))
                       #f 
                       '(ok))
          (stepper-go frame settings)))))
