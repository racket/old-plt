(unit/sig ()
  (import mzlib:pretty-print^
          mred^
          [d : drscheme:export^]
          [a : stepper:annotate^]
          [r : stepper:reconstruct^]
          [f : framework^])
  
  (define drscheme-eventspace (current-eventspace))
  
  (define stepper-canvas%
    (class editor-canvas% (parent editor style scrolls-per-page)
      ;; PROVIDE DEFAULTS FOR THESE VARIABLES
      
      
      
  (lambda (frame)
    (letrec ([edit (ivar frame definitions-text)]
             [text (send edit get-text)]
             [stepper-semaphore (make-semaphore)]
             [expr-list #f]
             
             [history null]
             
             [highlight-color (make-object color% 130 200 130)]
             
             [store-step
              (lambda (reconstructed redex)
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
                     reconstructed))
                  (let ([outer-edit (make-object f:text:basic%)])
                    (send outer-edit insert (get-output-string result))
                    (send outer-edit insert #\newline)
                    (let ([between (send outer-edit last-position)])
                      (send outer-edit insert (get-output-string output))
                      (send outer-edit change-style result-delta 0 between)
                      (send outer-edit change-style output-delta between
                            (send outer-edit last-position))
                      (when (and redex-begin redex-end)
;                        (send outer-edit change-style redex-delta redex-begin redex-end))
                        (send outer-edit highlight-range redex-begin redex-end highlight-color #f #f))
                      (set! history (append history (list outer-edit)))))))]
             
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
             
             [s-frame (make-object f:frame:basic% "Stepper")]
             [output-delta (make-object style-delta% 'change-family 'modern)]
             [result-delta (make-object style-delta% 'change-family 'modern)]
;             [redex-delta (make-object style-delta%)] 
             [error-delta (make-object style-delta%)]
                                       
             [button-panel (make-object horizontal-panel% s-frame)]
             [home-button (make-object button% "Home" button-panel
                                       (lambda (_1 _2) (home)))]
             [previous-button (make-object button% "<< Previous" button-panel
                                           (lambda (_1 _2) (previous)))]      
             
             [next-button (make-object button% "Next >>" button-panel (lambda
                                                                          (_1 _2) (next)))]
             [canvas (make-object editor-canvas% s-frame)]
             
             [update-view
              (lambda (new-view)
                (set! view new-view)
                (send canvas set-editor (list-ref history view))
                (send previous-button enable (not (zero? view)))
                (send home-button enable (not (zero? view)))
                (send next-button enable (not (eq? final-view view))))]
             
             [global-defined-vars #f]
             
             [break 
              (lambda (mark-list all-defs current-def)
                (when (r:stop-here? mark-list all-defs global-defined-vars)
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
                       (let ([new-text (make-object f:text:basic%)])
                         (when (> view-currently-updating 0)
                           (send (list-ref history (- view-currently-updating 1))
                                 copy-self-to
                                 new-text))
                         (let ([error-begin (send new-text last-position)])
                           (send new-text insert (exn-message exn))
                           (send new-text change-style error-delta error-begin (send new-text last-position))
                           (set! history (append history (list new-text)))
                           (set! final-view view-currently-updating)
                           (update-view view-currently-updating))))))
                  (k)))]
             
             [stepper-start
              (lambda ()
                (call-with-current-continuation
                 (lambda (k)
                   (let-values ([(annotated exprs)
                                 (parameterize ([current-exception-handler
                                                 (make-exception-handler k)])
                                   (a:annotate text break))])
                     (set! expr-list exprs)
                     (parameterize ([current-eventspace user-eventspace])
                       (queue-callback 
                        (lambda ()
                          ((require-library "beginner.ss" "userspce"))
                          (set! global-defined-vars
                                (map car (make-global-value-list)))
                          (call-with-current-continuation
                           (lambda (k)
                             (current-exception-handler
                              (make-exception-handler k))
                             (for-each eval annotated))))))))))]
           
;                          (d:basis:initialize-parameters
;                           (current-custodian)
;                           null
;                           (d:basis:find-setting-named "Beginner"))
           
             [update-view/next-step
              (lambda (new-view)
                (set! view-currently-updating new-view)
                (semaphore-post stepper-semaphore))])
             
             

      (send result-delta set-delta-foreground "BLACK")
      (send output-delta set-delta-foreground "PURPLE")
;      (send redex-delta set-delta-foreground "BLUE")
      (send error-delta set-delta-foreground "RED")
      (set! view-currently-updating 0)
      (stepper-start)
      (send button-panel stretchable-width #f)
      (send button-panel stretchable-height #f)
      (send canvas stretchable-height #t)
      (send canvas min-width 500)
      (send canvas min-height 500)
      (send previous-button enable #f)
      (send home-button enable #f)
      (send s-frame show #t))))
