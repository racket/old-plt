(unit/sig ()
  (import mzlib:pretty-print^
          mred^
          [d : drscheme:export^]
          [a : stepper:annotate^]
          [r : stepper:reconstruct^]
          [f : framework^])
  
  (define drscheme-eventspace (current-eventspace))
  
  (lambda (frame)
    (letrec ([edit (ivar frame definitions-edit)]
             [text (send edit get-text)]
             [stepper-semaphore (make-semaphore)]
             [expr-list #f]
             
             [history null]
             
             [highlight-color (make-object color% 200 200 200)]
             
             [store-step
              (lambda (reconstructed redex)
                (let ([result (open-output-string)]
                      [output (open-output-string)]
                      [real-print-hook (pretty-print-print-hook)]
                      [highlight-begin #f]
                      [highlight-end #f])
                  (parameterize ([current-output-port output]
                                 [pretty-print-print-hook
                                  (lambda (value . args)
                                    (error "getting here")
                                    (if (eq? value redex)
                                        (begin
                                          (set! highlight-begin (file-position result))
                                          (error 'store-step "found matching value")
                                          (apply real-print-hook value args)
                                          (set! highlight-end (file-position result)))
                                        (apply real-print-hook value args)))])
                    (for-each
                     (lambda (expr)
                       (pretty-print expr result)
                       (fprintf result "~n"))
                     reconstructed))
                  (error 'store-step "begin: ~a~nend: ~a" highlight-begin highlight-end)
                  (let ([outer-edit (make-object f:text:basic%)])
                    (send outer-edit insert (get-output-string result))
                    (send outer-edit insert #\newline)
                    (let ([between (send outer-edit last-position)])
                      (send outer-edit insert (get-output-string output))
                      (send outer-edit change-style result-delta 0 between)
                      (send outer-edit change-style output-delta between
                            (send outer-edit last-position))
                      (when (and highlight-begin highlight-end)
                        (send outer-edit highlight-range highlight-begin highlight-end highlight-color #f #f))
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
             
             [s-frame (make-object frame% "Stepper")]
             [output-delta (make-object style-delta% 'change-family 'modern)]
             [result-delta (make-object style-delta% 'change-family 'modern)]
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
             
             [break 
              (lambda (mark-list all-defs current-def)
                (when (r:stop-here? mark-list all-defs)
                  (parameterize ([current-eventspace drscheme-eventspace])
                    (queue-callback (lambda () 
                                      (apply store-step (r:reconstruct expr-list mark-list all-defs current-def))
                                      (when (r:final-mark-list? mark-list)
                                        (set! final-view view-currently-updating))
                                      (update-view view-currently-updating))))
                  (semaphore-wait stepper-semaphore)))]
             
             [user-eventspace (make-eventspace)]
           
             [stepper-start
              (lambda ()
                (let-values ([(annotated exprs)
                              (a:annotate text break)])
                  (set! expr-list exprs)
                  (parameterize ([current-eventspace user-eventspace])
                    (queue-callback 
                     (lambda ()
                       ((require-library "beginner.ss" "userspce"))
                       (for-each eval annotated))))))]
           
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
        (set! view-currently-updating 0)
        (stepper-start)
        (send button-panel stretchable-width #f)
        (send canvas min-width 500)
        (send canvas min-height 500)
        (send previous-button enable #f)
        (send home-button enable #f)
        (send s-frame show #t))))
