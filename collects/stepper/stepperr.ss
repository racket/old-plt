(unit/sig ()
  (import mzlib:pretty-print^
          mred^
          [d : drscheme:export^]
          [a : stepper:annotate^]
          [r : stepper:reconstruct^])
  
  
  (lambda (frame)
    (let* ([edit (ivar frame definitions-edit)]
           [text (send edit get-text)]
           [break-sema (make-semaphore)]
           [resume-sema (make-semaphore)]
           [break-info #f]
           [expr-list #f]
           [finished-stepping #f]
           [break 
            (lambda (mark-list all-defs current-def)
              (when (r:stop-here? mark-list all-defs)
                (when (r:final-mark-list? mark-list)
                  (set! finished-stepping #t))
                (set! break-info (list mark-list all-defs current-def))
                (semaphore-post break-sema)
                (semaphore-wait resume-sema)))]
           [stepper-fetch-step-info
            (lambda () 
              (semaphore-wait break-sema)
              (apply r:reconstruct expr-list break-info))]
           [stepper-start
            (lambda (text)
              (let-values ([(annotated exprs)
                            (a:annotate text break)])
                (set! expr-list exprs)
                (set! break-sema (make-semaphore))
                (set! resume-sema (make-semaphore))
                (thread (lambda ()
;                          (d:basis:initialize-parameters
;                           (current-custodian)
;                           null
;                           (d:basis:find-setting-named "Beginner"))
                          ((require-library "beginner.ss" "userspce"))
                          (for-each eval annotated)
                          (semaphore-post break-sema)))
                (stepper-fetch-step-info)))]
           [stepper-step
            (lambda ()
              (semaphore-post resume-sema)
              (stepper-fetch-step-info))])
      (letrec ([step-thunk (lambda ()
                             (set! step-thunk stepper-step)
                             (stepper-start text))]
               [frame (make-object frame% "Stepper")]
               [output-delta (make-object style-delta% 'change-family 'modern)]
               [result-delta (make-object style-delta% 'change-family 'modern)]
               [button-panel (make-object horizontal-panel% frame)]
               [home-button (make-object button% "Home" button-panel
                                         (lambda (_1 _2) (home)))]
               [previous-button (make-object button% "<< Previous" button-panel
                                             (lambda (_1 _2) (previous)))]
               [next-button (make-object button% "Next >>" button-panel (lambda
                                                                            (_1 _2) (next)))]
               [canvas (make-object editor-canvas% frame)]
               
               [history null]
               [view 0]
               [final-view #f]
               
               [fetch-next-step
                (lambda ()
                  (let ([result (open-output-string)]
                        [output (open-output-string)])
                    (parameterize ([current-output-port output])
                      ; changed by JBC for list of results
                      (for-each
                       (lambda (expr)
                         (pretty-print expr result)
                         (fprintf result "~n"))
                       (step-thunk)))
                    (let ([outer-edit (make-object text%)])
                      
                      (send outer-edit insert (get-output-string result))
                      (send outer-edit insert #\newline)
                      (let ([between (send outer-edit last-position)])
                        (send outer-edit insert (get-output-string output))
                        (send outer-edit change-style result-delta 0 between)
                        (send outer-edit change-style output-delta between
                              (send outer-edit last-position))
                        (set! history (append history (list outer-edit)))))))]
               
               [update-view
                (lambda (new-view)
                  (set! view new-view)
                  (send canvas set-editor (list-ref history view))
                  (send previous-button enable (not (zero? view)))
                  (send home-button enable (not (zero? view)))
                  (send next-button enable (not (eq? final-view view))))]
               [update-view/next-step
                (lambda (new-view)
                  (fetch-next-step)
                  (when finished-stepping
                    (set! final-view new-view))
                  (update-view new-view))]
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
                  (update-view (- view 1)))])
        (send result-delta set-delta-foreground "BLACK")
        (send output-delta set-delta-foreground "PURPLE")
        (update-view/next-step 0)
        (send button-panel stretchable-width #f)
        (send canvas min-width 500)
        (send canvas min-height 500)
        (send previous-button enable #f)
        (send home-button enable #f)
        (send frame show #t))))
  )