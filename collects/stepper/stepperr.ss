(unit/sig stepper:settings^
  (import mzlib:pretty-print^
          mred^
          [b : userspace:basis^]
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
  
  (define stepper-canvas%
    (class editor-canvas% (parent (editor #f) (style null) (scrolls-per-page 100))
      (rename (super-on-size on-size))
      (override 
        (on-size 
         (lambda (width height)
           (super-on-size width height))))
      (sequence (super-init parent editor style scrolls-per-page))))
  
      ;; PROVIDE DEFAULTS FOR THESE VARIABLES
  ;;(send (send (get-style-list) basic-style) get-text-width dc)    
; [reset-pretty-print-width
;	   (lambda ()
;	     (let* ([standard (send (get-style-list) find-named-style "Standard")])
;	       (when standard
;		 (let* ([admin (get-admin)]
;			[width
;			 (let ([bw (box 0)]
;			       [b2 (box 0)])
;			   (send admin get-view b2 b2 bw b2)
;			   (unbox bw))]
;			[dc (send admin get-dc)]
;			[new-font (send standard get-font)]
;			[old-font (send dc get-font)])
;		   (send dc set-font new-font)
;		   (let* ([char-width (send dc get-char-width)]
;			  [min-columns 50]
;			  [new-columns (max min-columns 
;					    (floor (/ width char-width)))])
;		     (send dc set-font old-font)
;		     (mzlib:pretty-print:pretty-print-columns new-columns))))))]
  
  (define (stepper-go frame settings)
    (letrec ([edit (ivar frame definitions-text)]
             [text (send edit get-text)]
             [stepper-semaphore (make-semaphore)]
             [expr-list #f]
             
             [history null]
             
             [highlight-color (make-object color% 193 251 181)]
             
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
             [error-delta (make-object style-delta%)]
             
             [button-panel (make-object horizontal-panel% s-frame)]
             [home-button (make-object button% "Home" button-panel
                                       (lambda (_1 _2) (home)))]
             [previous-button (make-object button% "<< Previous" button-panel
                                           (lambda (_1 _2) (previous)))]      
             
             [next-button (make-object button% "Next >>" button-panel (lambda
                                                                          (_1 _2) (next)))]
             [canvas (make-object stepper-canvas% s-frame)]
             
             [update-view
              (lambda (new-view)
                (set! view new-view)
                (send canvas set-editor (list-ref history view))
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
                (parameterize ([current-eventspace user-eventspace])
                  (queue-callback 
                   (lambda ()
                     (call-with-current-continuation
                      (lambda (k)
                        (let ([primitive-eval (current-eval)])
                          (d:basis:initialize-parameters (make-custodian) null settings)
                          (let-values ([(annotated exprs)
                                        (a:annotate text break (make-exception-handler k))])
                            (set! expr-list exprs)
                            (set! par-namespace (current-namespace))
                            (set! par-global-defined-vars (map car (make-global-value-list)))
                            (set! par-constructor-style-printing (p:constructor-style-printing))
                            (set! par-abbreviate-cons-as-list (p:abbreviate-cons-as-list)) 
                            (set! par-empty-list-name (p:empty-list-name))
                            (set! par-show-sharing (p:show-sharing))
                            (set! par-cons (global-defined-value 'cons))
                            (set! par-vector (global-defined-value 'vector))
                            (current-exception-handler
                             (make-exception-handler k))
                            (for-each primitive-eval annotated)))))))))]
             
             [update-view/next-step
              (lambda (new-view)
                (set! view-currently-updating new-view)
                (semaphore-post stepper-semaphore))])
      
      
      
      (send result-delta set-delta-foreground "BLACK")
      (send output-delta set-delta-foreground "PURPLE")
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
      (send s-frame show #t)))
  
  (lambda (frame)
    (let ([settings (f:preferences:get 'drscheme:settings)])
      (if (not (string=? (b:setting-name settings) "Beginner"))
          (message-box "Stepper" 
                       (format "Language level is set to \"~a\".~nPlease set the language level to \"Beginner\"" 
                               (b:setting-name settings))
                       #f 
                       '(ok))
          (stepper-go frame settings)))))
