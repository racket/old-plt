(unit/sig ()
  (import [mred : mred-interfaces^]
          mzlib:core^
          [fw : framework^]
          mzlib:print-convert^
          (drscheme : drscheme:export^)
          zodiac:system^)
  
  (define invoke-stepper
    (lambda (frame)
      (fw:gui-utils:show-busy-cursor
       (lambda ()
         (let* ([e (mred:make-eventspace)]
                [f (parameterize ([mred:current-eventspace e])
                     (mred:begin-busy-cursor)
                     (make-object mred:frame% "The Foot"))]
                [p (make-object mred:horizontal-panel% f)]
                [m (make-object mred:message% "Please wait, loading the foot." p)]
                [spacer-pane (make-object mred:grow-box-spacer-pane% p)])
           (send p stretchable-height #f)
           (send p stretchable-width #f)
           (send f show #t)
           (parameterize ([mred:current-eventspace e])
             (mred:flush-display) (mred:yield))
           (set! invoke-stepper 
                 (invoke-unit/sig
                  (require-library "stepper.ss" "stepper")
                  mzlib:core^
                  (fw : framework^)
                  mzlib:print-convert^
                  (mred : mred-interfaces^)
                  (drscheme : drscheme:export^)
                  zodiac:system^))
           (send f show #f)
           (parameterize ([mred:current-eventspace e])
             (mred:end-busy-cursor))
           (invoke-stepper frame))))))
  
  (define stepper-bitmap
    (drscheme:unit:make-bitmap
     "Step"
     (build-path (collection-path "icons") "foot.bmp")))
  
  (drscheme:get/extend:extend-unit-frame%
   (lambda (super%)
     (class super% args
       (inherit button-panel)
       (sequence (apply super-init args))
       (rename [super-disable-evaluation disable-evaluation]
               [super-enable-evaluation enable-evaluation])
       (override
         [enable-evaluation
          (lambda ()
            (send stepper-button enable #t)
            (super-enable-evaluation))]
         [disable-evaluation
          (lambda ()
            (send stepper-button enable #f)
            (super-disable-evaluation))])
       (public
         [stepper-button (make-object mred:button%
                                      (stepper-bitmap this)
                                      button-panel
                                      (lambda (button evt) 
                                        (invoke-stepper this)))])
       (sequence
         (send button-panel change-children
               (lambda (l)
                 (cons stepper-button (function:remq stepper-button l)))))))))
