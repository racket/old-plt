(unit/sig (break)
  (import mzlib:core^
          [m : mred^]
          [marks : stepper:marks^]
          [pc : mzlib:print-convert^]
          [pp : mzlib:pretty-print^]
          [z : zodiac:system^]
          [e : zodiac:interface^]
          [fw : framework^]
          [d : drscheme:export^])
  
  
  
  (fw:preferences:set-default 'debugger-width 400 number?)
  (fw:preferences:set-default 'debugger-height 800 number?)
  
  (define debugger-frame%
    (class (fw:frame:standard-menus-mixin fw:frame:basic%) (debugger)
      (rename [super-on-size on-size]
              [super-can-close? can-close?])
      (override
        [on-size
         (lambda (width height)
           (fw:preferences:set 'debugger-width width)
           (fw:preferences:set 'debugger-height height)
           (send debugger on-size-frame)
           (super-on-size width height))]
        
        [can-close?
         (lambda ()
           (and (send debugger can-close-frame?)
                (super-can-close?)))])
      (sequence (super-init "Debugger" #f
                            (fw:preferences:get 'debugger-width)
                            (fw:preferences:get 'debugger-height)))))
  
  (define drscheme-eventspace (m:current-eventspace))
  
  ; ----------------------------------------------------------------
  
  ; gui setup
  
  (define test-dc (make-object m:bitmap-dc% (make-object m:bitmap% 1 1)))
  (define debug-highlight-color (make-object m:color% 255 255 255))
  (send test-dc try-color (make-object m:color% 145 211 219) debug-highlight-color)
  (define clear-highlight-thunk #f)
  
  ; ----------------------------------------------------------------
  
  (define (binding-name binding)
    (ccond [(z:binding? binding)
            (z:binding-orig-name binding)]
           [(box? binding)
            (if (null? (unbox binding))
                (e:internal-error #f "empty slot in binding list")
                (z:varref-var (car (unbox binding))))]))
  
  (define (is-drscheme-definitions-editor? editor)
    (and (is-a? editor m:editor<%>)
         (send editor get-canvas)
         (is-a? (send editor get-canvas) d:unit:definitions-canvas%)))
  
  (define (get-drscheme-frame editor)
    (and (is-drscheme-definitions-editor? editor)
         (send (send editor get-canvas) get-top-level-window)))
  
  (define (find-location-text zodiac)
    (let* ([source (z:location-file (z:zodiac-start zodiac))])
      (if (is-drscheme-definitions-editor? source)
          (let* ([start-offset (z:location-offset (z:zodiac-start zodiac))]
                 [finish-offset (z:location-offset (z:zodiac-finish zodiac))])
            "Can't copy text yet")
          (format "~a : ~a" (z:location-offset (z:zodiac-start zodiac)) (z:location-file (z:zodiac-start zodiac))))))
  
  (define (highlight-location-text zodiac)
    (when clear-highlight-thunk (clear-highlight-thunk))
    (let* ([source (z:location-file (z:zodiac-start zodiac))])
      (if (is-drscheme-definitions-editor? source)
          (let* ([start-offset (z:location-offset (z:zodiac-start zodiac))]
                 [finish-offset (z:location-offset (z:zodiac-finish zodiac))])
            (send source highlight-range start-offset (+ finish-offset 1) debug-highlight-color #f))
          #f)))
  
  (define debugger%
    (class object% (drscheme-frame)

      (private [parsed #f]
               [needs-update #t])
      (public [set-zodiac!
               (lambda (new-parsed)
                 (if (z:parsed? new-parsed)
                     (set! parsed new-parsed)
                     (set! needs-update #t)
                     (e:internal-error new-parsed "not a parsed zodiac value")))]
              
              [handle-breakpoint
               (lambda (frame-info-list semaphore)
                 (set! break-semaphore semaphore)
                 (when needs-update
                   (send defns-text clear)
                   (send drscheme-frame drscheme:get/extend
                 (send frame show #t)
                 )]
                 
              
              )
                 
      (sequence (super-init))
      
      (private [frame (make-object debugger-frame% this)]
               [area-container (send frame get-area-container)]
               [button-panel (make-object m:horizontal-panel% area-container)]
               [defns-canvas (make-object m:editor-canvas% area-container)]
               [defns-text (make-object f:text:basic%)]
               [break-semaphore #f])
      
      (sequence (send button-panel stretchable-height #f)
                (make-object m:button% "continue" button-panel continue-callback)
                (send interactions-canvas set-editor value-text))
      
      (public [on-size-frame
               (lambda ()
                 (send value-text reset-pretty-print-width))]
              
              [can-close-frame?
               (lambda ()
                 (if break-semaphore
                     (begin 
                       (m:message-box "oops!" (string-append "You cannot close the debugger window "
                                                             "while a breakpoint is waiting.")
                                      #f '(ok))
                       #f)
                     #t))]
              
              )))
  
  
  
  ; add the get-debugger-frame and set-debugger-frame! methods to the drscheme frame
  (d:get/extend:extend-unit-frame
   (lambda (super%)
     (class super% args
       (sequence (apply super-init args))
       (private [debugger #f])
       (public
         [get-debugger
          (lambda ()
            (when (not debugger)
              (set! debugger (make-object debugger%)))
            debugger)]))))
  
  ; ----------------------------------------------------------------
  
  ; set up debugger preferences panel
  
  (fw:preferences:set-default 'ankle-annotation #f boolean?)
  
  ;link to parameter
  (marks:ankle-wrap-enabled (fw:preferences:get 'ankle-annotation))
  (fw:preferences:add-callback 'ankle-annotation
                               (lambda (pref-name val)
                                 (marks:ankle-wrap-enabled val)
                                 #t))
  
  
  (define (ankle-pref-callback cbox event)
    (case (send event get-event-type)
      ((check-box)
       (let ([setting (send cbox get-value)])
         (fw:preferences:set 'ankle-annotation setting)))))
  
  (define (create-debugger-prefs-panel parent)
    (local
        ((define cbox-panel (make-object m:vertical-panel% parent))
         (define debugger-checkbox (make-object m:check-box% "Enable Lightweight Debugger" cbox-panel 
                                     ankle-pref-callback null)))
      (send debugger-checkbox set-value (fw:preferences:get 'ankle-annotation))
      cbox-panel))
  
  (fw:preferences:add-panel "debugging" create-debugger-prefs-panel)
  
  ; ----------------------------------------------------------------
  
  
  (define-struct frame-info (source full? bindings))
  
  (define (parse-break-info mark)
    (if (marks:cheap-mark? mark)
        (make-frame-info (marks:cheap-mark-source mark) #f ())
        (make-frame-info (marks:mark-source mark) #t (marks:mark-bindings mark))))
  
  
  (define (break)
    (let* ([break-info-list (marks:extract-mark-list  (current-continuation-marks))]
           [new-semaphore (make-semaphore)])
      (when (null? break-info-list)
        (error 'breakpoint "no marks to debug (could be in No Debugging mode?)"))
      (parameterize
          ([m:current-eventspace drscheme-eventspace])
        (m:queue-callback 
         (lambda ()
           (let* ([frame-info-list (map parse-break-info break-info-list)]
                  [drscheme-frame (let loop ([frame-info-list frame-info-list])
                                    (if (null? frame-info-list)
                                        (error "no drscheme marks to hang the debugger from")
                                        (or (get-drscheme-frame (z:location-file
                                                                 (z:zodiac-start
                                                                  (frame-info-source (car frame-info-list)))))
                                            (loop (cdr frame-info-list)))))]
                  [debugger (send drscheme-frame get-debugger)])
             (send debugger handle-breakpoint frame-info-list new-semaphore))))
        (semaphore-wait new-semaphore)
        (void)))))