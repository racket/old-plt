(unit/sig (break)
  (import mzlib:core^
          [m : mred^]
          [marks : stepper:marks^]
          [annotate : stepper:annotate^]
          [pc : mzlib:print-convert^]
          [pp : mzlib:pretty-print^]
          [z : zodiac:system^]
          [utils : stepper:cogen-utils^]
          [e : zodiac:interface^]
          [fw : framework^]
          [d : drscheme:export^])
  
  (define value-text%
    (class fw:text:basic% ((line-spacing 1.0) (tabstops null))
      (inherit insert get-style-list set-style-list change-style highlight-range last-position lock erase
               begin-edit-sequence end-edit-sequence get-start-position select-all clear get-canvas)
      (public [reset-pretty-print-width
               (when has-value?
                 (let ([canvas (get-canvas)])
                   (let-values ([(width height) (send canvas get-client-size)])
                     (let* ([style (send (get-style-list) find-named-style "Standard")]
                            [char-width (send style get-text-width (send canvas get-dc))]
                            [width (floor (/ (- width 18) char-width))])
                       (reformat-value width)))))]
              
              [set-value!
               (lambda (new-value)
                 (set! value new-value)
                 (set! has-value? #t)
                 (set! pretty-printed-width #f)
                 (reset-pretty-print-width))]
              
              [clear-value!
               (lambda ()
                 (set! value 'bad-value)
                 (set! has-value? #f)
                 (clear))]
      
      (private [value 'bad-value]
               [has-value?  #f]
               [print-converted (pc:print-convert value)]
               [pretty-printed-width #f]               
               [reset-style
                (lambda ()
                  (change-style (send (get-style-list) find-named-style "Standard")))]
               
               [reformat-value
                (lambda (width)
                  (when (not (eq? pretty-printed-width width))
                    (set! pretty-printed-width width)
                    (format-value)))]
               
               [format-value
                (lambda ()
                  (begin-edit-sequence)
                  (lock #f)
                  (erase)
                  (let ([real-print-hook (pp:pretty-print-print-hook)])
                    (parameterize ([pp:pretty-print-display-string-handler
                                    (lambda (string port)
                                      (insert string))]
                                   [pp:pretty-print-print-line
                                    (lambda (number port old-length dest-columns)
                                      (when (and number (not (eq? number 0)))
                                        (insert #\newline))
                                      0)])
                      (pp:pretty-print print-converted)))
                  (lock #t)
                  (end-edit-sequence))])
      (sequence (super-init line-spacing tabstops)
                (set-style-list (fw:scheme:get-style-list)))))
  
  (fw:preferences:set-default 'debugger-width 400)
  (fw:preferences:set-default 'debugger-height 300)
  
  (define debugger-frame%
    (class (fw:frame:standard-menus-mixin fw:frame:basic%) (debugger)
      (rename [super-on-size on-size]
              [super-can-close? can-close?])
      (public
        [set-printing-proc 
         (lambda (proc)
           (set! printing-proc proc))]
        [printing-proc (lambda (item evt)
                         (printf "shouldn't be called~n"))])
      (inherit show)
      (override
        [on-size
         (lambda (width height)
           (fw:preferences:set 'debugger-width width)
           (fw:preferences:set 'debugger-height height)
           (send debugger on-size-frame)
           (super-on-size width height))]
        
        [can-close?
         (lambda ()
           (if (semaphore-try-wait? debugger-available-semaphore)
               (begin
                 (semaphore-post debugger-available-semaphore)
                 (super-can-close?))
               (begin 
                 (m:message-box "oops!" (string-append "You cannot close the debugger window "
                                                       "while a breakpoint is waiting.")
                                #f '(ok))
                 #f)))])
      (sequence (super-init "Debugger" #f
                            (fw:preferences:get 'debugger-width)
                            (fw:preferences:get 'debugger-height)))))
    
  (define drscheme-eventspace (m:current-eventspace))

  ; ----------------------------------------------------------------

  ; gui setup
  
  (define test-dc (make-object bitmap-dc% (make-object bitmap% 1 1)))
  (define debug-highlight-color (make-object color% 255 255 255))
  (send test-dc try-color (make-object color% 145 211 219) debug-highlight-color)
  (define clear-highlight-thunk #f)

  ; ----------------------------------------------------------------

  (define (binding-name binding)
    (ccond [(z:binding? binding)
            (z:binding-orig-name binding)]
           [(box? binding)
            (if (null? (unbox binding))
                (e:internal-error #f "empty slot in binding list")
                (z:varref-var (car (unbox binding))))]))
  
  ; extend drscheme frame to hold a debugger-frame
  
  (define debugger%
    (class object% ()
      (private [frame (make-object debugger-frame%)]
               [area-container (send f get-area-container)]
               [button-panel (make-object m:horizontal-panel% area-container)]
               [listbox-panel (make-object m:horizontal-panel% area-container)]
               [level-listbox (make-object m:list-box% "level" () listbox-panel level-listbox-callback '(single))]
               [binding-listbox (make-object m:list-box% "bindings" () binding-listbox-calback '(single))]
               [interactions-canvas (make-object m:editor-canvas% area-container)]
               [value-text (make-object value-text%)]
               [break-semaphore (make-semaphore)])
      
      (sequence (send button-panel stretchable-height #f)
                (make-object m:button% "continue" button-panel continue-callback))
      
      (private [level-listbox-callback
                (lambda (listbox event)
                  (case (send event get-event-type) 
                    ((list-box) 
                     (let ([selections (send listbox get-selections)])
                       (when (not (null? selections))
                         (display-bindings (send listbox get-data (car selections))))))))]                

                [binding-listbox-callback
                 (lambda (listbox event)
                   (case (send event get-event-type)
                     ((list-box)
                      (let ([selections (send listbox get-selections)])
                        (when (not (null? selections))
                          (send value-text set-value! (send listbox get-data (car selections))))))))]
  
                 [display-bindings 
                  (lambda (frame-info)
                    (send binding-listbox clear)
                    (send interactions-canvas set-editor #f)
                    (if (frame-info-full? frame-info)
                        (let ([binding-names (map (function:compose binding-name marks:mark-binding-binding)
                                                  (frame-info-bindings frame-info))])
                          (send binding-listbox enable #t)
                          (for-each (lambda (name data)
                                      (send binding-listbox append (symbol->string name) data))
                                    binding-names
                                    (map marks:mark-binding-value (frame-info-bindings frame-info))))
                        (send binding-listbox enable #f)))]

                 )
      
      (public [on-size-frame
               (lambda ()
                 (send value-text reset-pretty-print-width))]
              
              [handle-breakpoint
               (lambda (frame-info)
                 
                 
  )
      
    
 
  ; add the get-debugger-frame and set-debugger-frame! methods to the drscheme frame
  (drscheme:get/extend:extend-unit-frame
   (lambda (super%)
     (class super% args
       (sequence (apply super-init args))
       (private [debugger-i #f])
       (public
         [debugger
          (lambda ()
            (when (not debugger-i)
              (set! debugger-i (make-object debugger%)))
            debugger-i)]))))

  ; ----------------------------------------------------------------

  ; set up debugger preferences panel
  (fw:preferences:set-default 'ankle-annotation #f boolean?)
  
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

  (define (continue debugger)
    (send (debugger-bp debugger) enable #f)
    (semaphore-post (debugger-bs debugger)))
  
  (define-struct frame-info (source full? bindings))
  
  (define (parse-break-info mark)
    (if (marks:cheap-mark? mark)
        (make-frame-info (marks:cheap-mark-source mark) #f ())
        (make-frame-info (marks:mark-source mark) #t (marks:mark-bindings mark))))
  
  
  

  

  
  ; ----------------------------------------------------------------

  (define (is-drscheme-definitions-editor? editor)
    (and (send editor get-canvas)
         (is-a? (send editor get-canvas) d:unit:definitions-canvas%)))
  
  (define (find-location-text zodiac)
    (let* ([source (z:location-file (z:zodiac-start zodiac))])
      (if (is-drscheme-definitions-editor? source fw:text:basic%)
          (let* ([start-offset (z:location-offset (z:zodiac-start zodiac))]
                 [finish-offset (z:location-offset (z:zodiac-finish zodiac))])
            "Can't copy text yet")
          (string-append "~a : ~a" (z:location-offset (z:zodiac-start zodiac)) (z:location-file (z:zodiac-start zodiac))))))
    
  (define (highlight-location-text zodiac)
    (when clear-highlight-thunk (clear-highlight-thunk))
    (let* ([source (z:location-file (z:zodiac-start zodiac))])
      (when (is-drscheme-definitions-editor? source fw:text:basic%)
        (let* ([start-offset (z:location-offset (z:zodiac-start zodiac))]
               [finish-offset (z:location-offset (z:zodiac-finish zodiac))])
          (set! clear-highlight-thunk (send source highlight-range start-offset finish-offset debug-color))))))
  
  (define (break)
    (let ([break-info-list (continuation-mark-set->list (current-continuation-marks) 
                                                        annotate:debug-key)])
      (parameterize
          ([m:current-eventspace drscheme-eventspace])
        (m:queue-callback 
         (lambda ()
           (send debugger-frame show #t)
           (enable-debugger)
           (let* ([frame-info-list (map parse-break-info break-info-list)]
                  [location-list (map (function:compose find-location-text frame-info-source) frame-info-list)])
             (send level-listbox clear)
             (send binding-listbox clear)
             (for-each (lambda (name data)
                         (send level-listbox append name data))
                       location-list
                       frame-info-list))))
        (semaphore-wait break-semaphore)
        (semaphore-post debugger-available-semaphore)
        break-resume-value))))