(unit/sig (break)
  (import mzlib:core^
          [mred : mred^]
          [marks : stepper:marks^]
          [annotate : stepper:annotate^]
          [pc : mzlib:print-convert^]
          [pp : mzlib:pretty-print^]
          [zodiac : zodiac:system^]
          [utils : stepper:cogen-utils^]
          [e : zodiac:interface^]
          [fw : framework^])
  
  (define value-text%
    (class fw:text:basic% (value (line-spacing 1.0) (tabstops null))
      (inherit insert get-style-list set-style-list change-style highlight-range last-position lock erase
               begin-edit-sequence end-edit-sequence get-start-position select-all clear)
      (public [reset-pretty-print-width
               (lambda (canvas)
                 (let-values ([(width height) (send canvas get-client-size)])
                   (let* ([style (send (get-style-list) find-named-style "Standard")]
                          [char-width (send style get-text-width (send canvas get-dc))]
                          [width (floor (/ (- width 18) char-width))])
                     (reformat-value width))))])
      
      (private [print-converted (pc:print-convert value)]
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
  
  (fw:preferences:set 'debugger-width 400)
  (fw:preferences:set 'debugger-height 300)
  
  (define debugger-frame%
    (class (fw:frame:standard-menus-mixin fw:frame:basic%) ()
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
           (if debugger-displayed-value-text
               (send debugger-displayed-value-text reset-pretty-print-width interaction-canvas))
           (super-on-size width height))]
        
        [can-close?
         (lambda ()
           (if (semaphore-try-wait? debugger-available-semaphore)
               (begin
                 (semaphore-post debugger-available-semaphore)
                 (super-can-close?))
               (begin 
                 (mred:message-box "oops!" (string-append "You cannot close the debugger window "
                                                          "while a breakpoint is waiting.")
                                   #f '(ok))
                 #f)))])
      (sequence (super-init "Debugger" #f
                            (fw:preferences:get 'debugger-width)
                            (fw:preferences:get 'debugger-height)))))
    
  (define drscheme-eventspace (mred:current-eventspace))

;  ; add the get-debugger-frame and set-debugger-frame! methods to the drscheme frame
;  (drscheme:get/extend:extend-unit-frame
;   (lambda (super%)
;     (class super% args
;       (sequence (apply super-init args))
;       (private [debugger-frm #f])
;       (public
;         [debugger-frame
;          (case-lambda
;           (() debugger-frm)
;           ((new-frame) (set! debugger-frm new-frame)))]))))
; ^ not sure how to do this, instead we'll just have one debugging frame for the application

  ; setup debugger preferences panel
  (fw:preferences:set-default 'ankle-annotation #f boolean?)
  
  (define (ankle-pref-callback cbox event)
    (case (send event get-event-type)
      ((check-box)
       (let ([setting (send cbox get-value)])
         (fw:preferences:set 'ankle-annotation setting)))))
  
  (define (create-debugger-prefs-panel parent)
    (local
        ((define cbox-panel (make-object mred:vertical-panel% parent))
         (define debugger-checkbox (make-object mred:check-box% "Enable Lightweight Debugger" cbox-panel 
                                     ankle-pref-callback null)))
      (send debugger-checkbox set-value (fw:preferences:get 'ankle-annotation))
      cbox-panel))
  
  (fw:preferences:add-panel "debugging" create-debugger-prefs-panel)
  
  (define break-semaphore (make-semaphore))
  (define break-resume-value #f)
  (define (continue val)
    (set! break-resume-value val)
    (disable-debugger)
    (semaphore-post break-semaphore))
  (define debugger-available-semaphore (make-semaphore)) ; to ensure that only one program is in the debugger.
  (semaphore-post debugger-available-semaphore)
  
  (define (enable-debugger)
    (send button-panel enable #t))
  
  (define (disable-debugger)
    (send button-panel enable #f))
  
  (define-struct frame-info (source full? bindings))
  
  (define (parse-break-info mark)
    (if (marks:cheap-mark? mark)
        (make-frame-info (marks:cheap-mark-source mark) #f ())
        (make-frame-info (marks:mark-source mark) #t (marks:mark-bindings mark))))
  
  (define (level-listbox-callback listbox event)
    (case (send event get-event-type) 
      ((list-box) 
       (let ([selections (send listbox get-selections)])
         (when (not (null? selections))
           (display-bindings (send listbox get-data (car selections))))))))
  
  (define (binding-listbox-callback listbox event)
    (case (send event get-event-type)
      ((list-box)
       (let ([selections (send listbox get-selections)])
         (when (not (null? selections))
           (display-binding-value (send listbox get-data (car selections))))))))
  
  (define (display-bindings frame-info)
    (send binding-listbox clear)
    (if (frame-info-full? frame-info)
        (let ([binding-names (map (function:compose
                                   (lambda (binding)
                                     (ccond [(zodiac:binding? binding)
                                             (zodiac:binding-orig-name binding)]
                                            [(box? binding)
                                             (if (null? (unbox binding))
                                                 (e:internal-error #f "empty slot in binding list")
                                                 (zodiac:varref-var (car (unbox binding))))]))
                                   marks:mark-binding-binding)
                                  (frame-info-bindings frame-info))])
          (send binding-listbox enable #t)
          (for-each (lambda (name data)
                      (send binding-listbox append (symbol->string name) data))
                    binding-names
                    (map marks:mark-binding-value (frame-info-bindings frame-info))))
        (send binding-listbox enable #f)))
  
  (define (display-binding-value value)
    (let* ([value-text (make-object value-text% value)])
      (set! debugger-displayed-value-text value-text)
      (send interaction-canvas set-editor value-text)
      (send value-text reset-pretty-print-width interaction-canvas)))
  
  (define debugger-frame (make-object debugger-frame%))
  (define area-container (send debugger-frame get-area-container))
  (define button-panel (make-object mred:horizontal-panel% area-container))
  (send button-panel stretchable-height #f)
  (make-object mred:button% "continue" button-panel (lambda (a b) (continue (void))))
  (define listbox-panel (make-object mred:horizontal-panel% area-container))
  (define level-listbox (make-object mred:list-box% "level" () listbox-panel level-listbox-callback '(single)))
  (define binding-listbox (make-object mred:list-box% "bindings" () listbox-panel binding-listbox-callback '(single)))
  (define interaction-canvas (make-object mred:editor-canvas% area-container))
  (define debugger-displayed-value-text #f)

  
  (define (break)
    (semaphore-wait debugger-available-semaphore)
    (let ([break-info-list (continuation-mark-set->list (current-continuation-marks) 
                                                        annotate:debug-key)])
      (parameterize
          ([mred:current-eventspace drscheme-eventspace])
        (mred:queue-callback 
         (lambda ()
           (send debugger-frame show #t)
           (enable-debugger)
           (let* ([frame-info-list (map parse-break-info break-info-list)]
                  [location-list (map (function:compose
                                       (lambda (v) (let ([sp (open-output-string)])
                                                     (parameterize ([print-struct #t])
                                                       (write v sp))
                                                     (get-output-string sp)))
                                       (function:compose zodiac:zodiac-start
                                                         frame-info-source))
                                      frame-info-list)])
             (send level-listbox clear)
             (for-each (lambda (name data)
                         (send level-listbox append name data))
                       location-list
                       frame-info-list))))
        (semaphore-wait break-semaphore)
        (semaphore-post debugger-available-semaphore)
        break-resume-value))))