(unit/sig (break)
  (import mzlib:core^
          [mred : mred^]
          [marks : stepper:marks^]
          [annotate : stepper:annotate^]
          [print-convert : mzlib:print-convert^]
          [zodiac : zodiac:system^]
          [utils : stepper:cogen-utils^]
          [e : zodiac:interface^])
  
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
  
  (define break-semaphore (make-semaphore))
  (define break-resume-value #f)
  (define (continue val)
    (set! break-resume-value val)
    (disable-debugger)
    (semaphore-post break-semaphore))
  
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
  
  (define (display-bindings frame-info)
    (send binding-listbox clear)
    (if (frame-info-full? frame-info)
        (let ([binding-names (map (compose
                                   (lambda (binding)
                                     (ccond [(zodiac:binding? binding)
                                             (utils:binding-orig-name binding)]
                                            [(box? binding)
                                             (if (null? (unbox binding))
                                                 (e:internal-error #f "empty slot in binding list")
                                                 (zodiac:varref-var (car (unbox binding))))]))
                                   marks:mark-binding-binding)
                                  (frame-info-bindings frame-info))])
          (send binding-listbox enable #t)
          (for-each (lambda (name data)
                      (send binding-listbox append name data))
                    binding-names
                    (map marks:mark-binding-value (frame-info-bindings frame-info))))
        (send binding-listbox enable #f)))
  
  (define debugger-frame (make-object mred:frame% "debugger" #f 300 300))
  (define button-panel (make-object mred:horizontal-panel% f))
  (send button-panel stretchable-height #f)
  (make-object mred:button% "continue" bp (lambda (a b) (continue (void))))
  (define listbox-panel (make-object mred:horizontal-panel% f))
  (define level-listbox (make-object mred:list-box% "level" () lp level-listbox-callback '(single)))
  (define binding-listbox (make-object mred:list-box% "bindings" () lp (lambda (a b) (void)) '(single)))
  (define interaction-canvas (make-object mred:canvas% f))
  
  (define (break)
    (let ([break-info-list (continuation-mark-set->list (current-continuation-marks) 
                                                        annotate:debug-key)])
      (parameterize
          ([mred:current-eventspace drscheme-eventspace])
        (mred:queue-callback 
         (lambda ()
           (when (not debugger-frame)
             (create-debugger-frame)
             (send debugger-frame show #t))
           (enable-debugger)
           (let* ([frame-info-list (map parse-break-info break-info-list)]
                  [location-list (map (function:compose
                                       (lambda (v) (let ([sp (open-output-string)])
                                                     (write v sp)
                                                     (get-output-string sp)))
                                       (function:compose print-convert:print-convert
                                                         (function:compose zodiac:zodiac-start
                                                                           frame-info-source)))
                                      frame-info-list)]
                  )
             (send level-listbox clear)
             (for-each (lambda (name data)
                         (send level-listbox append name data))
                       location-list
                       frame-info-list))))
        (semaphore-wait break-semaphore)
        break-resume-value))))