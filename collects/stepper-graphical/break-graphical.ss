(unit/sig (break)
  (import [mred : mred^]
          [marks : stepper:marks^]
          [annotate : stepper:annotate^]
          [print-convert : mzlib:print-convert^])
  
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
; ^ not sure how to do this, insead we'll just have one debugging frame for the application
  
  (define break-semaphore (make-semaphore))
  (define break-resume-value #f)
  (define (continue val)
    (set! break-resume-value val)
    (semaphore-post break-semaphore))
  
  (define debugger-frame #f)
  (define level-listbox #f)
  (define binding-listbox #f)
  (define interaction-panel #f)
  
  (define (create-debugger-frame)
    (let* ([f (make-object frame% "debugger" #f 300 300)]
           [bp (make-object frame% horizontal-pane% f)]
           [step (make-object button% "continue" bp (lambda () 
                                                      (continue (void))))]
           [lp (make-object horizontal-panel% f)]
           [level-lb (make-object list-box% "level" () lp void)]
           [binding-lb (make-object list-box% "bindings" () lp void)]
           [interaction (make-object canvas% f)])
      (set! debugger-frame f)
      (set! level-listbox level-lb)
      (set! binding-listbox binding-lb)
      (set! interaction-panel interaction)))
   
  (define-struct frame-info (source full? bindings))
  
  (define (parse-break-info mark)
    (if (cheap-mark? mark)
        (make-frame-info (cheap-mark-source mark) #f ())
        (make-frame-info (mark-source mark) #t (mark-bindings mark))))
     
  (define (break)
    (let ([break-info-list (continuation-mark-set->list (current-continuation-marks) 
                                                        annotate:debug-key)])
      (parameterize
          ([mred:current-eventspace drscheme-eventspace])
        (mred:queue-callback 
         (lambda ()
           (when (not (debugger-frame))
             (create-debugger-frame))
           (let* ([frame-info-list (map parse-break-info break-info-list)]
                  [location-list (map (compose
                                       (lambda (v) (let ([sp (open-output-string)])
                                                     (write v sp)
                                                     (get-output-string sp)))
                                       (compose print-convert
                                                (compose zodiac:source
                                                         frame-info-source)))
                                      frame-info-list)])
             (send level-listbox clear)
             (for-each (lambda (name data)
                         (send level-listbox append name data))
                       location-list
                       frame-info-list)
           
           (current-namespace (make-namespace))
           (global-defined-value 'break-info break-info)
           (global-defined-value 'break-resume (lambda (val) 
                                                 (set! break-resume-value val)
                                                 (semaphore-post break-semaphore)))
           (global-defined-value 'expose-mark marks:expose-mark)
           (global-defined-value 'display-mark marks:display-mark)
           (mred:graphical-read-eval-print-loop)))
        (semaphore-wait break-semaphore)
        break-resume-value))))