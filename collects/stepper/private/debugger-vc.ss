(module debugger-vc mzscheme
  (require (lib "unitsig.ss")
           (lib "debugger-sig.ss" "stepper")
           (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "framework.ss" "framework")
           "marks.ss"
           "debugger-bindings.ss")
  
  (provide debugger-vc@)
  
  (define debugger-vc@ 
    (unit/sig debugger-vc^
      (import debugger-model^)
      
      (define debugger-eventspace 
        (parameterize ([current-custodian user-custodian])
          (make-eventspace)))
      
      (define (receive-result result)
        (set! event-list (append event-list (list result)))
        (send-output-to-debugger-window (format-event result) debugger-output))
      
      (define (format-event debugger-event)
        (cond [(normal-breakpoint-info? debugger-event) 
               (format "normal breakpoint\nsource:~v\n" (car (expose-mark (car (normal-breakpoint-info-mark-list debugger-event)))))]
              [(error-breakpoint-info? debugger-event)
               (format "error breakpoint\nmessage: ~v\n" (error-breakpoint-info-message debugger-event))]
              [(breakpoint-halt? debugger-event)
               (format "breakpoint halt\n")]
              [(expression-finished? debugger-event)
               (format "expression finished\nresults: ~v\n" (expression-finished-returned-value-list debugger-event))]))
      
      
      (define event-list null)
      
      (define (events) event-list)
      
      (thread 
       (lambda () 
         (graphical-read-eval-print-loop debugger-eventspace #t)))
      
      (define debugger-output (make-output-window))
      
      ; set up debugger eventspace
      
      (parameterize ([current-eventspace debugger-eventspace])
            (queue-callback 
             (lambda ()
               ; yuck!  hidden dependence on the list of names provided by "debugger-bindings.ss"
               (namespace-set-variable-value! 'go-semaphore go-semaphore)
               (namespace-set-variable-value! 'events events)
               (namespace-set-variable-value! 'user-custodian user-custodian)
               (namespace-set-variable-value! 'set-event-num! set-event-num!)
               (namespace-set-variable-value! 'bt bt)
               (namespace-set-variable-value! 'set-frame-num! set-frame-num!)
               (namespace-set-variable-value! 'src src)
               (namespace-set-variable-value! 'binding binding))))))
  
  ;; Info functions:
  
  ;; Debugger Output Window:
  
  ; make-output-window : (-> text:basic%)
  (define (make-output-window)
    (let* ([frame (instantiate frame:basic% () 
                    (label "Debugger Output")
                    (width 400)
                    (height 400))]
           [canvas (instantiate canvas:basic% () (parent (send frame get-area-container)))]
           [text (instantiate text:basic% ())])
      (send canvas set-editor text)
      (send frame show #t)
      text))
  
  ; send-output-to-debugger-window : (string text:basic% -> void)
  (define (send-output-to-debugger-window str text)
    (send text insert str (send text last-position)))
    
    
    )      
      
    