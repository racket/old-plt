(module debugger-vc mzscheme
  (require (lib "unitsig.ss")
           (lib "debugger-sig.ss" "stepper")
           (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "framework.ss" "framework")
           "marks.ss")
  
  (provide debugger-vc@)
  
  (define debugger-vc@ 
    (unit/sig debugger-vc^
      (import debugger-model^)
      
      (define debugger-eventspace 
        (parameterize ([current-custodian user-custodian])
          (make-eventspace)))
      
      (define (receive-result result)
        (set! event-list (append event-list (list result)))
        (parameterize ([current-eventspace debugger-eventspace])
          (queue-callback
           (lambda ()
             (printf "new event arrived: ~a\n" result)))))
      
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
               (namespace-set-variable-value! 'go-semaphore go-semaphore)
               (namespace-set-variable-value! 'events events)
               (namespace-set-variable-value! 'user-custodian user-custodian))))))
  
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
      
    