(module debugger-vc mzscheme
  (require (lib "unitsig.ss")
           (lib "debugger-sig.ss" "stepper")
           (lib "mred.ss" "mred")
           "marks.ss")
  
  (provide debugger-vc@)
  
  (define debugger-vc@ 
    (unit/sig debugger-vc^
      (import debugger-model^)
      
      (define debugger-eventspace (make-eventspace))
      
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
      
      (define debugger-output-port
        (let ([sema (make-semaphore)]
              [op #f])
          (parameterize ([current-eventspace debugger-eventspace])
            (queue-callback 
             (lambda ()
               (set! op (current-output-port))
               (namespace-set-variable-value! 'go-semaphore go-semaphore)
               (namespace-set-variable-value! 'events events)
               (namespace-set-variable-value! 'user-custodian user-custodian)
               (semaphore-post sema))))
          (semaphore-wait sema)
          op)))))      
      
    