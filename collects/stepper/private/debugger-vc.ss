(module debugger-vc mzscheme
  (require (lib "unitsig.ss")
           (lib "debugger-sig.ss" "stepper")
           (lib "mred.ss" "mred")
           "marks.ss")
  
  (provide debugger-vc@)
  
  (define debugger-vc@ 
    (unit/sig debugger-vc^
      (import debugger-model^)
      
      (define (receive-result result)
        ((receive-result-parameter) result))
      
      (define receive-result-parameter
        (make-parameter
         (lambda (dont-care)
           (message-box/custom "not set yet"
                               "The receive-result function has not yet been set"
                               "OK"
                               #f
                               #f
                               #f
                               '(default=1 stop)))))

      (define debugger-eventspace (make-eventspace))
      
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
               (namespace-set-variable-value! 'receive-result-parameter receive-result-parameter)
               (namespace-set-variable-value! 'user-custodian user-custodian)
               (semaphore-post sema))))
          (semaphore-wait sema)
          op)))))      
      
    