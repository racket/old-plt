

(module debugger-model mzscheme
  (require (lib "contracts.ss")
           (lib "etc.ss")
           (lib "mred.ss" "mred")
           "my-macros.ss"
           (prefix a: "annotate.ss")
           "shared.ss"
           "marks.ss")
 
  
  (define program-expander-contract
    (-> (-> void?) ; init
        (-> (union eof-object? syntax? (cons/p string? any?)) (-> void?) void?) ; iter
        void?))
    
  (provide/contract [go (-> program-expander-contract ; program-expander
                            eventspace? ; debugger-eventspace
                            void?)])
  
  (define (send-to-eventspace eventspace thunk)
    (parameterize ([current-eventspace eventspace])
      (queue-callback thunk)))

  (define debugger-debugger-error-port (current-error-port))
  
  ; go starts a stepper instance
  (define go 
    (lambda (program-expander debugger-eventspace)
      (local
          
          ((define debugger-out-port #f)
           
           (define current-expr #f)
           
           (define packaged-envs (a:make-initial-env-package))
           
           (define (send-to-debugger-eventspace thunk)
             (send-to-eventspace debugger-eventspace thunk))
           
           (define basic-eval (current-eval))

           (define user-program-semaphore (make-semaphore))
           (define receive-result (make-parameter void))
           
           (define user-custodian (box #f))
           
           (define (break mark-set break-kind returned-value-list)
             (let* ([mark-list (extract-mark-list mark-set)])
               (send-to-debugger-eventspace
                (lambda ()
                  ((receive-result) (make-normal-breakpoint-info mark-list break-kind returned-value-list))
                  ((receive-result) (make-breakpoint-halt))))
               (semaphore-wait user-program-semaphore)))
           
           (define (step-through-expression expanded expand-next-expression)
             (let*-values ([(annotated envs) (a:annotate expanded packaged-envs break 
                                                         'foot-wrap)])
               (set! packaged-envs envs)
               (set! current-expr expanded)
               (let ([expression-result
                      (parameterize ([current-eval basic-eval])
                        (eval annotated))])
                 (send-to-debugger-eventspace
                  (lambda ()
                    ((receive-result) (make-expression-finished (list expression-result)))
                    ((receive-result) (make-breakpoint-halt))))
                 (semaphore-wait user-program-semaphore)
                 (expand-next-expression))))
           
           (define (err-display-handler message exn)
             (send-to-debugger-eventspace
              (lambda ()
                ((receive-result) (make-error-breakpoint-info message))))))
        

        ; set up bindings in grepl:
        
        (fprintf debugger-debugger-error-port "about to set up debugger.\n")
        
        (parameterize ([current-eventspace debugger-eventspace])
          (queue-callback 
           (lambda ()
             (set! debugger-out-port (current-output-port))
             (namespace-set-variable-value! 'go-semaphore user-program-semaphore)
             (namespace-set-variable-value! 'receive-result receive-result)
             (namespace-set-variable-value! 'user-custodian user-custodian))))
        
        (fprintf debugger-debugger-error-port "about to call expand-program.\n")

        (program-expander
         (lambda ()
           (set-box! user-custodian (current-custodian))
           (current-output-port debugger-out-port)
           (error-display-handler err-display-handler)
           (fprintf debugger-debugger-error-port "about to perform first wait.\n")
           (semaphore-wait user-program-semaphore)) ; init
         (lambda (expanded continue-thunk) ; iter
           (unless (eof-object? expanded)
               (step-through-expression expanded continue-thunk))))))))

