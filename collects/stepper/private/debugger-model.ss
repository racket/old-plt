

(module debugger-model mzscheme
  (require (lib "unitsig.ss")
           (lib "contracts.ss")
           (lib "etc.ss")
           (lib "mred.ss" "mred")
           "my-macros.ss"
           (prefix a: "annotate.ss")
           "shared.ss"
           "marks.ss"
           (lib "debugger-sig.ss" "stepper")
           "debugger-vc.ss")
 
  
  (define program-expander-contract
    (-> (-> void?) ; init
        (-> (union eof-object? syntax? (cons/p string? any?)) (-> void?) void?) ; iter
        void?))
    
  (provide/contract [go (-> program-expander-contract ; program-expander
                            void?)])
  
  (define (send-to-eventspace eventspace thunk)
    (parameterize ([current-eventspace eventspace])
      (queue-callback thunk)))

  (define debugger-debugger-error-port (current-error-port))

  ; go starts a stepper instance
  (define go 
    (lambda (program-expander)
      (local
          
          ((define go-semaphore (make-semaphore))
           (define user-custodian (make-custodian))
           
           (define-values/invoke-unit/sig debugger-vc^ debugger-vc@ #f debugger-model^)

           (define queue-eventspace (make-eventspace))
           
           (define current-expr #f)
           
           (define packaged-envs (a:make-initial-env-package))
           
           (define (queue-result result)
             (send-to-eventspace 
              queue-eventspace
              (lambda ()
                (receive-result result))))
           
           (define basic-eval (current-eval))

           (define (break mark-set break-kind returned-value-list)
             (let* ([mark-list (extract-mark-list mark-set)])
               (queue-result (make-normal-breakpoint-info mark-list break-kind returned-value-list))
               (queue-result (make-breakpoint-halt))
               (semaphore-wait go-semaphore)))
           
           (define (step-through-expression expanded expand-next-expression)
             (let*-values ([(annotated envs) (a:annotate expanded packaged-envs break 
                                                         'foot-wrap)])
               (set! packaged-envs envs)
               (set! current-expr expanded)
               (let ([expression-result
                      (parameterize ([current-eval basic-eval])
                        (eval annotated))])
                 (queue-result (make-expression-finished (list expression-result)))
                 (queue-result (make-breakpoint-halt))
                 (semaphore-wait go-semaphore)
                 (expand-next-expression))))
           
           (define (err-display-handler message exn)
             (queue-result (make-error-breakpoint-info message))))
        
        (fprintf debugger-debugger-error-port "about to call expand-program.\n")

        (parameterize ([current-custodian user-custodian])
          (program-expander
           (lambda ()
             (current-output-port debugger-output-port)
             (error-display-handler err-display-handler)
  
             ) ; init
           (lambda (expanded continue-thunk) ; iter
             (unless (eof-object? expanded)
               (step-through-expression expanded continue-thunk)))))))))

