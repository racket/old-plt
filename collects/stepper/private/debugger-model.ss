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
                            (-> string? semaphore? void?) ; receive-result
                            void?)])
  
  (define (send-to-eventspace eventspace thunk)
    (parameterize ([current-eventspace eventspace])
      (queue-callback thunk)))

  ; go starts a stepper instance
  (define go 
    (lambda (program-expander receive-result)
      (local
          
          ((define current-expr #f)
           
           (define packaged-envs (a:make-initial-env-package))
           
           (define drscheme-eventspace (current-eventspace))
           
           (define (send-to-drscheme-eventspace thunk)
             (send-to-eventspace drscheme-eventspace thunk))
           
           (define user-computation-semaphore (make-semaphore))
           
           (define basic-eval (current-eval))
           
           (define (break mark-set break-kind returned-value-list)
             (let* ([mark-list (extract-mark-list mark-set)]
                    [exposed (map expose-mark mark-list)])
               (send-to-drscheme-eventspace
                (lambda ()
                  (receive-result (format "*breakpoint*\nmark-list: ~e\nbreak-kind: ~e\nreturned-value-list: ~e\n" exposed break-kind returned-value-list)
                                  user-computation-semaphore)))
               (semaphore-wait user-computation-semaphore)))
           
           (define (step-through-expression expanded expand-next-expression)
             (let*-values ([(annotated envs) (a:annotate expanded packaged-envs break 
                                                         'foot-wrap)])
               (set! packaged-envs envs)
               (set! current-expr expanded)
               (let ([expression-result
                      (parameterize ([current-eval basic-eval])
                        (eval annotated))])
                 (send-to-drscheme-eventspace
                  (lambda ()
                    (receive-result (format "*expression finished*\nresulting value: ~e" expression-result)
                                    user-computation-semaphore)))
                 (semaphore-wait user-computation-semaphore)
                 (expand-next-expression))))
           
           (define (err-display-handler message exn)
             (send-to-drscheme-eventspace
              (lambda ()
                (receive-result (format "*error*\nmessage: ~e" message)
                                user-computation-semaphore)))))
        
        (program-expander
         (lambda ()
           (error-display-handler err-display-handler)) ; init
         (lambda (expanded continue-thunk) ; iter
           (unless (eof-object? expanded)
               (step-through-expression expanded continue-thunk))))))))

