(module debugger-model mzscheme
  (require (lib "contracts.ss")
           (lib "etc.ss")
           (lib "mred.ss" "mred")  
           (prefix frame: (lib "framework.ss" "framework"))
           "my-macros.ss"
           (prefix a: "annotate.ss")
           (prefix r: "reconstruct.ss")
           "shared.ss"
           "marks.ss"
           "highlight-placeholder.ss"
           "model-settings.ss")
 
  
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
             (let* ([mark-list (extract-mark-list mark-set)])
               (case break-kind
                       [(normal-break)
                        (let*-2vals ([(reconstructed redex-list) (reconstruct-helper)])
                                    (set! held-expr-list reconstructed)
                                    (set! held-redex-list redex-list))]
                       
                       [(result-exp-break result-value-break)
                        (if (eq? held-expr-list skipped-step)
                            (begin 
                              (set! held-expr-list no-sexp)
                              (set! held-redex-list no-sexp))
                            (let*-2vals ([(reconstructed reduct-list) (reconstruct-helper)])
                                        ; this invariant (contexts should be the same)
                                        ; fails in the presence of unannotated code.  For instance,
                                        ; currently (map my-proc (cons 3 empty)) goes to
                                        ; (... <body-of-my-proc> ...), where the context of the first one is
                                        ; empty and the context of the second one is (... ...).
                                        ; so, I'll just disable this invariant test.
                                        ;(when (not (equal? reconstructed held-expr-list))
                                        ;  (error 'reconstruct-helper
                                        ;         "pre- and post- redex/uct wrappers do not agree:~nbefore: ~a~nafter~a"
                                        ;         held-expr-list reconstructed))
                                        (let ([result
                                               (if (not (eq? held-expr-list no-sexp))
                                                   (let*-values 
                                                       ([(new-finished current-pre current-post after) 
                                                         (double-redivide finished-exprs held-expr-list reconstructed)])
                                                     (make-before-after-result new-finished current-pre held-redex-list current-post reduct-list after))
                                                   (let*-values
                                                       ([(before current after) (redivide reconstructed)])
                                                     (make-before-after-result (append finished-exprs before) `(,highlight-placeholder) `(...)
                                                                               current reduct-list after)))])
                                          (set! held-expr-list no-sexp)
                                          (set! held-redex-list no-sexp)
                                          (send-to-drscheme-eventspace
                                           (lambda ()
                                             (receive-result result user-computation-semaphore)))
                                          (semaphore-wait user-computation-semaphore))))]
                       [(double-break)
                        ; a double-break occurs at the beginning of a let's evaluation.
                        (let* ([reconstruct-quadruple
                                (r:reconstruct-current current-expr mark-list break-kind returned-value-list)])
                          (when (not (eq? held-expr-list no-sexp))
                            (error 'break-reconstruction
                                   "held-expr-list not empty when a double-break occurred"))
                          (let*-values 
                              ([(new-finished current-pre current-post after) 
                                (double-redivide finished-exprs 
                                                 (list-ref reconstruct-quadruple 0) 
                                                 (list-ref reconstruct-quadruple 2))])
                            (send-to-drscheme-eventspace
                             (lambda () 
                               (receive-result (make-before-after-result new-finished
                                                                         current-pre
                                                                         (list-ref reconstruct-quadruple 1)
                                                                         current-post
                                                                         (list-ref reconstruct-quadruple 3)
                                                                         after)
                                               user-computation-semaphore)))
                            (semaphore-wait user-computation-semaphore)))]
                       [(late-let-break)
                        (let ([new-finished (car (r:reconstruct-current current-expr mark-list break-kind returned-value-list))])
                          (set! finished-exprs (append finished-exprs new-finished)))]
                       [else (error 'break "unknown label on break")])))
           
           (define (step-through-expression expanded expand-next-expression)
             (let*-values ([(annotated envs) (a:annotate expanded packaged-envs break 
                                                         'foot-wrap)])
               (set! packaged-envs envs)
               (set! current-expr expanded)
               (let ([expression-result
                      (parameterize ([current-eval basic-eval])
                        (eval annotated))])
                 (add-finished-expr expression-result)
                 (expand-next-expression))))
           
           (define (add-finished-expr expression-result)
             (let ([reconstructed (r:reconstruct-completed current-expr expression-result)])
               (set! finished-exprs (append finished-exprs (list reconstructed)))))
           
           (define (err-display-handler message exn)
             (send-to-drscheme-eventspace
              (lambda ()
                (if (not (eq? held-expr-list no-sexp))
                    (let*-values
                        ([(before current after) (redivide held-expr-list)])
                      (receive-result (make-before-error-result (append finished-exprs before) 
                                                                current held-redex-list message after)
                                      user-computation-semaphore))
                    (receive-result (make-error-result finished-exprs message) user-computation-semaphore))))))
        
        (program-expander
         (lambda ()
           (error-display-handler err-display-handler)
           (r:set-render-settings! (get-render-settings))) ; init
         (lambda (expanded continue-thunk) ; iter
           (if (eof-object? expanded)
               (send-to-drscheme-eventspace 
                (lambda ()
                  (receive-result (make-finished-result finished-exprs) user-computation-semaphore)))
               (step-through-expression expanded continue-thunk))))))))

