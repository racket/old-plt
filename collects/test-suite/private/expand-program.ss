(module expand-program mzscheme
  
  (require
   (lib "class.ss")
   (lib "tool.ss" "drscheme")
   (lib "unitsig.ss")
   (lib "mred.ss" "mred")
   (lib "etc.ss")
   "signatures.ss")
  
  (provide expand-program@)
  
  (define expand-program@
    (unit/sig expand-program^
      (import drscheme:tool^)
      (define expand-program%
        (class object%
          
          (init-field
           language
           error-handler
           clean-up)
          
          (field
           [drscheme-eventspace (current-eventspace)]
           [user-eventspace false]
           [user-custodian false]
           [user-thread false]
           [first-break? true]
           [done? false]
           [expand-program
            (drscheme:eval:expand-program/multiple
             language
             false
             (lambda () ; =user-eventspace=
               (error-display-handler error-handler)
               (set! user-thread (current-thread))
               (set! user-custodian (current-custodian))
               (set! user-eventspace (current-eventspace)))
             (lambda () ; =a seperate eventspace=
               (queue-to-drscheme clean-up)))])
          
          ;; eval-file (string? (-> void?) . -> . void?)
          ;; evaluate the fileport's code into the user's eventspace
          (define/public (eval-file filename next) ; =drscheme-eventspace=
            (filename->text/pos
             filename
             (lambda (text) ; =drscheme-eventspace=
               (expand-program
                text
                (lambda (object continue) ; =user-eventspace=
                  (cond
                    [(eof-object? object) (next)]
                    [(syntax? object)
                     (eval object)
                     (continue)]
                    [else (error 'eval-file "Error")]))))))
          
          ;; break (-> void?)
          ;; break the test execution. if called more than once in an execution... kill the test.
          (define/public (break) ; =drscheme-eventspace=
            (when (thread-running? user-thread)
              (if (not done?)
                  (if first-break?
                      (begin
                        (set! first-break? false)
                        (break-thread user-thread))
                      (when (kill-confirmation?)
                        (custodian-shutdown-all user-custodian)))
                  (custodian-shutdown-all user-custodian))))
          
          ;; expand-text ((is-a?/c text%) (any? . -> . void?) . -> . void?)
          ;; evaluate the text box and call the function with it's value
          (define/public (expand-text text next) ; =drscheme-eventspace=
            (expand-program
             (drscheme:language:make-text/pos
              text 0 (send text last-position))
             (let ([value (void)]
                   [first-expr? true])
               (lambda (object continue) ; =user-eventspace=
                 (cond
                   [(eof-object? object)
                    (queue-to-drscheme
                     (lambda () ; =drscheme-eventspace=
                       (next value)))]
                   [(syntax? object)
                    (if first-expr?
                        (begin
                          (set! value object)
                          (set! first-expr? false)
                          (continue))
                        (error 'iter "Too many expressions in a test box"))]
                   [else (error 'eval-text "Error")])))))
          
          ;; eval-syntax (syntax? (any? . -> . void?) . -> . void?)
          ;; evaluate the syntax in the users eventspace
          (define/public (eval-syntax syntax-object next) ; =drscheme-eventspace=
            (queue-to-user
             (lambda () ; = user-eventspace=
               (let ([value (eval syntax-object)])
                 (queue-to-drscheme
                  (lambda () ; =drscheme-eventspace=
                    (next value)))))))
          
          ;; user-format (any? (string? . -> . void?) . -> . void?)
          ;; formats a value to a string using the users language printing style
          ;; status: this is wrong. i need to queue a callback here
          (define/public (user-format value next) ; =drscheme-eventspace=
            (queue-to-user
             (lambda () ; =user-eventspace=
               (let ([s (format "~v" value)])
                 (queue-to-drscheme
                  (lambda () ; =drscheme-eventspace=
                    (next s)))))))

          ;; filename->text/pos (string? ((is-a?/c text%) . -> . void?) . -> . void?)
          ;; a text/pos containing the text from the given file
          (define/private (filename->text/pos filename next) ; =drscheme-eventspace
            (queue-to-user
             (lambda () ;=user-eventspace=
               (let ([t (instantiate text% ())])
                 (send t load-file filename)
                 (next
                  (drscheme:language:make-text/pos
                   t 0 (send t last-position)))))))
          
          ;; done
          ;; sets the expander to completed with normal termination
          (define/public (done) ; =drscheme-eventspace=
            (set! done? true))
          
          ;; queue-to-drscheme ((-> void?) . -> . void?)
          ;; runs a callback on the drscheme eventspace
          (define/private (queue-to-drscheme thunk) ; =user-eventspace=
            (parameterize ([current-eventspace drscheme-eventspace])
              (queue-callback thunk)))
          
          ;; queue-to-user ((-> void?) . -> . void?)
          ;; runs a callback on the user eventspace
          (define/private (queue-to-user thunk) ; =drscheme-eventspace=
            (parameterize ([current-eventspace user-eventspace])
              (when (thread-running? user-thread)
                (queue-callback thunk))))
          
          (super-instantiate ())
          ))
      
      ;; kill-confirmation? (-> boolean?)
      ;; asks the user for confirmation to kill the thread and returns true on confirmation false otherwise
      (define (kill-confirmation?)
        (= 2 (message-box/custom "Kill?" "Do you want to kill the evaluation?" "Just break" "Kill" false)))
      ))
  )