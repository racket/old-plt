(module expand-program mzscheme
  
  (require
   (lib "class.ss")
   (lib "tool.ss" "drscheme")
   (lib "unitsig.ss")
   (lib "mred.ss" "mred")
   (lib "etc.ss")
   "signatures.ss")
  
  (provide expand-program@)
  
  (define oport (current-output-port))
  (define (oprintf . args) (apply fprintf oport args))
  
  (define expand-program@
    (unit/sig expand-program^
      (import drscheme:tool^)
      (define expand-program%
        (class object%
          
          (init-field
           language
           teachpacks
           error-handler
           clean-up
           (load-path #f))
          
          (field
           [drscheme-eventspace (current-eventspace)]
           [user-eventspace false]
           [user-custodian false]
           [user-thread false]
           [first-break? true]
           [done? false]
           [expand-program
            (drscheme:eval:traverse-program/multiple
             language
             (lambda () ; =user-eventspace=
               (current-load-relative-directory load-path)
               (error-display-handler error-handler)
               (set! user-thread (current-thread))
               (set! user-custodian (current-custodian))
               (set! user-eventspace (current-eventspace))
               (drscheme:teachpack:install-teachpacks
                (drscheme:teachpack:new-teachpack-cache
                 teachpacks)))
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
                    [else (error 'eval-file "Error")]))
                #t))))
          
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
          
          ;; eval-text ((is-a?/c text%) (any? . -> . void?) . -> . void?)
          ;; evaluate the text box and call the function with it's value
          (define/public (eval-text text next) ; =drscheme-eventspace=
            (eval-text/any text next eval (void)))
          
          ;; eval-text/values/ports ((is-a?/c text%) 
          ;;                         ((listof any?) . -> . void?)
          ;;                         (union port #f)
          ;;                         (union port #f)
          ;;                         ->
          ;;                         void?)
          (define/public (eval-text/values/ports text next out err)
            (eval-text/any
             text
             next
             (lambda (object)
               (call-with-values
                (lambda ()
                  (parameterize ([current-output-port out]
                                 [current-error-port err])
                    (eval object)))
                (lambda x x)))
             (list (void))))
          
          ;; eval-text/any ((is-a?/c text%) 
          ;;                (any? . -> . void?)
          ;;                (syntax? . -> . any?)
          ;;                . -> .
          ;;                void?)
          ;; evaluate the text box and call the function with the values of the box
          (define/public (eval-text/any text next do-eval blank) ; =drscheme-eventspace=
            (expand-program
             (drscheme:language:make-text/pos
              text 0 (send text last-position))
             (let ([value blank]
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
                          (set! value (do-eval object))
                          (set! first-expr? false)
                          (continue))
                        (error-handler
                         "Useless argument"
                         (make-exn:syntax "Too many expressions in a test box"
                                          (current-continuation-marks) object false false)))]
                   [else (error 'eval-text "Error")])))
             #f))
          
          ;; eval-text/multiple ((is-a?/c text%) (any? . -> . void?) . -> . void?)
          ;; evaluate the text box and call the function with it's value
          (define/public (eval-text/multiple text next) ; =drscheme-eventspace=
            (expand-program
             (drscheme:language:make-text/pos
              text 0 (send text last-position))
             (lambda (object continue) ; =user-eventspace=
               (cond
                 [(eof-object? object)
                  (queue-to-drscheme
                   (lambda () ; =drscheme-eventspace=
                     (next)))]
                 [(syntax? object)
                  (eval object)
                  (continue)]
                 [else (error 'eval-text "Error")]))
             #f))
          
          ;; eval-stx (syntax? (any? . -> . void?) . -> . void?)
          ;; evaluate the syntax in the users eventspace
          (define/public (eval-stx syntax-object next) ; =drscheme-eventspace=
            (queue-to-user
             (lambda () ; =user-eventspace=
               (let ([value (eval syntax-object)])
                 (queue-to-drscheme
                  (lambda () ; =drscheme-eventspace=
                    (next value)))))))

          ;; get-language (-> language?)
          ;; the language being used by the execution
          (define/public (get-language)
            language)
          
          ;; filename->text/pos (string? ((is-a?/c text%) . -> . void?) . -> . void?)
          ;; a text/pos containing the text from the given file
          (define/private (filename->text/pos filename next) ; =drscheme-eventspace=
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
          
          ;; make-ports : -> (values port port)
          ;; =drscheme-eventspace=
          (define/public (make-ports text show-text)
            (let* ([text-shown? #f]
                   [write-string-proc
                    (lambda (style) ;; =drscheme-eventspace=
                      (lambda (str start end ignore-me?) ;; =user-eventspace=
                        (let ([str-to-insert (substring str start end)])
                          (queue-to-drscheme
                           (lambda () ; =drscheme-eventspace=
                             (show-text)
                             (send text begin-edit-sequence)
                             (send text lock #f)
                             (let ([pos (send text last-position)])
                               (send text insert 
                                     str-to-insert
                                     pos
                                     pos)
                               (send text change-style style pos (send text last-position)))
                             (send text lock #t)
                             (send text end-edit-sequence))))
                        (- end start)))])
              (send text lock #t)
              (values
               (make-custom-output-port #f (write-string-proc stdout-style) void void)
               (make-custom-output-port #f (write-string-proc stderr-style) void void))))
          
          
          (super-instantiate ())
          ))
      
      (define stdout-style (make-object style-delta% 'change-bold))
      (send stdout-style set-delta-foreground "purple")
      (define stderr-style (make-object style-delta% 'change-italic))
      (send stderr-style set-delta-foreground "red")
      
      ;; kill-confirmation? (-> boolean?)
      ;; asks the user for confirmation to kill the thread and returns true on confirmation false otherwise
      (define (kill-confirmation?)
        (= 2 (message-box/custom "Kill?" "Do you want to kill the evaluation?" "Just break" "Kill" false)))
      ))
  )