(module test-suite-tool mzscheme
  
  (require
   (lib "class.ss")
   (lib "etc.ss")
   (lib "mred.ss" "mred")
   (lib "framework.ss" "framework")
   (lib "unitsig.ss")
   (lib "tool.ss" "drscheme")
   "private/window/test-suite-window.ss")
  
  (provide tool@)
  
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      
      (define (phase1) (void))
      (define (phase2) (void))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Program Evaluation ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;
      
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
      
      ;; tools (symbol? . -> . proceedure?)
      ;; a hack inplace until I can reorganize the tool to use units
      (define (tools symbol)
        (cond
          [(symbol=? symbol 'preferences:get)
           preferences:get]
          [(symbol=? symbol 'drscheme:language-configuration:get-settings-preferences-symbol)
           drscheme:language-configuration:get-settings-preferences-symbol]
          [(symbol=? symbol 'drscheme:language-configuration:language-dialog)
           drscheme:language-configuration:language-dialog]
          [(symbol=? symbol 'preferences:set)
           preferences:set]
          [(symbol=? symbol 'expand-program%)
           expand-program%]
          ))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; DrScheme Frame Extensions ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ;; open-test-suite ((union string? false?) . -> . void?)
      ;; opens a test-suite window
      (define (open-test-suite filename)
        (let ([window
               (instantiate test-suite-window% ()
                 (tools tools))])
          (send window show true)
          (when filename (send window load-file filename))))
      
      ;; new-callback ((is-a?/c menu%) (is-a?/c control-event%) . -> . void?)
      ;; create a new test suite window
      (define (new-callback menu event)
        (open-test-suite false))
      
      ;; open-callback ((is-a?/c menu%) (is-a?/c control-event%) . -> . void?)
      ;; open an existing test suite file
      (define (open-callback menu event)
        (let ([file (get-file)])
          (when file
            (open-test-suite file))))
      
      ;; test-suite:frame-basics-mixin mixin-contract?
      ;; a new frame with an open and new file menu option for test-suites
      (define (test-suite:frame-basics-mixin super%)
        (class super%
          
          ;; file-menu:between-new-and-open ((is-a?/c file-menu%) . -> . void?)
          ;; called when contructing the menu between new and open
          (rename [super-file-menu:between-new-and-open
                   file-menu:between-new-and-open])
          (define/override (file-menu:between-new-and-open file-menu)
            (new-menu-item file-menu)
            (super-file-menu:between-new-and-open file-menu))
          
          ;; file-menu:between-open-and-revert ((is-a?/c file-menu%) . -> . void?)
          ;; called when contructing the menu between open and revert
          (rename [super-file-menu:between-open-and-revert
                   file-menu:between-open-and-revert])
          (define/override (file-menu:between-open-and-revert file-menu)
            (open-menu-item file-menu)
            (super-file-menu:between-open-and-revert file-menu))
          
          (super-instantiate ())
          ))
      
      ;; new-menu-item ((union (is-a?/c menu%) (is-a?/c popup-menu%) . -> . void?)
      ;; makes a new test suite window
      (define (new-menu-item parent)
        (instantiate menu-item% ()
          (label "New Test Suite")
          (parent parent)
          (callback new-callback)))
      
      ;; open-menu-item ((union (is-a?/c menu%) (is-a?/c popup-menu%) . -> . void?)
      ;; opens a test suite in a window
      (define (open-menu-item parent)
        (instantiate menu-item% ()
          (label "Open Test Suite...")
          (parent parent)
          (callback open-callback)))
      
      (drscheme:get/extend:extend-unit-frame test-suite:frame-basics-mixin)
      ))
  )
