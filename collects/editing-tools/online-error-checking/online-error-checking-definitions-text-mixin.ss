(module online-error-checking-definitions-text-mixin mzscheme
  (provide online-error-checking-definitions-text-mixin)
  (require (lib "compiler.ss" "compiler")
           (lib "cm.ss")
           (lib "list.ss")
           (lib "plt-match.ss")
           (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "file.ss")
           (lib "etc.ss")
           
           "../common/drscheme-tool-support.ss"
           "../common/expansion-tool-definitions-text-mixin.ss"
           )
  
  (define orc<%> (interface () ))  
  
  (define (online-error-checking-definitions-text-mixin dt% drscheme:eval:expand-program drscheme:language:make-text/pos)
    (if (implementation? dt% orc<%>)
        dt%
        (class* (expansion-tool-definitions-text-mixin (shared-mixin dt%)) (orc<%>)
          
          ;; from the shared-mixin
          (inherit kill-autocompile-thread set-autocompile-thread-proc! make-syntax-error-handler clear-error-canvas)
          ;; from the expansion-tool-definitions-text-mixin
          (inherit buffer-directory)
          (inherit-field latest-expansion)
          
          
          ;; from DrScheme's definitions text
          (inherit get-text get-next-settings last-position get-character)
          
          (super-new)
          
          #|
      (define/augment (after-insert start len)
(printf "after-insert~n")
        (after-change))
      
      (define/augment (after-delete start len)
(printf "after-delete~n")
        (after-change))
|#
          
          
          (define/override (on-char event)
            (super on-char event)
            ;(printf "on-char: (~v ~v ~v)~n"
            ;        (send event get-key-code) (send event get-key-release-code) (send event get-time-stamp))
            (when (and (eq? 'release (send event get-key-code))
                       (let ([code (send event get-key-release-code)])
                         (or (eq? #\space code)
                             (eq? #\newline code)
                             (eq? #\return code)
                             (eq? #\) code)
                             (eq? #\] code)
                             (eq? #\} code))))
              (after-change)))
          
          (define/override (do-paste start time)
            (super do-paste start time)
            (after-change))
          
          (define/augment (after-delete start len)
            ;; >1 to ignore the parens matcher deleting things
            (unless (and (= 1 len)
                         (eq? #\) (get-character start)))
              (after-change)))
          
          (define/private (after-change)
            ;(kill-autocompile-thread)
            (set-autocompile-thread-proc!
             (lambda ()
               ;;  If we were only compiling modules, this would be the "drs-expand" code:
               #|  (parameterize ([current-directory (buffer-directory)])
               (with-handlers ([exn:fail:read? void] ;; ignore read errors from mismatched parens, etc
                               [exn:fail:filesystem? void] ;; TODO: handle this error: (require (lib "oops.ss"))
                               [exn:fail:syntax? (make-syntax-error-handler #f)])
                 (define port (open-input-string (get-text)))
                 (port-count-lines! port)
                 (set! latest-expansion (expand (read-syntax #f port))))) |#
               (drs-expand)
               )))
          
          (define/private (drs-expand)
            (define text/pos (drscheme:language:make-text/pos this
                                                                0
                                                                (last-position)))
            
            (define these-expansions null)
            (define (shutdown)
              (custodian-shutdown-all (current-custodian)))
            ;; sometimes by the time we're checking, the user deleted enough
            ;; characters to make our previous value of (last-position) invalid
           (when #t  ; nevermind.. I think changing after-delete fixed this
                   ; with-handlers ([exn:fail:contract?
                   ;         (lambda (e)
                   ;           (printf "drs-expand: exn before exanding: ~v: ~v~n"
                   ;                   e (exn-message e))
                   ;           (raise e))])
            (drscheme:eval:expand-program
             text/pos
             (get-next-settings)
             #t
             (lambda () ;; init, set error handlers, current directory
               (define old-handler (current-exception-handler))
               ;(printf "init~n")
               (current-directory (buffer-directory))
               (current-exception-handler
                (lambda (exn)
                  ;; TODO: print the exception in some status message
                 ; (printf "exn ~v: ~v~n" exn (exn-message exn))
               ;  (time
                  ;(kill-autocompile-thread)
                 ;      )
                  (cond
                    [(exn:fail:syntax? exn) ;(printf "syntax error~n")
                     (begin ((make-syntax-error-handler this) exn)
                            (shutdown))]
                    ;; ignore read errors from mismatched parens, etc
                    [(exn:fail:read? exn) (when-debugging
                                           (printf "oops, read error ~a: ~a~n" exn (exn-message exn)))
                                          (shutdown)] 
                    ;; TODO: handle this error: (require (lib "oops.ss"))
                    [(exn:fail:filesystem? exn) (when-debugging
                                                 (printf "oops, fs error ~a: ~a~n" exn (exn-message exn)))
                                                (shutdown)] 
                    ;; contract errors show up when expanding (module foo BADLANGUAGE)
                    [(exn:fail:contract? exn) (when-debugging
                                               (printf "oops, contract error ~a: ~a~n" exn (exn-message exn)))
                                              (shutdown)] 
                    [else (begin (when #t ;when-debugging
                                  (printf "oops, other error ~a: ~a~n" exn (exn-message exn)))
                                 (old-handler exn))]))))
             (lambda ()
               ;(printf "cleanup~n")
               (set! these-expansions #f)
               (set! text/pos #f))
             (lambda (stx-obj continue)
               ;(printf "expanded~n")
               (if (eof-object? stx-obj)
                   (begin 
                     ;(printf "finished expanding: ~v~n" (map syntax-object->datum these-expansions))
                     (set! latest-expansion (if (= 1 (length these-expansions))
                                                (car these-expansions)
                                                #`(begin #,@these-expansions)))
                     (clear-error-canvas)
                     (shutdown))
                   (begin (set! these-expansions (cons stx-obj these-expansions))
                          ;(printf "expanded more: ~v~n" (syntax-object->datum stx-obj))
                          (continue)))))))
          
          )))
  )
