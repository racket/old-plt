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
          (inherit with-lock/edit-sequence kill-autocompile-thread set-autocompile-thread-proc! make-syntax-error-handler clear-error-canvas pos-in-last-error-range?)
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
          
          (define/augment (after-insert start len)
            (when (or (> len 1)
                      (let ([char (get-character start)])
                        (or (eq? #\space char)
                            (eq? #\newline char)
                            (eq? #\return char)
                            (eq? #\) char)
                            (eq? #\] char)
                            (eq? #\} char)
                            (eq? #\; char)))
                      (pos-in-last-error-range? start))
              (after-change)))
;            (printf "after-insert ~a at ~a: ~v~n" len start (get-text start (+ start len))))
          
          (define/augment (after-delete start len)
            ;; to prevent out-of-range errors
            (kill-autocompile-thread)
            (when (or (> len 1)
                      (pos-in-last-error-range? start))
              (after-change)))
;            (printf "after-delete ~a at ~a: ~v~n" len start (get-text start (+ start len))))
          #|
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
          
          |#
          
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
          
          (define expander-custodian (make-custodian))
          (define expander-thread (parameterize ([current-custodian expander-custodian]) (thread void)))
          (define expander-shutdown void)
          (define/private (set-expander-thread&custodian t c s)
            ;; get rid of the old expansion
            (when (thread-running? expander-thread)
              (parameterize ([current-custodian expander-custodian])
                (kill-thread expander-thread))
              (custodian-shutdown-all expander-custodian)
              (expander-shutdown))
            (set! expander-shutdown s)
            (set! expander-custodian c)
            (set! expander-thread t))
          
          (define/private (drs-expand)
            (define text/pos (drscheme:language:make-text/pos this
                                                                0
                                                                (last-position)))
            
            (define these-expansions null)
;            (define c (current-custodian))
            (define (shutdown)
              ;; in case the GC is missing these...
              (set! these-expansions #f)
              (set! text/pos #f)
              (custodian-shutdown-all (current-custodian)))
            ;; sometimes by the time we're checking, the user deleted enough
            ;; characters to make our previous value of (last-position) invalid
           (when #t  ; nevermind.. I think changing after-delete fixed this
                   ; with-handlers ([exn:fail:contract?
                   ;         (lambda (e)
                   ;           (printf "drs-expand: exn before exanding: ~v: ~v~n"
                   ;                   e (exn-message e))
                   ;           (raise e))])
            ; (with-lock/edit-sequence
            ;  (lambda ()
            (drscheme:eval:expand-program
             text/pos
             (get-next-settings)
             #t
             (lambda () ;; init, set error handlers, current directory
               (define old-handler (current-exception-handler))
               (define expand-thread (current-thread))
               ;; only allow one expansion at a time.. so kill the old one
               ;; and store data about this new one
               (set-expander-thread&custodian expand-thread (current-custodian) shutdown)
               (current-directory (buffer-directory))
               (current-exception-handler
                (lambda (exn)
                  ;; TODO: print the exception in some status message
                  ;(printf "exn ~v: ~v~n" exn (exn-message exn))
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
