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
  
  (define (online-error-checking-definitions-text-mixin dt% drscheme:eval:expand-program drscheme:language:make-text/pos)
    (class (expansion-tool-definitions-text-mixin (shared-mixin dt%))
      
      ;; from the shared-mixin
      (inherit kill-autocompile-thread set-autocompile-thread-proc! make-syntax-error-handler)
      
      ;; from the expansion-tool-definitions-text-mixin
      (inherit buffer-directory)
      (inherit-field latest-expansion)
      
      ;; from DrScheme's definitions text
      (inherit get-text get-next-settings)
      
      (super-new)

      (define/augment (after-insert start len)
        (after-change))
      
      (define/augment (after-delete start len)
        (after-change))

      (define/private (after-change)
        (kill-autocompile-thread)
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
        (define text (get-text))
        (define text/pos (drscheme:language:make-text/pos this
                                                          0 (string-length text)))
        (define these-expansions null)
        (drscheme:eval:expand-program
         text/pos
         (get-next-settings)
         #t
         (lambda () ;; init, set error handlers, current directory
           (define old-handler (current-exception-handler))
           (current-directory (buffer-directory))
           (current-exception-handler
            (lambda (exn)
              (cond
                [(exn:fail:syntax? exn)
                 (begin ((make-syntax-error-handler this) exn)
                        ((error-escape-handler)))]
                ;; ignore read errors from mismatched parens, etc
                [(exn:fail:read? exn) (when-debugging
                                       (printf "oops, read error ~a: ~a~n" exn (exn-message exn)))
                                      ((error-escape-handler))] 
                ;; TODO: handle this error: (require (lib "oops.ss"))
                [(exn:fail:filesystem? exn) (when-debugging
                                             (printf "oops, fs error ~a: ~a~n" exn (exn-message exn)))
                                            ((error-escape-handler))] 
                ;; contract errors show up when expanding (module foo BADLANGUAGE)
                [(exn:fail:contract? exn) (when-debugging
                                           (printf "oops, contract error ~a: ~a~n" exn (exn-message exn)))
                                          ((error-escape-handler))] 
                [else (begin (when-debugging
                              (printf "oops, other error ~a: ~a~n" exn (exn-message exn)))
                             (old-handler exn))]))))
         void
         (lambda (stx-obj continue)
           ;; TODO: is this a good idea? is this the right thread?
           ;(set-autocompile-thread (current-thread))
           (if (eof-object? stx-obj)
               (set! latest-expansion (if (= 1 (length these-expansions))
                                          (car these-expansions)
                                          #`(begin #,@these-expansions)))
               (begin (set! these-expansions (cons stx-obj these-expansions))
                      (continue))))))
      
      
      
      ))
  
  
  )
