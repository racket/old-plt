(module drscheme-tool-support mzscheme
  (require (lib "class.ss")
           (lib "mred.ss" "mred"))
  (provide shared-mixin when-debugging warn append-status)
    
  ;; enable or disable debugging code
  (define-syntax (when-debugging stx)
    (syntax-case stx ()
      [(_ exp ...)
       ;#'(let () exp ...)]))
       #''not-debugging]))
  
  (define warnings-frame (when-debugging (new frame% [label "AutoCompile status"])))
  (define warnings-field (when-debugging (new text-field%
                                              [parent warnings-frame]
                                              [label #f]
                                              [callback void]
                                              [init-value "AutoCompile is ready."]
                                              [style '(multiple)])))
  
  (define (set-status msg)
    (when-debugging
     (send warnings-field set-value msg)))
  
  (define (append-status msg)
    (when-debugging
     (show-status)
     (set-status (format "~a~n~a"
                         (send warnings-field get-value)
                         msg))))
  
  (define (hide-status)
    (when-debugging (send warnings-frame show #f)))
  (define (show-status)
    (when-debugging (send warnings-frame show #t)))
  
  (define (warn proc-name fmt . args)
    (when-debugging
     (append-status (format "autocompile error at ~a: ~a"
                            proc-name (apply format fmt args)))))
  
  (define (handle-failed-syntax exn)
    (when-debugging
     (define exprs (exn:fail:syntax-exprs exn))
     (for-each (lambda (expr)
                 (append-status "-----------")
                 (append-status (format "failed datum: ~v" (syntax-object->datum expr)))
                 (append-status (format "position: ~a" (syntax-position expr)))
                 (append-status (format "span: ~a" (syntax-span expr)))
                 (append-status (format "source module: ~a" (syntax-source-module expr)))
                 (append-status (format "source: ~a" (syntax-source expr)))
                 (append-status (format "line: ~a col: ~a" (syntax-line expr) (syntax-column expr))))
               exprs)))

  (define (filename? x)
    (or (string? x) (path? x)))

  (define (filename=? a b)
    (string=? (if (string? a) a (path->string a))
              (if (string? b) b (path->string b))))
 
  (define shared-interface<%>
    (interface () 
       make-syntax-error-handler kill-autocompile-thread set-autocompile-thread-proc!))

  (define (shared-mixin dt%)
    (if (implementation? dt% shared-interface<%>)
        dt%
    (class* dt% (shared-interface<%>)
      (super-new)
      
      (inherit change-style set-position)

      (define last-error-position 0)
      
      ;; TODO: FIXME: don't I want "define/protected" here?
      (define/public (make-syntax-error-handler code-source)
        (lambda (exn)
          (define delta (make-object style-delta% 'change-nothing))
          (send delta set-delta-foreground "red")
          (for-each (lambda (expr)
                      (when (or (eq? code-source (syntax-source expr))
                                (and (filename? code-source) (filename? (syntax-source expr))
                                     (filename=? code-source (syntax-source expr))))
                        ;; sub1 because text% buffers start at 0
                        (let ([pos (sub1 (syntax-position expr))])
                          (set! last-error-position pos)
                          ;; TODO: try not to break DrScheme's locked/unlocked contracts.
                          (change-style delta
                                        pos
                                        (+ pos (syntax-span expr))
                                        #f))))
                    (exn:fail:syntax-exprs exn))
          (set! delta #f)
          (handle-failed-syntax exn)))

      (define/override (on-char event)
        (if (and (eq? 'release (send event get-key-code))
                 (send event get-shift-down)
                 (let ([code (send event get-key-release-code)])
                   (and (char? code)
                        (char=? #\space code))))
            (move-to-error)
            (super on-char event)))

      (define/private (move-to-error)
        (set-position last-error-position))

      
      (define autocompile-thread-group (make-thread-group (make-thread-group (make-thread-group))))
      (define autocompile-thread (thread void))
      
      ;(define shared-custodian (make-custodian (current-custodian)))
      
      ;; FIXME: ugh this should be protected
      (define/public (kill-autocompile-thread)
        (kill-thread autocompile-thread)
        ;; TODO: FIXME: I think this is a hack.. 
        ;(with-handlers ([void (lambda (e)
        ;                        (printf "shutdown oops: ~a: ~a~n" e (exn-message e)))])
        ;(custodian-shutdown-all shared-custodian))
        )


      ;; FIXME: protect this..
      (define/public (set-autocompile-thread-proc! thread-proc)
        ;(define custodian (make-custodian (current-custodian)))
        ;(custodian-limit-memory custodian 5000 custodian)
#|
        (define exn-handler (current-exception-handler))
        (kill-thread autocompile-thread)
        (set! autocompile-thread (parameterize ([current-custodian shared-custodian]
                                                [current-exception-handler
                                                 (lambda (e)
                                                      ;; the custodian was shut down... so what :)
                                                   (if (exn:fail:contract? e)
                                                       ((error-escape-handler))
                                                       (exn-handler e)))]
;                                                [initial-exception-handler (lambda (e)
;                                                                             (printf "initial exn handler: ~a ~a~n" e (exn-message e)))]
;                                                [error-escape-handler (lambda ()
;                                                                        (printf "error escape handler~n" ))]
                                                )
                                   (thread (lambda ()
                                             (with-handlers ([void (lambda (e)
                                                                     (printf "thread oops: ~a: ~a~n" e (exn-message e)))])
                                               (thread-proc))))))
|#
  (set! autocompile-thread (parameterize ([current-thread-group autocompile-thread-group])
                              (thread thread-proc)))
;(thread-proc)
        ;(set! shared-custodian custodian)
        )
      
      )))
  )
