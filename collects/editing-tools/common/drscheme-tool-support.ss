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
      
      (inherit change-style)
      
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
                          ;; TODO: try not to break DrScheme's locked/unlocked contracts.
                          (change-style delta
                                        pos
                                        (+ pos (syntax-span expr))
                                        #f))))
                    (exn:fail:syntax-exprs exn))
          (handle-failed-syntax exn)))
      
      (define autocompile-thread (thread void))
      
      ;; FIXME: ugh this should be protected
      (define/public (kill-autocompile-thread)
        (kill-thread autocompile-thread))
      
      ;; FIXME: protect this..
      (define/public (set-autocompile-thread-proc! thread-proc)
        (set! autocompile-thread (thread thread-proc)))
      
      )))
  )
