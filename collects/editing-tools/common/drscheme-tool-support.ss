(module drscheme-tool-support mzscheme
  (require (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "etc.ss")
           (lib "framework.ss" "framework"))
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
  
  (define red (make-object color% 255 0 0))
  (define red-delta (make-object style-delta% 'change-nothing))
  (send red-delta set-delta-foreground red)

  (define shared-interface<%>
    (interface () 
      make-syntax-error-handler kill-autocompile-thread set-autocompile-thread-proc!))
  
  (define (shared-mixin dt%)
    (if (implementation? dt% shared-interface<%>)
        dt%
        (class* dt% (shared-interface<%>)
          (super-new)
          
          (inherit change-style set-position get-canvas find-snip get-snip-position locked-for-flow? locked-for-write? is-locked? freeze-colorer thaw-colorer)
          
          (field [last-error-canvas #f])
          
          (define default-canvas-background (preferences:get 'framework:basic-canvas-background))
          
          (define/public (clear-error-canvas)
            (when (and last-error-canvas
                       (= (send (send last-error-canvas get-canvas-background) red) 255))
              ;; recolor the previously bad text
              (freeze-colorer)
              (thaw-colorer #t #f)
              (send last-error-canvas set-canvas-background
                    default-canvas-background)))
          
          (define/override (on-focus on?)
            (super on-focus on?)
            (when (and on? (not last-error-canvas))
              (build-last-error-panel)))
          
          (define/private (build-last-error-panel)
            (let* ([editor-canvas (get-canvas)]
                   [frame (and editor-canvas (send editor-canvas get-top-level-window))]
                   [info-panel (and frame (is-a? frame frame:info<%>) (send frame get-info-panel))])
              (when info-panel
                (set! last-error-canvas (new (class canvas%
                                               (super-new)
                                               (define/override (on-event evt)
                                                 (case (send evt get-event-type)
                                                   [(left-up)
                                                    (move-to-error)
                                                    (send editor-canvas focus)]
                                                   [(right-up)
                                                    (when last-error-string
                                                      (message-box "Syntax error" last-error-string))]
                                                   [else void])
                                                 (super on-event evt)))
                                             [parent info-panel]
                                             [min-width 20]
                                             [min-height 20]
                                             [stretchable-height #f]
                                             [stretchable-width #f]))
                (set! default-canvas-background (send last-error-canvas get-canvas-background)))))
          
          (define last-error-snip #f)
          (define last-error-string #f)
          
          ;; TODO: FIXME: don't I want "define/protected" here?
          (define/public (make-syntax-error-handler code-source)
            (lambda (exn)
              (parameterize ([current-eventspace autocompile-eventspace])
                (queue-callback
                 (lambda ()
                   (for-each (lambda (expr)
                               (when (or (eq? code-source (syntax-source expr))
                                         (and (filename? code-source) (filename? (syntax-source expr))
                                              (filename=? code-source (syntax-source expr))))
                                 ;; sub1 because text% buffers start at 0
                                 (let ([pos (sub1 (syntax-position expr))])
                                   (set! last-error-snip (find-snip pos 'after))
                                   ;; TODO: try not to break DrScheme's locked/unlocked contracts.
                                   (change-style red-delta
                                                 pos
                                                 (+ pos (syntax-span expr))
                                                 #f))))
                             (exn:fail:syntax-exprs exn))
                   (when last-error-canvas
                     (send last-error-canvas set-canvas-background red))
                   (set! last-error-string (exn-message exn))
                   (handle-failed-syntax exn))))))
          
          #|  Disabled until someone requests it (and suggest a better key combo)
      (define/override (on-char event)
        (if (and (eq? 'release (send event get-key-code))
                 (send event get-shift-down)
                 (let ([code (send event get-key-release-code)])
                   (and (char? code)
                        (char=? #\space code))))
            (move-to-error)
            (super on-char event)))
|#
          
          (define/private (move-to-error)
            (when (and last-error-snip
                       (not (locked-for-flow?)))
              (set-position (get-snip-position last-error-snip))))
          
          
          (define autocompile-custodian (make-custodian (current-custodian)))
          (define autocompile-thread-group (make-thread-group (make-thread-group (make-thread-group (make-thread-group)))))
          (define autocompile-eventspace (parameterize ([current-thread-group autocompile-thread-group])
                                           (make-eventspace)))
          (define autocompile-thread (thread void))
          
          ;; FIXME: ugh this should be protected
          (define/public (kill-autocompile-thread)
            (when-debugging 
             (when (eq? (current-thread) autocompile-thread)
               (printf "thread committing suicide??~n"))
             (when (eq? (current-custodian) autocompile-custodian)
               (printf "custodian committing suicide??~n")))
            ; (parameterize ([current-custodian autocompile-custodian])
            (kill-thread autocompile-thread)
            ;  )
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
            (kill-autocompile-thread)
            ;        (custodian-shutdown-all autocompile-custodian)
            (set! autocompile-thread (parameterize ([current-thread-group autocompile-thread-group]
                                                    ;[current-custodian autocompile-custodian]
                                                    )
                                       (thread thread-proc)))
            ;  (parameterize ([current-eventspace autocompile-eventspace])
            ;    (queue-callback thread-proc))
            ;(thread-proc)
            ;(set! shared-custodian custodian)
            )
          
          )))
  )
