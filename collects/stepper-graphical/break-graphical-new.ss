(unit/sig (break)
  (import mzlib:core^
          [m : mred^]
          [marks : stepper:marks^]
          [pc : mzlib:print-convert^]
          [pp : mzlib:pretty-print^]
          [z : zodiac:system^]
          [e : zodiac:interface^]
          [fw : framework^]
          [d : drscheme:export^]
          [utils : stepper:cogen-utils^])
  
  (define debugger-text%
    (class fw:text:basic% args
      (inherit dc-location-to-editor-location find-position)
      (rename [super-on-local-event on-local-event])
      
      (private [click-callback #f])
      (public [set-click-callback! 
               (lambda (callback)
                 (if (and (procedure? callback)
                          (procedure-arity-includes? callback 3))
                     (set! click-callback callback)
                     (e:internal-error #f "set-click-callback called with invalid argument")))])
      
      (override
        [on-local-event
         (lambda (event)
           (when (send event button-down? 'left)
             ;(printf "rec'd button-press~n")
             (when click-callback
               (let*-values ([(event-x) (send event get-x)]
                             [(event-y) (send event get-y)]
                             [(x y) (dc-location-to-editor-location
                                     event-x event-y)]
                             [(position) (find-position x y)])
                 (click-callback x y position)))))])
      
      (sequence (apply super-init args))))
  
  (fw:preferences:set-default 'debugger-width 400 number?)
  (fw:preferences:set-default 'debugger-height 800 number?)
  
  (define debugger-frame%
    (class (fw:frame:standard-menus-mixin fw:frame:basic%) (debugger)
      (rename [super-on-size on-size]
              [super-can-close? can-close?])
      (override
        [on-size
         (lambda (width height)
           (fw:preferences:set 'debugger-width width)
           (fw:preferences:set 'debugger-height height)
           (super-on-size width height))]
        
        [can-close?
         (lambda ()
           (and (send debugger can-close-frame?)
                (super-can-close?)))])
      (sequence (super-init "Debugger" #f
                            (fw:preferences:get 'debugger-width)
                            (fw:preferences:get 'debugger-height)))))
  
  (define drscheme-eventspace (m:current-eventspace))
  
  ; ----------------------------------------------------------------
  
  ; gui setup
  
  (define test-dc (make-object m:bitmap-dc% (make-object m:bitmap% 1 1)))
  (define debug-highlight-color (make-object m:color% 255 255 255))
  (send test-dc try-color (make-object m:color% 145 211 219) debug-highlight-color)

  ; ----------------------------------------------------------------
  
  (define-struct lbox-info (mark closure?))
  
  (define debugger%
    (class object% (drscheme-frame)

      (public [can-close-frame?
               (lambda ()
                 (if break-semaphore
                     (begin 
                       (m:message-box "oops!" (string-append "You cannot close the debugger window "
                                                             "while a breakpoint is waiting.")
                                      #f '(ok))
                       #f)
                     #t))]
              
              [on-close-frame
               (lambda ()
                 (send frame on-close)
                 (send frame show #f))])
      
      (private [parsed #f]
               [needs-update #t]
               [stored-mark-list #f]
               [text-region-table null] ; candidate for hash-table-ization, if speed is needed
               [clear-highlight-thunk #f]
               [stored-selection #f]

               [context-lbox-selection
                (lambda ()
                  (let ([selection-list (send context-lbox get-selections)])
                    (if (= (length selection-list) 1)
                        (car selection-list)
                        (error 'context-lbox-selection "~a items selected in lbox; should be 1" 
                               (length selection-list)))))]
               
               [click-callback
                (lambda (x y loc)
                  (for-each (lambda (entry)
                              (if (and (>= loc (car entry))
                                       (< loc (cadr entry)))
                                  (show-var-values (caddr entry) x y)))
                            text-region-table))]
               
               [continue-callback
                (lambda (a b)
                  (send frame show #f)
                  (semaphore-post break-semaphore)
                  (set! break-semaphore #f))]
               
               [listbox-callback
                (lambda (list-box event)
                  (if (= (length (send list-box get-selections)) 0) ; i.e., nothing selected 
                      (begin (send list-box select saved-selection)
                             (message-box "BAD user! No deselecting the current item." '(ok)))
                      (let* ([selection (context-lbox-selection)]
                             [lbox-info (send list-box get-data selection)]
                             [mark (lbox-info-mark lbox-info)]
                             [closure? (lbox-info-closure? lbox-info)]
                             [source (marks:mark-source mark)]
                             [source-file (z:location-file (z:zodiac-start source))]
                             [source-start (z:location-offset (z:zodiac-start source))]
                             [source-finish (z:location-offset (z:zodiac-finish source))])
                        (set! stored-selection selection) ; in case someone deselects it
                        (when clear-highlight-thunk 
                          (clear-highlight-thunk))
                        (if (eq? source-file (ivar drscheme-frame definitions-text))
                            (set! clear-highlight-thunk 
                                  (send defns-text highlight-range source-start (+ 1 source-finish) debug-highlight-color #f))
                            (m:message-box "source text is not in this buffer" '(ok)))
                        (clear-var-highlights)
                        (highlight-vars mark closure?))))]

               [show-var-values
                (lambda (binding x y)
                  (let ([pm (make-object m:popup-menu%)])
                    (if (z:top-level-varref? binding) ; this will only include truly-top-level vars
                        (add-to-popup pm (global-defined-value (z:varref-var binding)) 'highlighted) ; hack to fix namespace?
                        (let*-values ([(mark) (send context-lbox get-data (context-lbox-selection))]
                                      [(rev-before after)
                                       (let loop ([remaining stored-mark-list] [building null])
                                         (cond [(null? remaining)
                                                (error 'show-var-values "mark does not occur in stored list")]
                                               [(eq? (car remaining) mark)
                                                (values (cons (car remaining) building) (cdr remaining))]
                                               [else
                                                (loop (cdr remaining) (cons (car remaining) building))]))])
                          (let* ([extract-vals
                                  (lambda (mark-list)
                                    (map marks:mark-binding-value (marks:lookup-binding-list mark-list binding)))]
                                 [rev-before-vals
                                  (extract-vals rev-before)]
                                 [after-vals
                                  (extract-vals after)])
                            (for-each (lambda (val)
                                        (add-to-popup pm val))
                                      (reverse after-vals))
                            (add-to-popup pm (car rev-before-vals) 'highlighted)
                            (for-each (lambda (val)
                                        (add-to-popup pm val))
                                      rev-before-vals))))
                    (send frame popup-menu pm (inexact->exact x) (inexact->exact y))))]
               
                 [add-to-popup
                  (lambda (pm val . options)
                    (cond [(procedure? val)
                           (make-object m:menu-item% "(closure)" pm (make-display-closure val))]
                          [(struct? val)
                           (make-object m:menu-item% "(structure)" pm void)]
                          [(class? val)
                           (make-object m:menu-item% "(class)" pm void)]
                          [(object? val)
                           (make-object m:menu-item% "(object)" pm void)]
                          [(unit? val)
                           (make-object m:menu-item% "(unit)" pm void)]
                          [else
                           (let ([sp (open-output-string)])
                             (write (pc:print-convert val) sp)
                             (if (memq 'highlighted options)
                                 (let ([item (make-object m:checkable-menu-item% (get-output-string sp) pm void)])
                                   (send item check #t))
                                 (make-object m:menu-item% (get-output-string sp) pm void)))]))]
                 
                 [make-display-closure
                  (lambda (closure)
                    (lambda (menu event)
                      ([
                    ; whoops!  need to get into the closure table
                    )]
                 
               
                 [clear-var-highlights
                  (lambda ()
                    (for-each (lambda (entry)
                                (send defns-text change-style 
                                      (send (send defns-text get-style-list) find-named-style "Standard")
                                      (car entry) (cadr entry)))
                              text-region-table)
                    (set! text-region-table null))]
                 
                 [zodiac-abbr
                  (lambda (src)
                    (ccond ; we need a z:parsed iterator...
                     [(z:varref? src)
                      "<varref>"]
                     [(z:app? src)
                      "<application>"]
                     [(z:struct-form? src)
                      "<struct-form>"]
                     [(z:if-form? src)
                      "(if <test> <then> <else>)"]
                     [(z:quote-form? src)
                      "(quote <val>)"]
                     [(z:begin-form? src)
                      "(begin <bodies>)"]
                     [(z:begin0-form? src)
                      "(begin0 <bodies>)"]
                     [(z:let-values-form? src)
                      "(let-values ([(<vars>) ... ] ...) <body>)"]
                     [(z:letrec-values-form? src)
                      "(letrec-values ([(<vars>) ...] ...) <body>)"]
                     [(z:define-values-form? src)
                      "(define-values (<vars>) <bodies>)"]
                     [(z:set!-form? src)
                      "(set! <var> <body>)"]
                     [(z:case-lambda-form? src)
                      "(case-lambda ((<args>) <bodies>) ...)"]
                     [(z:with-continuation-mark-form? src)
                      "(with-continuation-mark <key> <mark> <body>)"]
                     [(z:unit-form? src)
                      "(unit <imports> <exports> <clauses>)"]
                     [(z:compound-unit-form? src)
                      "(compound-unit <imports> <links> <exports>)"]
                     [(z:invoke-unit-form? src)
                      "(invoke-unit ...)"]
                     [(z:interface-form? src)
                      "(interface ...)"]
                     [(z:class*/names-form? src)
                      "(class*/names ...)"]))]
                  
               ; highlight-vars 
                  
               [highlight-vars
                (lambda (mark closure?)
                  (let* ([src (marks:mark-source mark)]
                         [mark-bindings (if closure?
                                            (apply append
                                                   (map (lambda (mark)
                                                          (map marks:mark-binding-binding
                                                               (marks:mark-bindings mark)))
                                                        stored-mark-list))
                                            (map marks:mark-binding-binding (marks:mark-bindings mark)))]
                         [highlight-var ; (parsed [binding | slot | top-level-varref] -> (void))
                          (lambda (ref binding)
                            (let* ([start (z:location-offset (z:zodiac-start ref))]
                                   [finish (+ (z:location-offset (z:zodiac-finish ref)) 1)])
                              (send defns-text change-style ub-delta start finish)
                              (set! text-region-table (cons (list start finish binding) text-region-table))))]
                         [maybe-highlight-bound-var
                          (lambda (ref binding)
                            (when (memq binding mark-bindings)
                              (highlight-var ref binding)))])
                    (let recur ([src src])
                      (ccond ; we need a z:parsed iterator...
                        [(z:varref? src)
                         (if (z:top-level-varref? src)
                             (if (utils:is-unit-bound? src)
                                 (maybe-highlight-bound-var src (z:top-level-varref/bind-slot src))
                                 (highlight-var src src))
                             (maybe-highlight-bound-var src (z:bound-varref-binding src)))]
                        [(z:app? src)
                         (map recur (cons (z:app-fun src) (z:app-args src)))]
                        [(z:struct-form? src)
                         (when (z:struct-form-super src)
                           (recur (z:struct-form-super src)))]
                        [(z:if-form? src)
                         (for-each recur (list (z:if-form-test src)
                                               (z:if-form-then src)
                                               (z:if-form-else src)))]
                        [(z:quote-form? src)
                         (void)]
                        [(z:begin-form? src)
                         (for-each recur (z:begin-form-bodies src))]
                        [(z:begin0-form? src)
                         (for-each recur (z:begin0-form-bodies src))]
                        [(z:let-values-form? src)
                         (for-each (lambda (binding-list) ; I believe these should never be highlighted
                                     (for-each (lambda (binding)
                                                 (maybe-highlight-bound-var binding binding))
                                               binding-list))
                                   (z:let-values-form-vars src))
                         (for-each recur (z:let-values-form-vals src))
                         (recur (z:let-values-form-body src))]
                        [(z:letrec-values-form? src)
                         (for-each (lambda (binding-list) ; or these either, or any binding instances, to be honest.
                                     (for-each (lambda (binding)
                                                 (maybe-highlight-bound-var binding binding))
                                               binding-list))
                                   (z:letrec-values-form-vars src))
                         (for-each recur (z:let-values-form-vals src))
                         (recur (z:let-values-form-body src))]
                        [(z:define-values-form? src)
                         (for-each recur (z:define-values-form-vars src))
                         (recur (z:define-values-form-val src))]
                        [(z:set!-form? src)
                         (recur (z:set!-form-var src))
                         (recur (z:set!-form-val src))]
                        [(z:case-lambda-form? src)
                         (for-each 
                          (lambda (arglist)
                            (for-each (lambda (x) (maybe-highlight-bound-var x x)) 
                                      (z:arglist-vars arglist)))
                          (z:case-lambda-form-args src))
                         (for-each recur (z:case-lambda-form-bodies src))]
                        [(z:with-continuation-mark-form? src)
                         (recur (z:with-continuation-mark-form-key src))
                         (recur (z:with-continuation-mark-form-val src))
                         (recur (z:with-continuation-mark-form-body src))]
                        [(z:unit-form? src)
                         ; imports and exports can never be highlighted, same as lambda and let bindings
                         (for-each recur (z:unit-form-clauses src))]
                        [(z:compound-unit-form? src)
                         ; imports and exports not highlighted as with units
                         (for-each (lambda (link-clause) (recur (cadr link-clause)))
                                   (z:compound-unit-form-links src))]
                        [(z:invoke-unit-form? src)
                         (for-each recur (z:invoke-unit-form-variables src))
                         (recur (z:invoke-unit-form-unit src))]
                        [(z:interface-form? src)
                         (for-each recur (z:interface-form-super-exprs src))]
                        [(z:class*/names-form? src)
                         (recur (z:class*/names-form-super-expr src))
                         (for-each recur (z:class*/names-form-interfaces src))
                         (for-each (lambda (arg)
                                     (when (pair? arg)
                                       (recur (cdr arg))))
                                   (z:paroptarglist-vars (z:class*/names-form-init-vars src)))
                         (for-each (lambda (clause)
                                     (ccond [(z:public-clause? src)
                                             (for-each recur (z:public-clause-exprs src))]
                                            [(z:override-clause? src)
                                             (for-each recur (z:override-clause-exprs src))]
                                            [(z:private-clause? src)
                                             (for-each recur (z:private-clause-exprs src))]
                                            [(z:inherit-clause? src)
                                             (void)]
                                            [(z:rename-clause? src)
                                             (void)]
                                            [(z:sequence-clause? src)
                                             (for-each recur (z:sequence-clause-exprs src))]))
                                   (z:class*/names-form-inst-clauses src))]))))])

      (public [handle-breakpoint
               (lambda (mark-list semaphore)
                 (set! break-semaphore semaphore)
                 (set! stored-mark-list mark-list)
                 (if needs-update
                     (begin
                       (send defns-text lock #f)
                       (send defns-text erase)
                       (send (ivar drscheme-frame definitions-text) copy-self-to defns-text)
                       (send defns-text lock #t)
                       ;(set! needs-update #f) ; no mechanism currently for flagging updates to window
                       )
                     (begin
                       (clear-var-highlights)
                       (when clear-highlight-thunk
                         (clear-highlight-thunk))))
                 (send context-lbox clear)
                 (for-each 
                  (lambda (mark)
                    ; will show even stale marks
                    (send context-lbox append (zodiac-abbr (marks:mark-source mark)) (make-lbox-info mark #f)))
                  (reverse mark-list))
                 (let ([last-element (- (send context-lbox get-number) 1)])
                   (send context-lbox select last-element)
                   (set! stored-selection last-element))
                 (listbox-callback context-lbox 'bogus-event)
                 (send frame show #t))])
                 
      (sequence (super-init))
      
      (private [frame (make-object debugger-frame% this)]
               [area-container (send frame get-area-container)]
               [control-panel (make-object m:horizontal-panel% area-container)]
               [context-lbox (make-object m:list-box% #f null control-panel listbox-callback)]
               [defns-canvas (make-object m:editor-canvas% area-container)]
               [defns-text (make-object debugger-text%)]
               [standard-style (send (send defns-text get-style-list) find-named-style "Standard")]
               [ub-delta (let* ([style-list (send defns-text get-style-list)]
                                [underline-delta (make-object m:style-delta% 'change-underline #t)])
                           (send underline-delta set-delta-foreground "blue"))]
               ;[var-style (let* ([style-list (send defns-text get-style-list)]
               ;                  [underline-delta (make-object m:style-delta% 'change-underline #t)]
               ;                  [underline-blue-delta (send underline-delta set-delta-foreground "blue")])
               ;             (send style-list find-or-create-style standard-style underline-blue-delta))]
               [break-semaphore #f])
      
      (sequence (send defns-text set-click-callback! click-callback)
                (send defns-text hide-caret #t)
                (send control-panel stretchable-height #f)
                (make-object m:button% "continue" control-panel continue-callback)
                (send defns-canvas set-editor defns-text))))
  
  
  
  ; add the get-debugger-frame and set-debugger-frame! methods to the drscheme frame
  (d:get/extend:extend-unit-frame
   (lambda (super%)
     (class super% args
       (sequence (apply super-init args))
       (rename [super-can-close? can-close?]
               [super-on-close on-close])
       (private [debugger #f])
       (public
         [get-debugger
          (lambda ()
            (when (not debugger)
              (set! debugger (make-object debugger% this)))
            debugger)])
       (override
         [can-close?
          (lambda ()
            (and (if debugger
                     (send debugger can-close-frame?)
                     #t)
                 (super-can-close?)))]
         [on-close
          (lambda ()
            (when debugger
              (send debugger on-close-frame))
            (super-on-close))]))))
  
  ; ----------------------------------------------------------------
  
  ; set up debugger preferences panel
  
  (fw:preferences:set-default 'ankle-annotation #f boolean?)
  
  ;link to parameter
  (marks:ankle-wrap-enabled (fw:preferences:get 'ankle-annotation))
  (fw:preferences:add-callback 'ankle-annotation
                               (lambda (pref-name val)
                                 (marks:ankle-wrap-enabled val)
                                 #t))
  
  
  (define (ankle-pref-callback cbox event)
    (case (send event get-event-type)
      ((check-box)
       (let ([setting (send cbox get-value)])
         (fw:preferences:set 'ankle-annotation setting)))))
  
  (define (create-debugger-prefs-panel parent)
    (local
        ((define cbox-panel (make-object m:vertical-panel% parent))
         (define debugger-checkbox (make-object m:check-box% "Enable Lightweight Debugger" cbox-panel 
                                     ankle-pref-callback null)))
      (send debugger-checkbox set-value (fw:preferences:get 'ankle-annotation))
      cbox-panel))
  
  (fw:preferences:add-panel "debugging" create-debugger-prefs-panel)
  
  ; ----------------------------------------------------------------
  
  (define (is-drscheme-definitions-editor? editor)
    (and (is-a? editor m:editor<%>)
         (send editor get-canvas)
         (is-a? (send editor get-canvas) d:unit:definitions-canvas%)))
  
  (define (get-drscheme-frame editor)
    (and (is-drscheme-definitions-editor? editor)
         (send (send editor get-canvas) get-top-level-window)))
  
  (define (break)
    (let* ([mark-list (marks:extract-mark-list (current-continuation-marks))]
           [new-semaphore (make-semaphore)])
      (when (null? mark-list)
        (error 'breakpoint "no marks to debug (could be in No Debugging mode?)"))
      (parameterize
          ([m:current-eventspace drscheme-eventspace])
        (m:queue-callback 
         (lambda ()
           (let* ([drscheme-frame (let loop ([mark-list mark-list])
                                    (if (null? mark-list)
                                        (error "no drscheme marks to hang the debugger from")
                                        (or (get-drscheme-frame (z:location-file
                                                                 (z:zodiac-start
                                                                  (marks:mark-source (car mark-list)))))
                                            (loop (cdr mark-list)))))]
                  [debugger (send drscheme-frame get-debugger)])
             (send debugger handle-breakpoint mark-list new-semaphore))))
        (semaphore-wait new-semaphore)
        (void)))))
