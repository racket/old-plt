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
           (send debugger on-size-frame)
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
  (define clear-highlight-thunk #f)
  
  ; ----------------------------------------------------------------
  
  (define (add-to-popup pm val)
    (cond [(procedure? val)
           (make-object m:menu-item% "(closure)" pm (void))]
          [(struct? val)
           (make-object m:menu-item% "(structure)" pm (void))]
          [(class? val)
           (make-object m:menu-item% "(class)" pm (void))]
          [(object? val)
           (make-object m:menu-item% "(object)" pm (void))]
          [(unit? val)
           (make-object m:menu-item% "(unit)" pm (void))]
          [else
           (let ([sp (open-output-string)])
             (write (pc:print-convert val) sp)
             (make-object m:menu-item% (get-output-string sp) pm (void)))]))

  (define debugger%
    (class object% (drscheme-frame)

      (private [parsed #f]
               [needs-update #t]
               [stored-mark-list #f]
               [text-region-table null] ; candidate for hash-table-ization, if speed is needed

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
                  (semaphore-post break-semaphore))]
                
               [show-var-values
                (lambda (binding x y)
                  (let* ([values (marks:lookup-binding-list stored-mark-list binding)]
                         [pm (make-object m:popup-menu%)])
                    (for-each (lambda (val)
                                (add-to-popup pm val))
                              values)
                    (send frame popup-menu x y)))]
               
               [clear-highlights
                (lambda ()
                  (for-each (lambda (entry)
                              (send defns-text change-style standard-style (car entry) (cadr entry)))
                            text-region-table))]
               
               ; highlight-vars 
                  
               [highlight-vars
                (lambda (mark)
                  (let* ([src (marks:mark-source mark)]
                         [mark-bindings (map marks:mark-binding-binding (marks:mark-bindings mark))]
                         [highlight-var ; (parsed [binding | slot | top-level-varref] -> (void))
                          (lambda (ref binding)
                            (let* ([start (z:location-offset (z:zodiac-start ref))]
                                   [finish (z:location-offset (z:zodiac-finish ref))])
                              (send defns-text change-style var-style start finish)
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

      (public [set-zodiac!
               (lambda (new-parsed)
                 (if (z:parsed? new-parsed)
                     (begin
                       (set! parsed new-parsed)
                       (set! needs-update #t)
                       (set! text-region-table null))
                     (e:internal-error new-parsed "not a parsed zodiac value")))]
              
              [handle-breakpoint
               (lambda (mark-list semaphore)
                 (printf "entering handle-breakpoint~n")
                 (set! break-semaphore semaphore)
                 (set! stored-mark-list mark-list)
                 (if needs-update
                     (begin
                       (send defns-text clear)
                       (send (ivar drscheme-frame definitions-text) copy-self-to defns-text))
                     (clear-highlights))
                 (for-each highlight-vars mark-list)
                 (send frame show #t))])
                 
      (sequence (printf "initializing~n"))
      (sequence (super-init))
      
      (private [frame (make-object debugger-frame% this)]
               [area-container (send frame get-area-container)]
               [button-panel (make-object m:horizontal-panel% area-container)]
               [defns-canvas (make-object m:editor-canvas% area-container)]
               [defns-text (make-object debugger-text%)]
               [standard-style (send (send defns-text get-style-list) find-named-style "Standard")]
               [var-style (let* ([style-list (send defns-text get-style-list)]
                                 [underline-delta (make-object m:style-delta% 'change-underline #t)]
                                 [underline-blue-delta (send underline-delta set-delta-foreground "blue")])
                            (send style-list find-or-create-style standard-style underline-blue-delta))]
               [break-semaphore #f])
      
      (sequence (send defns-text set-click-callback! click-callback)
                (send button-panel stretchable-height #f)
                (make-object m:button% "continue" button-panel continue-callback)
                (send defns-canvas set-editor defns-text))
      
      (public [can-close-frame?
               (lambda ()
                 (if break-semaphore
                     (begin 
                       (m:message-box "oops!" (string-append "You cannot close the debugger window "
                                                             "while a breakpoint is waiting.")
                                      #f '(ok))
                       #f)
                     #t))])))
  
  
  
  ; add the get-debugger-frame and set-debugger-frame! methods to the drscheme frame
  (d:get/extend:extend-unit-frame
   (lambda (super%)
     (class super% args
       (sequence (apply super-init args))
       (private [debugger #f])
       (public
         [get-debugger
          (lambda ()
            (when (not debugger)
              (set! debugger (make-object debugger% this)))
            debugger)]))))
  
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
