(unit/sig stepper:settings^
  (import [z : zodiac:system^]
          mzlib:pretty-print^
          mred^
          [d : drscheme:export^]
          [p : mzlib:print-convert^]
          [e : stepper:error^]
          [a : stepper:annotate^]
          [r : stepper:reconstruct^]
          [f : framework^]
          stepper:shared^)
  
  (define beginner-level-name "Beginner Student")
  
  (define (send-to-other-eventspace eventspace thunk)
    (parameterize ([current-eventspace eventspace])
      (queue-callback thunk)))
       
  ;;;;;; copied from /plt/collects/drscheme/snip.ss :
  
  (define separator-snipclass
    (make-object
     (class-asi snip-class%
       (override
         [read (lambda (s) 
                 (let ([size-box (box 0)])
                   (send s get size-box)
                   (make-object separator-snip%)))]))))
  
  (send* separator-snipclass
    (set-version 1)
    (set-classname "drscheme:separator-snip%"))
  
  (send (get-the-snip-class-list) add separator-snipclass)
  
  ;; the two numbers 1 and 2 which appear here are to line up this snip
  ;; with the embedded snips around it in the drscheme rep.
  ;; I have no idea where the extra pixels are going.
  (define separator-snip%
    (class snip% ()
      (inherit get-style set-snipclass set-flags get-flags get-admin)
      (private [width 500]
	       [height 1]
	       [white-around 2])
      (override
	[write (lambda (s) 
		 (send s put (char->integer #\r)))]
	[copy (lambda () 
		(let ([s (make-object separator-snip%)])
		  (send s set-style (get-style))
		  s))]
	[get-extent
	 (lambda (dc x y w-box h-box descent-box space-box lspace-box rspace-box)
	   (for-each (lambda (box) (unless (not box) (set-box! box 0)))
		     (list descent-box space-box lspace-box rspace-box))
	   (let* ([admin (get-admin)]
		  [reporting-media (send admin get-editor)]
		  [reporting-admin (send reporting-media get-admin)]
		  [widthb (box 0)]
		  [space 2])
	     (send reporting-admin get-view #f #f widthb #f)
	     (set! width (- (unbox widthb) 
			    space
			    2)))
	   (set! height 1)
	   (unless (not w-box)
	     (set-box! w-box width))
	   (unless (not h-box)
	     (set-box! h-box (+ (* 2 white-around) height))))]
	[draw
	 (let* ([body-pen (send the-pen-list find-or-create-pen
				"BLUE" 0 'solid)]
		[body-brush (send the-brush-list find-or-create-brush
				  "BLUE" 'solid)])
	   (lambda (dc x y left top right bottom dx dy draw-caret)
	     (let ([orig-pen (send dc get-pen)]
		   [orig-brush (send dc get-brush)])
	       (send dc set-pen body-pen)
	       (send dc set-brush body-brush)
	       
	       (send dc draw-rectangle (+ x 1)
		     (+ white-around y) width height)
	       
	       (send dc set-pen orig-pen)
	       (send dc set-brush orig-brush))))])
      (sequence
	(super-init)
	(set-flags (cons 'hard-newline (get-flags)))
	(set-snipclass separator-snipclass))))
  
  ;;;; end of copied region
  
  (define drscheme-eventspace (current-eventspace))

  (define (send-to-drscheme-eventspace thunk)
    (send-to-other-eventspace drscheme-eventspace thunk))
  
  (define par-constructor-style-printing #f)
  (define (constructor-style-printing?)
    par-constructor-style-printing)
  
  (define par-abbreviate-cons-as-list #f)
  (define (abbreviate-cons-as-list?)
    par-abbreviate-cons-as-list)
  
  (define par-cons #f)
  (define (user-cons? val)
    (eq? val par-cons))
  
  (define par-vector #f)
  (define (user-vector? val)
    (eq? val par-vector))
  
  (define user-pre-defined-vars #f)
  
  (define (check-pre-defined-var identifier)
    (memq identifier user-pre-defined-vars))
  
  (define user-namespace #f)

  (define (check-global-defined identifier)
    (with-handlers
        ([exn:variable? (lambda args #f)])
      (global-lookup identifier)
      #t))
  
  (define (global-lookup identifier)
    (parameterize ([current-namespace user-namespace])
      (global-defined-value identifier)))
    
  (define (image? val)
   (is-a? val snip%))
  
  (define print-convert #f)
  
  (define (setup-print-convert settings)
    (let ([print-convert-space (make-eventspace)]
          [print-convert-result #f]
          [print-convert-semaphore (make-semaphore)])
      (send-to-other-eventspace
       print-convert-space
       (lambda ()
         (d:basis:initialize-parameters (make-custodian) settings)
         (d:rep:invoke-library)
         (current-namespace user-namespace)
         (p:current-print-convert-hook 
          (lambda (v basic-convert sub-convert)
            (if (image? v)
                v
                (basic-convert v))))))
      (set! print-convert
            (lambda (val)
              (send-to-other-eventspace
               print-convert-space
               (lambda ()
                 (set! print-convert-result
                       (p:print-convert val))
                 (semaphore-post print-convert-semaphore)))
              (semaphore-wait print-convert-semaphore)
              print-convert-result))))

  (define finished-exprs #f)

  (define stepper-frame%
    (class (d:frame:basics-mixin (f:frame:standard-menus-mixin f:frame:basic%)) (drscheme-frame)
      (rename [super-on-close on-close])
      (override
        [on-close
         (lambda ()
           (send drscheme-frame stepper-frame #f)
           (super-on-close))])
      (sequence (super-init "The Foot"))))

  (define stepper-canvas%
    (class editor-canvas% (parent (editor #f) (style null) (scrolls-per-page 100))
      (rename (super-on-size on-size))
      (inherit get-editor)
      (override 
        [on-size 
         (lambda (width height)
           (super-on-size width height)
           (let ([editor (get-editor)])
             (when editor
               (send editor reset-pretty-print-width this))))])
      (sequence (super-init parent editor style scrolls-per-page))))
  
  (define stepper-text%
    (class f:text:basic% (pre-sexp pre-redex post-sexp post-redex break-kind error-msg (line-spacing 1.0) (tabstops null))
      (inherit find-snip insert change-style highlight-range last-position lock erase
               begin-edit-sequence end-edit-sequence get-start-position get-style-list set-style-list)
      (public (pretty-printed-width -1)
              (char-width 0)
              (clear-highlight-thunks null)
              (now-finished-exprs finished-exprs)
              [reset-style
               (lambda ()
                 (change-style (send (get-style-list) find-named-style "Standard")))]
              (reset-pretty-print-width 
               (lambda (canvas)
                 (begin-edit-sequence)
                 (let* ([style (send (get-style-list) find-named-style "Standard")]
                        [_ (set! char-width (send style get-text-width (send canvas get-dc)))]
                        [canvas-width (let-values ([(client-width client-height)
                                                    (send canvas get-client-size)])
                                        (- client-width 18))] ; 12 border pixels + 6 for wrap char
                        [min-columns 30]
                        [new-columns (max min-columns 
                                          (floor (/ canvas-width char-width)))])
                   (pretty-print-columns new-columns)
                   (reformat-sexp)
                   (end-edit-sequence))))
              (reformat-sexp
               (lambda ()
                 (when (not (= pretty-printed-width (pretty-print-columns)))
                   (set! pretty-printed-width (pretty-print-columns))
                   (format-whole-step))))
              [format-sexp
               (lambda (sexp redex highlight-color)
                 (let ([real-print-hook (pretty-print-print-hook)]
                       [redex-begin #f]
                       [redex-end #f]
                       [placeholder-present? #f])
                   (parameterize ([pretty-print-size-hook
                                   (lambda (value display? port)
                                     (if (eq? value highlight-placeholder)
                                         (begin
                                           (set! placeholder-present? #t)
                                           (string-length (format "~s" redex)))
                                         (if (image? value)
                                             1   ; if there was a good way to calculate a image widths ...
                                             #f)))]
                                  [pretty-print-print-hook
                                   (lambda (value display? port)
                                     (if (eq? value highlight-placeholder)
                                         (insert (format "~s" redex))
                                         ; next occurs if value is an image:
                                         (insert (send value copy))))]
                                  [pretty-print-display-string-handler
                                   (lambda (string port)
                                     (insert string))]
                                  [pretty-print-print-line
                                   (lambda (number port old-length dest-columns)
                                     (when (not (eq? number 0))
                                       (insert #\newline))
                                     0)]
                                  [pretty-print-pre-print-hook
                                   (lambda (value p)
                                     (when (or (and (not placeholder-present?)
                                                    (eq? value redex))
                                               (eq? value highlight-placeholder))
                                       (set! redex-begin (get-start-position))))]
                                  [pretty-print-post-print-hook
                                   (lambda (value p)
                                     (when (or (and (not placeholder-present?)
                                                    (eq? value redex))
                                               (eq? value highlight-placeholder))
                                       (set! redex-end (get-start-position))))])
                     (pretty-print sexp)
                     (if redex-begin
                         (set! clear-highlight-thunks
                               (cons (highlight-range redex-begin redex-end highlight-color #f #f)
                                     clear-highlight-thunks))))))]

              [format-whole-step
               (lambda ()
                 (lock #f)
                 (begin-edit-sequence)
                 (for-each (lambda (fun) (fun)) clear-highlight-thunks)
                 (set! clear-highlight-thunks null)
                 (erase)
                 (for-each
                  (lambda (expr)
                    (format-sexp expr no-sexp #f)
                    (insert #\newline))
                  now-finished-exprs)
                 (insert (make-object separator-snip%))
                 (when (not (eq? pre-sexp no-sexp))
                   (insert #\newline)
                   (reset-style)
                   (format-sexp pre-sexp pre-redex redex-highlight-color)
                   (insert #\newline)
                   (insert (make-object separator-snip%))
                   (insert #\newline))
                 (cond [(not (eq? post-sexp no-sexp))
                        (reset-style)
                        (format-sexp post-sexp post-redex result-highlight-color)]
                       [error-msg
                        (let ([before-error-msg (last-position)])
                          (reset-style)
                          (insert error-msg)
                          (change-style error-delta before-error-msg (last-position)))])
                 (end-edit-sequence)
                 (lock #t))])
      (sequence (super-init line-spacing tabstops)
                (set-style-list (f:scheme:get-style-list)))))
  

  (define error-delta (make-object style-delta% 'change-style 'italic))
  (send error-delta set-delta-foreground "RED")

  (define test-dc (make-object bitmap-dc% (make-object bitmap% 1 1)))
  (define result-highlight-color (make-object color% 255 255 255))
  (define redex-highlight-color (make-object color% 255 255 255))
  (send test-dc try-color (make-object color% 212 159 245) result-highlight-color)
  (send test-dc try-color (make-object color% 193 251 181) redex-highlight-color)
             
  (define no-sexp (gensym "no-sexp-"))
  
  (define (stepper-go drscheme-frame settings)
    (local
        ((define text (ivar drscheme-frame definitions-text))
         (define stepper-semaphore (make-semaphore))
         (define history null)
         
         (define current-expr #f)
         (define packaged-envs a:initial-env-package)
         
         (define user-eventspace (make-eventspace))
         
         (define (send-to-user-eventspace thunk)
           (send-to-other-eventspace user-eventspace thunk))
         
         (define user-primitive-eval #f)
         (define user-vocabulary #f)
         
         (define _1 ; install settings & library:
           (begin
             (send-to-user-eventspace 
              (lambda ()
                (set! user-primitive-eval (current-eval))
                (d:basis:initialize-parameters (make-custodian) settings)
                (d:rep:invoke-library)
                (set! user-namespace (current-namespace))
                (set! user-pre-defined-vars (map car (make-global-value-list)))
                (set! user-vocabulary (d:basis:current-vocabulary))
                (set! par-constructor-style-printing (p:constructor-style-printing))
                (set! par-abbreviate-cons-as-list (p:abbreviate-cons-as-list))
                (set! par-cons (global-defined-value 'cons))
                (set! par-vector (global-defined-value 'vector))
                (semaphore-post stepper-semaphore)))
             (semaphore-wait stepper-semaphore)))
         
         (define begin-next-expr
           (let* ([reader (z:read (f:gui-utils:read-snips/chars-from-buffer text)
                                  (z:make-location 1 1 0 "stepper-text"))]
                  [zodiac-eventspace (make-eventspace)]
                  [send-to-zodiac-eventspace 
                   (lambda (thunk)
                     (send-to-other-eventspace zodiac-eventspace thunk))]
                  [zodiac-semaphore (make-semaphore)])
             (send-to-zodiac-eventspace
              (lambda ()
                (d:basis:initialize-parameters (make-custodian) settings)
                (d:rep:invoke-library)
                (semaphore-post zodiac-semaphore)))
             (semaphore-wait zodiac-semaphore)
             (lambda ()
               (send-to-zodiac-eventspace
                (lambda ()
                  (let/ec k
                    (let ([inner-handler
                           (lambda (exn)
                             (send-to-drscheme-eventspace
                              (lambda ()
                                (handle-exception exn)))
                             (k))]
                          [return-handler
                           (lambda (read parsed)
                             (send-to-drscheme-eventspace
                              (lambda ()
                                (continue-next-expr read parsed))))])
                      (d:interface:set-zodiac-phase 'reader)
                      (let* ([new-expr (with-handlers
                                           ((exn:read? inner-handler))
                                         (reader))])
                        (return-handler new-expr
                                        (if (z:eof? new-expr)
                                            #f
                                            (begin
                                              (d:interface:set-zodiac-phase 'expander)
                                              (with-handlers
                                                  ((exn:syntax? inner-handler))
                                                (z:scheme-expand new-expr 'previous user-vocabulary)))))))))))))

         
         (define (continue-next-expr read parsed)
           (let/ec k
             (let ([exn-handler (make-exception-handler k)])
               (if (z:eof? read)
                   (construct-final-step)
                   (let*-values ([(annotated-list envs) (a:annotate (list read) (list parsed) packaged-envs break)]
                                 [(annotated) (car annotated-list)])
                     (set! packaged-envs envs)
                     (set! current-expr parsed)
                     (check-for-repeated-names parsed exn-handler)
                     (send-to-user-eventspace
                      (lambda ()
                        (let/ec k
                          (current-exception-handler (make-exception-handler k))
                          (user-primitive-eval annotated)
                          (send-to-drscheme-eventspace
                           (lambda ()
                             (add-finished-expr)
                             (begin-next-expr)))))))))))
         
         
         (define (check-for-repeated-names expr exn-handler)
           (with-handlers
               ((exn:user? exn-handler))
             (when (z:define-values-form? expr)
               (for-each (lambda (name) 
                           (when (check-global-defined name)
                             (error 'check-for-repeated-names
                                    "name is already bound: ~s" name)))
                         (map z:varref-var (z:define-values-form-vars expr))))))
         
         (define (add-finished-expr)
           (let ([reconstructed (r:reconstruct-completed current-expr)])
             (set! finished-exprs (append finished-exprs (list reconstructed)))))
         
         (define view-currently-updating #f)
         (define final-view #f)
         
         (define view 0)
             
         (define (home)
           (update-view 0))
         
         (define (next)
           (send previous-button enable #f)
           (send next-button enable #f)
           (send home-button enable #f)
           (if (= view (- (length history) 1))
               (update-view/next-step (+ view 1))
               (update-view (+ view 1))))
         
         (define (previous)
           (update-view (- view 1)))
             
         (define s-frame (make-object stepper-frame% drscheme-frame))
             
         (define button-panel (make-object horizontal-panel% (send s-frame get-area-container)))
         (define home-button (make-object button% "Home" button-panel
                                          (lambda (_1 _2) (home))))
         (define previous-button (make-object button% "<< Previous" button-panel
                                              (lambda (_1 _2) (previous))))
         (define next-button (make-object button% "Next >>" button-panel (lambda
                                                                             (_1 _2) (next))))
         (define canvas (make-object stepper-canvas% (send s-frame get-area-container)))

         (define (update-view new-view)
           (set! view new-view)
           (let ([e (list-ref history view)])
             (send e reset-pretty-print-width canvas)
             (send canvas set-editor e))
           (send previous-button enable (not (zero? view)))
           (send home-button enable (not (zero? view)))
           (send next-button enable (not (eq? final-view view))))
         
         (define held-expr no-sexp)
         (define held-redex no-sexp)

         (define (break mark-list break-kind returned-value-list)
           (let ([reconstruct-helper
                  (lambda (finish-thunk)
                    (parameterize ([current-eventspace drscheme-eventspace])
                      (queue-callback
                       (lambda ()
                         (let* ([reconstruct-pair
                                 (r:reconstruct-current current-expr 
                                                        mark-list
                                                        break-kind
                                                        returned-value-list)]
                                [reconstructed (car reconstruct-pair)]
                                [redex (cadr reconstruct-pair)])
                           (finish-thunk reconstructed redex))))))])                    
             (case break-kind
               [(normal) 
                (when (not (r:skip-redex-step? mark-list))
                  (reconstruct-helper 
                   (lambda (reconstructed redex)
                     (set! held-expr reconstructed)
                     (set! held-redex redex)
                     (semaphore-post stepper-semaphore)))
                  (semaphore-wait stepper-semaphore))]
               [(result-break)
                (when (if (not (null? returned-value-list))
                          (not (r:skip-redex-step? mark-list))
                          (and (not (eq? held-expr no-sexp))
                               (not (r:skip-result-step? mark-list))))
                  (reconstruct-helper 
                   (lambda (reconstructed redex)
                     (let ([step-text (make-object stepper-text% 
                                                   held-expr
                                                   held-redex
                                                   reconstructed
                                                   redex
                                                   break-kind
                                                   #f)])
                       (set! held-expr no-sexp)
                       (set! held-redex no-sexp)
                       (set! history (append history (list step-text)))
                       (update-view view-currently-updating))))
                  (semaphore-wait stepper-semaphore))])))

         (define (construct-final-step)
           (let ([step-text (make-object stepper-text% no-sexp no-sexp no-sexp no-sexp #f #f)])
             (set! history (append history (list step-text)))
             (set! final-view view-currently-updating)
             (update-view view-currently-updating)))

         (define (handle-exception exn)
           (let ([step-text (if held-expr
                                (make-object stepper-text% held-expr held-redex no-sexp no-sexp #f (exn-message exn))
                                (make-object stepper-text% no-sexp no-sexp no-sexp no-sexp #f (exn-message exn)))])
             (set! history (append history (list step-text)))
             (set! final-view view-currently-updating)
             (update-view view-currently-updating)))
         
         
         (define (make-exception-handler k)
           (lambda (exn)
             (parameterize ([current-eventspace drscheme-eventspace])
               (queue-callback
                (lambda ()
                  (handle-exception exn))))
             (k)))
             
             
         (define (update-view/next-step new-view)
           (set! view-currently-updating new-view)
           (semaphore-post stepper-semaphore)))
      
      (send drscheme-frame stepper-frame s-frame)
      (setup-print-convert settings)
      (set! finished-exprs null)
      (set! view-currently-updating 0)
      (begin-next-expr)
      (send button-panel stretchable-width #f)
      (send button-panel stretchable-height #f)
      (send canvas stretchable-height #t)
      (send canvas min-width 500)
      (send canvas min-height 500)
      (send previous-button enable #f)
      (send home-button enable #f)
      (send next-button enable #f)
      (send (send s-frame edit-menu:get-undo-item) enable #f)
      (send (send s-frame edit-menu:get-redo-item) enable #f)
      (send s-frame show #t)))
  
  (lambda (frame)
    (let ([settings (f:preferences:get 'drscheme:settings)])
      (if (not (string=? (d:basis:setting-name settings) beginner-level-name))
          (message-box "Stepper" 
                       (format (string-append "Language level is set to \"~a\".~n"
                                              "The Foot only works for the \"Beginner\" language level.~n")
                               (d:basis:setting-name settings))
                       #f 
                       '(ok))
          (stepper-go frame settings)))))
