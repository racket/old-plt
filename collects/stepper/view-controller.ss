(unit/sig (stepper-go)
  (import [c : mzlib:core^]
          [e : zodiac:interface^]
          [z : zodiac:system^]
          [cp : stepper:client-procs^]
          mzlib:pretty-print^
          mred^
          [d : drscheme:export^]
          [p : mzlib:print-convert^]
          [f : framework^]
          stepper:shared^
          [utils : stepper:cogen-utils^]
          [marks : stepper:marks^])

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
  
  (define (image? val)
   (is-a? val snip%))
  
  (define (confusable-value? val)
    (or (number? val)
        (boolean? val)
        (string? val)
        (symbol? val)))
  
  
  ; constructor : ((listof sexp) (union sexp no-sexp) (union sexp no-sexp) 
  ;                (union sexp no-sexp multiple-highlight) (union sexp no-sexp) (union string #f) ... -> )
  
  ; redexes MUST NOT OVERLAP. all warranties void if this is violated.
  
  (define stepper-text%
    (class f:text:basic% (finished-exprs exps redex-list post-exps reduct-list 
                                         error-msg after-exprs (line-spacing 1.0) (tabstops null))
      (inherit find-snip insert change-style highlight-range last-position lock erase auto-wrap
               begin-edit-sequence end-edit-sequence get-start-position get-style-list set-style-list)
      (public (pretty-printed-width -1)
              (char-width 0)
              (clear-highlight-thunks null)
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
              [highlight-begin #f]
              [highlight-color #f]
              [remaining-highlights null]
              [highlight-pop
               (lambda ()
                 (begin0 (car remaining-highlights) (set! remaining-highlights (cdr remaining-highlights))))]
              [format-sexp
               (lambda (sexp)
                 (let ([real-print-hook (pretty-print-print-hook)]
                       [placeholder-present? #f])
                   (parameterize ([pretty-print-size-hook
                                   (lambda (value display? port)
                                     (if (eq? value highlight-placeholder)
                                         (begin
                                           (set! placeholder-present? #t)
                                           (string-length (format "~s" (car remaining-highlights))))
                                         (if (image? value)
                                             1   ; if there was a good way to calculate a image widths ...
                                             #f)))]
                                  [pretty-print-print-hook
                                   (lambda (value display? port)
                                     (if (eq? value highlight-placeholder)
                                         (insert (format "~s" (car remaining-highlights)))
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
                                                    (eq? value (car remaining-highlights)))
                                               (eq? value highlight-placeholder))
                                       (set! highlight-begin (get-start-position))))]
                                  [pretty-print-post-print-hook
                                   (lambda (value p)
                                     (when (or (and (not placeholder-present?)
                                                    (eq? value (car remaining-highlights)))
                                               (eq? value highlight-placeholder))
                                       (highlight-pop)
                                       (set! placeholder-present? #f)
                                       (let ([highlight-end (get-start-position)])
                                         (unless highlight-begin
                                           (error 'format-whole-step "no highlight-begin to match highlight-end"))
                                         (set! clear-highlight-thunks
                                               (cons (highlight-range highlight-begin highlight-end highlight-color #f #f)
                                                     clear-highlight-thunks))
                                         (set! highlight-begin #f))))])
                     (pretty-print sexp))))]
              
              [advance-substitute
               (lambda (exp)
                 (letrec ([stack-copy remaining-highlights]
                          [stack-pop (lambda () (begin0 (car stack-copy) (set! stack-copy (cdr stack-copy))))]
                          [substitute
                           (lambda (exp)
                             (cond [(eq? exp highlight-placeholder)
                                    (let ([popped (stack-pop)])
                                      (if (confusable-value? popped)
                                          highlight-placeholder
                                          popped))]
                                   [(pair? exp)
                                    (cons (substitute (car exp)) (substitute (cdr exp)))]
                                   [else
                                    exp]))])
                 (substitute exp)))]
              

              
              [format-whole-step
               (lambda ()
                 (lock #f)
                 (begin-edit-sequence)
                 (for-each (lambda (fun) (fun)) clear-highlight-thunks)
                 (set! clear-highlight-thunks null)
                 (erase)
                 (for-each
                  (lambda (expr)
                    (format-sexp expr)
                    (insert #\newline))
                  finished-exprs)
                 (insert (make-object separator-snip%))
                 (when (not (eq? redex no-sexp))
                   (insert #\newline)
                   (reset-style)
                   (set! remaining-highlights redex-list)
                   (set! highlight-color redex-highlight-color)
                   (unless (= (length exps) 1)
                     (error 'format-sexp "wrong-length exp list in pre-step"))
                   (format-sexp (advance-substitute (car exps)))
                   (unless (null? highlights-remaining)
                     (error 'format-whole-step "left-over highlights in pre-step"))
                   (insert #\newline)
                   (insert (make-object separator-snip%))
                   (insert #\newline))
                 (cond [(not (eq? reduct no-sexp))
                        (reset-style)
                        (set! remaining-highlights reduct-list)
                        (set! highlight-color reduct-highlight-color)
                        (for-each
                         (lambda (exp) (format-sexp (advance-substitute exp)))
                         post-exps)
                        (unless (null? highlights-remaining)
                          (error 'format-whole-step "left-over highlights in post-step"))]
                       [error-msg
                        (let ([before-error-msg (last-position)])
                          (reset-style)
                          (auto-wrap #t)
                          (insert error-msg)
                          (change-style error-delta before-error-msg (last-position)))])
                 (unless (eq? after-exprs no-sexp)
                   (insert #\newline)
                   (insert (make-object separator-snip%))
                   (insert #\newline)
                   (reset-style)
                   (for-each
                    (lambda (expr)
                      (format-sexp expr)
                      (insert #\newline))
                    after-exprs))
                 (end-edit-sequence)
                 (lock #t))])
      (sequence (super-init line-spacing tabstops)
                (set-style-list (f:scheme:get-style-list)))))
  
  ;; DO SOME DAMN TEST CASES.
   
  (define error-delta (make-object style-delta% 'change-style 'italic))
  (send error-delta set-delta-foreground "RED")

  (define test-dc (make-object bitmap-dc% (make-object bitmap% 1 1)))
  (define result-highlight-color (make-object color% 255 255 255))
  (define redex-highlight-color (make-object color% 255 255 255))
  (send test-dc try-color (make-object color% 212 159 245) result-highlight-color)
  (send test-dc try-color (make-object color% 193 251 181) redex-highlight-color)

  (define (stepper-wrapper drscheme-frame settings)
    
    (local ((define view-history null)
            (define view-currently-updating #f)
            (define final-view #f)
            (define view 0)
            
            ; build gui object:
            
            (define (home)
              (update-view 0))
            
            (define (next)
              (send next-button enable #f)
              (send previous-button enable #f)
              (send home-button enable #f)
              (if (= view (- (length view-history) 1))
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
            
            (define (update-view/next-step new-view)
              (set! view-currently-updating new-view)
              (step))
            
            (define (update-view new-view)
              (set! view new-view)
              (let ([e (list-ref view-history view)])
                (send e reset-pretty-print-width canvas)
                (send canvas lazy-refresh #t)
                (send canvas set-editor e)
                (send e set-position (send e last-position))
                (send canvas lazy-refresh #f))
              (send previous-button enable (not (zero? view)))
              (send home-button enable (not (zero? view)))
              (send next-button enable (not (eq? final-view view))))
            
            (define (receive-result result)
              (let ([step-text
                     (cond [(before-after-result? result) 
                            (make-object stepper-text% 
                                         (before-after-result-finished-exprs result)
                                         (before-after-result-exp result)
                                         (before-after-result-redex result)
                                         (before-after-result-post-exp result)
                                         (before-after-result-reduct result)
                                         #f
                                         (before-after-result-after-exprs result))]
                           [(before-error-result? result)
                            (set! final-view view-currently-updating)
                            (make-object stepper-text%
                                         (before-error-result-finished-exprs result)
                                         (before-error-result-exp result)
                                         (before-error-result-redex result)
                                         no-sexp
                                         no-sexp
                                         (before-error-result-err-msg result)
                                         (before-error-result-after-exprs result))]
                           [(error-result? result)  
                            (set! final-view view-currently-updating)
                            (make-object stepper-text%
                                         (error-result-finished-exprs result)
                                         no-sexp
                                         no-sexp
                                         no-sexp
                                         no-sexp
                                         (error-result-err-msg result)
                                         no-sexp)]
                           [(finished-result? result)
                            (set! final-view view-currently-updating)
                            (make-object stepper-text%
                                         (finished-result-finished-exprs result)
                                         no-sexp
                                         no-sexp
                                         no-sexp
                                         no-sexp
                                         #f
                                         no-sexp)])])
                (set! view-history (append view-history (list step-text))) 
                (update-view view-currently-updating)))
            
            (define text-stream
              (f:gui-utils:read-snips/chars-from-text (ivar drscheme-frame definitions-text)))
            
            (define step 
              (invoke-unit/sig (require-library-unit/sig "instance.ss" "stepper")
                               stepper:model-input^
                               (c : mzlib:core^)
                               (e : zodiac:interface^)
                               (p : mzlib:print-convert^)
                               (d : drscheme:export^)
                               (z : zodiac:system^)
                               (cp : stepper:client-procs^)
                               stepper:shared^
                               mred^
                               (utils : stepper:cogen-utils^)
                               (marks : stepper:marks^))))
      
      (send drscheme-frame stepper-frame s-frame)
      (set! view-currently-updating 0)
      (send button-panel stretchable-width #f)
      (send button-panel stretchable-height #f)
      (send canvas stretchable-height #t)
      (send canvas min-width 400)
      (send canvas min-height 100)
      (send previous-button enable #f)
      (send home-button enable #f)
      (send next-button enable #f)
      (send (send s-frame edit-menu:get-undo-item) enable #f)
      (send (send s-frame edit-menu:get-redo-item) enable #f)
      (step)
      (send s-frame show #t)))
  
  (define beginner-level-name "Beginning Student")
      
  (define (stepper-go frame)
    (let ([settings (f:preferences:get d:language:settings-preferences-symbol)])
      (if #f ; (not (string=? (d:basis:setting-name settings) beginner-level-name))
          (message-box "Stepper" 
                       (format (string-append "Language level is set to \"~a\".~n"
                                              "The Foot only works for the \"~a\" language level.~n")
                               (d:basis:setting-name settings)
                               beginner-level-name)
                       #f 
                       '(ok))
          (stepper-wrapper frame settings)))))
