(unit/sig stepper:view-controller^
  (import ...)

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
  
  (define stepper-text%
    (class f:text:basic% (finished-exprs pre-sexp pre-redex post-sexp post-redex 
                                         break-kind error-msg (line-spacing 1.0) (tabstops null))
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
  
  
  (define (gui-init ;;; hmmm, how do we pass all this stuff in?)
  ;; gui initialization code
  
  (define error-delta (make-object style-delta% 'change-style 'italic))
  (send error-delta set-delta-foreground "RED")

  (define test-dc (make-object bitmap-dc% (make-object bitmap% 1 1)))
  (define result-highlight-color (make-object color% 255 255 255))
  (define redex-highlight-color (make-object color% 255 255 255))
  (send test-dc try-color (make-object color% 212 159 245) result-highlight-color)
  (send test-dc try-color (make-object color% 193 251 181) redex-highlight-color)
             
  (define no-sexp (gensym "no-sexp-"))

  ;;; stuff schlepped over from what used to be stepperr.ss
  
    (define history null)
         

  ; PUT THIS WHERE IT BELONGS: (define text (ivar drscheme-frame definitions-text))
    
             (define view-currently-updating #f)
         (define final-view #f)
         
         (define view 0)
             
         (define (home)
           (update-view 0))
         
         (define (next)
           (gui:prev-enable #f)
           (gui:next-enable #f)
           (gui:home-enable #f)
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
)