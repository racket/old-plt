#|

todo:
- scroll bars

- handle ranges (just the first letter of an identifier), 
- show/hide menu in the wrong place
- dock/undock the preview window
- editing should make the annotations disappear (need to extend the program mixin)
- move calls to draw-pict over to user's eventspace

|#

(module tool mzscheme
  (require (lib "tool.ss" "drscheme")
           (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "unitsig.ss")
           (lib "string-constant.ss" "string-constants")
           (lib "framework.ss" "framework"))

  (provide tool@)

  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      
      (define original-output-port (current-output-port))
      (define (printf . args) (apply fprintf original-output-port args))
      
      (define sc-show-slideshow-panel "Show Slideshow Panel")
      (define sc-hide-slideshow-panel "Hide Slideshow Panel")
      (define sc-freeze-picts "Freeze These Picts")
      (define sc-thaw-picts "Show Picts Under Mouse")
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;;  drscheme extensions
      ;;
      
      (define-struct p (pict-drawer width height))
      
      (define show-picts<%>
        (interface ()
          slideshow:register-pict))
      
      (define show-picts-mixin
        (mixin (color:text<%> editor<%>) (show-picts<%>)
          (inherit get-canvas freeze-colorer)
          
          (define picts-ht (make-hash-table 'equal))
          (define frozen-colorers (make-hash-table))
          (define clear-highlights void)
          
          (define mouse-loc #f)
          (define visible-picts #f)
          
          (define/public (slideshow:clear-picts)
            (set! picts-ht (make-hash-table 'equal))
            (hash-table-for-each
             frozen-colorers
             (lambda (k v)
               (send k thaw-colorer)))
            (clear-highlights)
            (set! clear-highlights void)
            (set! frozen-colorers (make-hash-table)))
          
          (define/public (slideshow:register-pict text offset range pict-drawer width height)
            (hash-table-get frozen-colorers
                            text
                            (lambda ()
                              (when (is-a? text color:text<%>)
                                (let ([locked? (send text is-locked?)])
                                  (send text lock #f)
                                  (send text freeze-colorer)
                                  (send text lock locked?))
                                (hash-table-put! frozen-colorers text #t))))
            (let ([locked? (send text is-locked?)])
              (send text lock #f)
              ;(send text change-style has-info-style offset (+ offset 1) #f)
              (set! clear-highlights
                    (let ([ch clear-highlights]
                          [new (send text highlight-range offset (+ offset 1) has-info-bkg-color)])
                      (lambda ()
                        (ch)
                        (new))))
              (send text lock locked?))
            (let ([key (cons text offset)])
              (hash-table-put! 
               picts-ht
               key
               (append
                (hash-table-get picts-ht key (lambda () null))
                (list (make-p pict-drawer width height))))))
          
          (rename [super-on-event on-event])
          (define/override (on-event evt)
            (cond
              [(send evt leaving?)
               (update-mouse #f #f)
               (super-on-event evt)]
              [(or (send evt moving?)
                   (send evt entering?))
               (let-values ([(pos text) (get-pos/text evt)])
                 (update-mouse text pos))
               (super-on-event evt)]
              [(send evt button-down? 'right)
               (let-values ([(pos text) (get-pos/text evt)])
                 (if (and pos text)
                     (show-menu evt text pos)
                     (super-on-event evt)))]
              [else
               (super-on-event evt)]))
          
          (define/private (show-menu evt text pos)
            (let ([frame (let ([canvas (get-canvas)])
                           (and canvas
                                (send canvas get-top-level-window)))])
              (when frame
                (let ([admin (send text get-admin)]
                      [menu (new popup-menu%)]
                      [show? #f])
                  (let* ([frozen-mouse-picts-key (cons text pos)]
                         [picts (hash-table-get picts-ht frozen-mouse-picts-key (lambda () #f))])
                    (when picts
                      (set! show? #t)
                      (new menu-item%
                           (label sc-freeze-picts)
                           (parent menu)
                           (callback
                            (lambda (x y)
                              (send frame slideshow:set-permanent-picts picts))))))
                  (when (send frame slideshow:has-permanent-picts?)
                    (new menu-item%
                         (label sc-thaw-picts)
                         (parent menu)
                         (callback
                          (lambda (x y)
                            (send frame slideshow:set-permanent-picts #f))))
                    (set! show? #t))
                  (when show?
                    (send admin popup-menu 
                          menu
                          (send evt get-x)
                          (send evt get-y)))))))
            
          (define/private (update-mouse text pos)
            (let ([new-mouse-loc (and text pos (cons text pos))])
              (unless (equal? new-mouse-loc mouse-loc)
                (set! mouse-loc new-mouse-loc)
                (let ([frame (let ([canvas (get-canvas)])
                               (and canvas
                                    (send canvas get-top-level-window)))])
                  (when frame
                    (send frame slideshow:set-visible-picts
                          (and pos 
                               text 
                               (hash-table-get picts-ht new-mouse-loc (lambda () #f)))))))))
              
          ;; get-pos/text : event -> (values (union #f text%) (union number #f))
          ;; returns two #fs to indicate the event doesn't correspond to
          ;; a position in an editor, or returns the innermost text
          ;; and position in that text where the event is.
          (define (get-pos/text event)
            (let ([event-x (send event get-x)]
                  [event-y (send event get-y)]
                  [on-it? (box #f)])
              (let loop ([editor this])
                (let-values ([(x y) (send editor dc-location-to-editor-location event-x event-y)])
                  (cond
                    [(is-a? editor text%)
                     (let ([pos (send editor find-position x y #f on-it?)])
                       (cond
                         [(not (unbox on-it?)) (values #f #f)]
                         [else
                          (let ([snip (send editor find-snip pos 'after-or-none)])
                            (if (and snip
                                     (is-a? snip editor-snip%))
                                (loop (send snip get-editor))
                                (values pos editor)))]))]
                    [(is-a? editor pasteboard%)
                     (let ([snip (send editor find-snip x y)])
                       (if (and snip
                                (is-a? snip editor-snip%))
                           (loop (send snip get-editor))
                           (values #f #f)))]
                    [else (values #f #f)])))))
          
          (super-new)))
      
      (define (unit-frame-mixin %)
        (class %
          (inherit get-show-menu get-definitions-text get-interactions-text)
          
          (define slideshow-parent-panel #f)
          (define everything-else-panel #f)
          (define slideshow-panel #f)
          (define slideshow-canvas #f)
          (define slideshow-panel-visible? #f)
          
          (define permanent-picts #f)
          (define visible-picts #f)
          
          (define/public (slideshow:set-visible-picts picts)
            (unless (equal? picts visible-picts)
              (set! visible-picts picts)
              (when slideshow-panel-visible?
                (draw-picts (send slideshow-canvas get-dc)))))
          
          (define/public (slideshow:set-permanent-picts picts)
            (set! permanent-picts picts)
            (if picts
                (send slideshow-canvas 
                      init-auto-scrollbars
                      (inexact->exact (floor (apply max (map p-width picts))))
                      (inexact->exact (floor (apply + (map p-height picts))))
                      0
                      0)
                (send slideshow-canvas init-auto-scrollbars #f #f 0 0)))
          (define/public (slideshow:has-permanent-picts?) permanent-picts)
          
          (rename [super-make-root-area-container make-root-area-container])
          (define/override (make-root-area-container cls parent)
            (set! slideshow-parent-panel (super-make-root-area-container slideshow-dragable% parent))
            (let ([root (make-object cls slideshow-parent-panel)])
              (set! everything-else-panel root)
              root))
          
          (rename [super-update-shown update-shown])
          (define/override (update-shown)
            (super-update-shown)
            (if slideshow-panel-visible?
                (begin
                  (unless slideshow-panel (build-slideshow-panel))
                  (when (is-a? view-menu-item menu-item%)
                    (send view-menu-item set-label sc-hide-slideshow-panel))
                  (send slideshow-parent-panel
                        change-children
                        (lambda (l)
                          (list everything-else-panel slideshow-panel))))
                (begin
                  (when (is-a? view-menu-item menu-item%)
                    (send view-menu-item set-label sc-show-slideshow-panel))
                  (send slideshow-parent-panel
                        change-children
                        (lambda (l)
                          (list everything-else-panel))))))
          
          (define (build-slideshow-panel)
            (let ([p (preferences:get 'plt:slideshow:panel-percentage)])
              ;; must save the value of the pref before creating slideshow-panel
              ;; so that the callback doesn't clobber it
              
              (set! slideshow-panel (new vertical-panel% (parent slideshow-parent-panel)))
              (set! slideshow-canvas (new canvas%
                                          (style '(hscroll vscroll))
                                          (parent slideshow-panel)
                                          (paint-callback
                                           (lambda (x dc)
                                             (draw-picts dc)))))
              (send slideshow-parent-panel set-percentages (list p (- 1 p)))
              (preferences:set 'plt:slideshow:panel-percentage p)))

          (define/private (draw-picts dc)
            (send dc clear)
            (let ([picts (or permanent-picts visible-picts)])
              (when picts
                (let loop ([picts picts]
                           [y 0])
                  (cond
                    [(null? picts) (void)]
                    [else (let ([pict (car picts)])
                            ((p-pict-drawer pict) dc 0 y)
                            (loop (cdr picts)
                                  (+ y (p-height pict))))])))))
          
          (rename [super-clear-annotations clear-annotations])
          (define/override (clear-annotations)
            (send (get-definitions-text) slideshow:clear-picts)
            (send (get-interactions-text) slideshow:clear-picts)
            (super-clear-annotations))
          
          (super-new)
          
          (define view-menu-item 
            (new menu-item%
                 (label sc-show-slideshow-panel)
                 (parent (get-show-menu))
                 (callback
                  (lambda (x y)
                    (set! slideshow-panel-visible? (not slideshow-panel-visible?))
                    (update-shown)))))))
      
      (define slideshow-dragable%
        (class panel:horizontal-dragable%
          (inherit get-percentages)
          (rename [super-after-percentage-change after-percentage-change])
          (define/override (after-percentage-change)
            (let ([percentages (get-percentages)])
              (when (= 2 (length percentages))
                (preferences:set 'plt:slideshow:panel-percentage (car percentages))))
            (super-after-percentage-change))
          (super-new)))

      (define has-info-bkg-color (make-object color% "gray"))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;; preference defaults
      ;;
      
      ;; size of the drscheme window.
      (preferences:set-default 'plt:slideshow:panel-percentage 3/4 (lambda (x) (and (number? x) (<= 0 x 1))))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;;  communication from user thread to drscheme's repl
      ;;
            
      (define system-eventspace (current-eventspace))
      
      ;; send-over : any syntax -> void
      ;; thread: (any) user's thread
      (define (send-over v stx)
        (let ([rep (drscheme:rep:current-rep)])
          (when rep
            (let ([pict? (dynamic-require '(lib "mrpict.ss" "texpict") 'pict?)])
              (when (pict? v)
                (let* ([make-pict-drawer (dynamic-require '(lib "mrpict.ss" "texpict") 'make-pict-drawer)]
                       [width ((dynamic-require '(lib "mrpict.ss" "texpict") 'pict-width) v)]
                       [height ((dynamic-require '(lib "mrpict.ss" "texpict") 'pict-height) v)]
                       [pict-drawer (make-pict-drawer v)])
                  (parameterize ([current-eventspace system-eventspace])
                    (queue-callback
                     (lambda ()
                       (add-pict-drawer stx pict-drawer width height))))))))))
      
      ;; add-pict-drawer : syntax pict-drawer number number -> void
      ;; thread: system eventspace
      (define (add-pict-drawer stx pict-drawer width height)
        (let ([src (syntax-source stx)]
              [offset (syntax-position stx)]
              [span (syntax-span stx)])
          (when (and (is-a? src editor<%>)
                     (number? offset)
                     (number? span))
            (let ([top-most (let loop ([src src])
                              (let ([admin (send src get-admin)])
                                (cond
                                  [(not admin) #f]
                                  [(is-a? admin editor-snip-editor-admin<%>)
                                   (let* ([outer-editor-snip (send admin get-snip)]
                                          [es-admin (send outer-editor-snip get-admin)]
                                          [outer-editor (send es-admin get-editor)])
                                     (loop outer-editor))]
                                  [else src])))])
              (when (is-a? top-most show-picts<%>)
                (send top-most slideshow:register-pict src (- offset 1) span pict-drawer width height))))))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;; slideshow lang
      ;;
      
      (define (slideshow-mixin lang%)
        (class lang%
          (rename [super-front-end/complete-program front-end/complete-program])
          (define/override (front-end/complete-program input settings teachpack-cache)
            (let ([st (super-front-end/complete-program input settings teachpack-cache)])
              (lambda ()
                (let ([sv (st)])
                  (cond
                    [(syntax? sv) (rewrite-syntax sv)]
                    [else sv])))))
          (rename [super-front-end/interaction front-end/interaction])
          (define/override (front-end/interaction input settings teachpack-cache)
            (let ([st (super-front-end/interaction input settings teachpack-cache)])
              (lambda ()
                (let ([sv (st)])
                  (cond
                    [(syntax? sv) (rewrite-syntax sv)]
                    [else sv])))))
          (define/override (get-language-name) "Slideshow")
          (super-new (module '(lib "plt-mred.ss" "lang"))
                     (language-position (list (string-constant experimental-languages)
                                              "Slideshow"))
                     (language-numbers (list 1000 341)))))
      
      (define (rewrite-syntax stx)
        (rewrite-top-level (expand stx)))
      
      (define (rewrite-top-level stx)
        (syntax-case stx (module begin)
          [(module identifier name (#%plain-module-begin module-level-expr ...))
           (with-syntax ([(rewritten-module-level-expr ...) (map rewrite-module-level
                                                                 (syntax->list
                                                                  (syntax (module-level-expr ...))))])
             (syntax (module identifier name (#%plain-module-begin rewritten-module-level-expr ...))))]
          [(begin top-level-expr ...)
           (with-syntax ([(rewritten-top-level-expr ...) 
                          (map rewrite-top-level (syntax->list (syntax (top-level-expr ...))))])
             (syntax (begin rewritten-top-level-expr ...)))]
          [general-top-level-expr (rewrite-general-top-level stx)]))

      (define (rewrite-module-level stx)
        (syntax-case stx (provide begin)
          [(provide provide-spec ...) stx]
          [(begin module-level-expr ...)
           (with-syntax ([(rewritten-module-level-expr ...)
                          (map rewrite-module-level 
                               (syntax->list (syntax (module-level-expr  ...))))])
             (syntax (begin rewritten-module-level-expr ...)))]
          [general-top-level-expr (rewrite-general-top-level stx)]))
      
      (define (rewrite-general-top-level stx)
        (syntax-case stx (define-values define-syntaxes require require-for-syntax)
          [(define-values (variable ...) expr)
           (with-syntax ([rewritten-expr (add-send-over (rewrite-expr (syntax expr)) 
                                                        (syntax expr) 
                                                        (length (syntax->list (syntax (variable ...)))))])
             (syntax (define-values (variable ...) rewritten-expr)))]
          [(define-syntaxes (variable ...) expr) stx]
          [(require require-spec ...) stx]
          [(require-for-syntax require-spec ...) stx]
          [expr (rewrite-expr stx)]))
      
      (define (rewrite-expr stx)
        (syntax-case stx (lambda case-lambda if begin begin0 let-values letrec-values set! quote quote-syntax with-continuation-mark #%app #%datum #%top)
          [variable
           (identifier? (syntax variable))
           (add-send-over (syntax variable) stx 1)]
          [(lambda formals expr ...)
           (with-syntax ([(rewritten-expr ...) 
                          (map rewrite-expr (syntax->list (syntax (expr ...))))])
                  (syntax (lambda formals rewritten-expr ...)))]
          [(case-lambda (formals expr ...) ...)
           (with-syntax ([((rewritten-expr ...) ...)
                          (map (lambda (exprs) (map rewrite-expr (syntax->list exprs)))
                               (syntax->list (syntax ((expr ...) ...))))])
             (syntax (case-lambda (formals rewritten-expr ...) ...)))]
          [(if expr1 expr2)
           (with-syntax ([rewritten-expr1 (add-send-over (rewrite-expr (syntax expr1)) (syntax expr1) 1)]
                         [rewritten-expr2 (rewrite-expr (syntax expr2))])
             (syntax (if rewritten-expr1 rewritten-expr2)))]
          [(if expr1 expr2 expr3)
           (with-syntax ([rewritten-expr1 (add-send-over (rewrite-expr (syntax expr1)) (syntax expr1) 1)]
                         [rewritten-expr2 (rewrite-expr (syntax expr2))]
                         [rewritten-expr3 (rewrite-expr (syntax expr3))])
             (syntax (if rewritten-expr1 rewritten-expr2 rewritten-expr3)))]
          [(begin expr ... last-expr)
           (with-syntax ([(rewritten-expr ...) (map (lambda (x) (add-send-over (rewrite-expr x) x 1))
                                                    (syntax->list (syntax (expr ...))))]
                         [rewritten-last-expr (rewrite-expr (syntax last-expr))])
             (syntax (begin rewritten-expr ... rewritten-last-expr)))]
          [(begin0 expr ...)
           (with-syntax ([(rewritten-expr ...) (map (lambda (x) (add-send-over (rewrite-expr x) x 1)) 
                                                    (syntax->list (syntax (expr ...))))])
             (syntax (begin0 rewritten-expr ...)))]
          [(let-values (((variable ...) v-expr) ...) expr ...)
           (with-syntax ([(rewritten-expr ...) (map rewrite-expr (syntax->list (syntax (expr ...))))]
                         [(rewritten-v-expr ...) (map (lambda (x vars) 
                                                        (add-send-over (rewrite-expr x) x (length (syntax->list vars))))
                                                      (syntax->list (syntax (v-expr ...)))
                                                      (syntax->list (syntax ((variable ...) ...))))])
             (syntax (let-values (((variable ...) rewritten-v-expr) ...) rewritten-expr ...)))]
          [(letrec-values (((variable ...) v-expr) ...) expr ...)
           (with-syntax ([(rewritten-expr ...) (map rewrite-expr (syntax->list (syntax (expr ...))))]
                         [(rewritten-v-expr ...) (map (lambda (x vars) 
                                                        (add-send-over (rewrite-expr x) x (length (syntax->list vars)))) 
                                                      (syntax->list (syntax (v-expr ...)))
                                                      (syntax->list (syntax ((variable ...) ...))))])
             (syntax (letrec-values (((variable ...) rewritten-v-expr) ...) rewritten-expr ...)))]
          [(set! variable expr)
           (with-syntax ([rewritten-expr (add-send-over (rewrite-expr (syntax expr)) (syntax expr) 1)])
             (syntax (set! variable rewritten-expr)))]
          [(quote datum) stx]
          [(quote-syntax datum) stx]
          [(with-continuation-mark expr1 expr2 expr3)
           (with-syntax ([rewritten-expr1 (add-send-over (rewrite-expr (syntax expr1)) (syntax expr1) 1)]
                         [rewritten-expr2 (add-send-over (rewrite-expr (syntax expr2)) (syntax expr2) 1)]
                         [rewritten-expr3 (rewrite-expr (syntax expr3))])
             (syntax (with-continuation-mark rewritten-expr1 rewritten-expr2 rewritten-expr3)))]
          [(#%app expr ...)
           (with-syntax ([(rewritten-expr ...) (map (lambda (x) (add-send-over (rewrite-expr x) x 1)) 
                                                    (syntax->list (syntax (expr ...))))])
             (syntax (#%app rewritten-expr ...)))]
          [(#%datum . datum) stx]
          [(#%top . variable) stx]))
      
      (define (add-send-over stx loc-stx values-expected)
        (if (object? (syntax-source loc-stx))
            (with-syntax ([send-over send-over]
                          [stx stx]
                          [loc (datum->syntax-object loc-stx 1 loc-stx)]
                          [(vars ...) (build-vars values-expected)])
              (syntax
               (let-values ([(vars ...) stx])
                 (send-over vars #'loc) ...
                 (values vars ...))))
            stx))
      
      (define (build-vars n)
        (cond
          [(zero? n) #'()]
          [else (cons (datum->syntax-object #'here (string->symbol (format "x~a" n)))
                      (build-vars (- n 1)))]))

      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;;  wire it up
      ;;
      
      (drscheme:get/extend:extend-interactions-text show-picts-mixin)
      (drscheme:get/extend:extend-definitions-text show-picts-mixin)
      (drscheme:get/extend:extend-unit-frame unit-frame-mixin)

      (define (phase1) (void))
      (define (phase2)
        (define slideshow-language%
          (slideshow-mixin
           ((drscheme:language:get-default-mixin)
            (drscheme:language:module-based-language->language-mixin
             (drscheme:language:simple-module-based-language->module-based-language-mixin
              drscheme:language:simple-module-based-language%)))))
        
        (drscheme:language-configuration:add-language
         (new slideshow-language%)))
      
      
      )))

