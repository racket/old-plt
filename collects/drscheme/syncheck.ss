
(module syncheck mzscheme
  (require (lib "unitsig.ss")
           "tool.ss"
           "default-code-style.ss"
           (lib "class.ss")
           (lib "list.ss")
           (prefix drscheme:arrow: "arrow.ss")
           (prefix fw: (lib "framework.ss" "framework"))
           (prefix mred: (lib "mred.ss" "mred")))
  (provide tool@)
  
  (define tool@
    (unit/sig ()
      (import drscheme:tool^)
      
      (define add/mult-set
        (lambda (m v)
          (send m set (car v) (cadr v) (caddr v))))
      
      (define add/mult-get
        (lambda (m)
          (let ([b1 (box 0)]
                [b2 (box 0)]
                [b3 (box 0)])
            (send m get b1 b2 b3)
            (map unbox (list b1 b2 b3)))))
      
      (define style-delta-get/set
        (list (cons (lambda (x) (send x get-alignment-off))
                    (lambda (x v) (send x set-alignment-off v)))
              (cons (lambda (x) (send x get-alignment-on))
                    (lambda (x v) (send x set-alignment-on v)))
              (cons (lambda (x) (add/mult-get (send x get-background-add)))
                    (lambda (x v) (add/mult-set (send x get-background-add) v)))
              (cons (lambda (x) (add/mult-get (send x get-background-mult)))
                    (lambda (x v) (add/mult-set (send x get-background-mult) v)))
              (cons (lambda (x) (send x get-face))
                    (lambda (x v) (send x set-face v)))
              (cons (lambda (x) (send x get-family))
                    (lambda (x v) (send x set-family v)))
              (cons (lambda (x) (add/mult-get (send x get-foreground-add)))
                    (lambda (x v) (add/mult-set (send x get-foreground-add) v)))
              (cons (lambda (x) (add/mult-get (send x get-foreground-mult)))
                    (lambda (x v) (add/mult-set (send x get-foreground-mult) v)))
              (cons (lambda (x) (send x get-size-add))
                    (lambda (x v) (send x set-size-add v)))
              (cons (lambda (x) (send x get-size-mult))
                    (lambda (x v) (send x set-size-mult v)))
              (cons (lambda (x) (send x get-style-off))
                    (lambda (x v) (send x set-style-off v)))
              (cons (lambda (x) (send x get-style-on))
                    (lambda (x v) (send x set-style-on v)))
              (cons (lambda (x) (send x get-underlined-off))
                    (lambda (x v) (send x set-underlined-off v)))
              (cons (lambda (x) (send x get-underlined-on))
                    (lambda (x v) (send x set-underlined-on v)))
              (cons (lambda (x) (send x get-weight-off))
                    (lambda (x v) (send x set-weight-off v)))
              (cons (lambda (x) (send x get-weight-on))
                    (lambda (x v) (send x set-weight-on v)))))
      
      (define marshall-style
        (lambda (style)
          (map (lambda (fs) ((car fs) style)) style-delta-get/set)))
      
      (define unmarshall-style
        (lambda (info)
          (let ([style (make-object mred:style-delta%)])
            (for-each (lambda (fs v) ((cdr fs) style v)) style-delta-get/set info)
            style)))
      
      (define prefixed-code-styles 
        (map (lambda (x) 
               (cons
                (string->symbol (string-append "drscheme:check-syntax:" (symbol->string (car x))))
                (cdr x)))
             (if ((mred:get-display-depth) . < . 8)
                 bw-default-code-styles
                 color-default-code-styles)))
      
      (define delta-symbols (map car prefixed-code-styles))
      
      (let ([set-default
             (lambda (default)
               (let* ([sym (car default)]
                      [code-style (cadr default)]
                      [color (code-style-color code-style)])
                 (fw:preferences:set-default
                  sym
                  (let ([s (make-object mred:style-delta%)])
                    (send s set-delta-foreground (if (string? color)
                                                     color
                                                     (make-object mred:color%
                                                       (car color)
                                                       (cadr color)
                                                       (caddr color))))
                    (when (code-style-bold? code-style)
                      (send s set-delta 'change-bold))
                    (when (code-style-underline? code-style)
                      (send s set-delta 'change-underline #t))
                    (when (code-style-slant? code-style)
                      (send s set-delta 'change-italic))
                    s)
                  (lambda (x)
                    (is-a? x mred:style-delta%)))))])
        (for-each set-default prefixed-code-styles))
      
      (for-each 
       (lambda (s) 
         (fw:preferences:set-un/marshall s marshall-style unmarshall-style))
       delta-symbols)
      
      ; a symbol naming the style  and a delta to set it to
      (define set-slatex-style
        (lambda (sym delta)
          (let* ([style-list (fw:scheme:get-style-list)]
                 [name (symbol->string sym)]
                 [style (send style-list find-named-style name)])
            (if style
                (send style set-delta delta)
                (send style-list new-named-style name
                      (send style-list find-or-create-style
                            (send style-list
                                  find-named-style "Standard")
                            delta))))))
      
      (for-each set-slatex-style delta-symbols (map fw:preferences:get delta-symbols))
      
      ;; used for quicker debugging of the preference panel
      '(define test-preference-panel
         (lambda (name f)
           (let ([frame (make-object mred:frame% name)])
             (f frame)
             (send frame show #t))))
      
      (define simple-scheme-text% (fw:scheme:text-mixin (fw:editor:keymap-mixin fw:text:basic%)))
      
      (fw:preferences:add-panel
       "Check Syntax"
       (let ([delta-panel
              (lambda (sym parent)
                (let* ([delta (fw:preferences:get sym)]
                       [style-name (symbol->string sym)]
                       [h (make-object mred:horizontal-panel% parent '(border))]
                       [c (make-object mred:editor-canvas% h
                            #f
                            (list 'hide-hscroll
                                  'hide-vscroll))]
                       [_ (send c set-line-count 1)]
                       [_ (send c allow-tab-exit #t)]
                       [e (make-object (class simple-scheme-text%
                                         (inherit change-style get-style-list)
                                         (rename [super-after-insert after-insert])
                                         (override after-insert)
                                         (define (after-insert pos offset)
                                           (super-after-insert pos offset)
                                           (let ([style (send (get-style-list)
                                                              find-named-style style-name)])
                                             (change-style style pos (+ pos offset))))
                                         (super-instantiate ())))]
                       [_ (fw:preferences:add-callback sym
                                                       (lambda (sym v)
                                                         (set-slatex-style sym v)
                                                         #t))]
                       [_ (set-slatex-style sym delta)]
                       [make-check
                        (lambda (name on off)
                          (let* ([c (lambda (check command)
                                      (if (send check get-value)
                                          (on)
                                          (off))
                                      (fw:preferences:set sym delta))]
                                 [check (make-object mred:check-box% name h c)])
                            check))]
                       [_ (send c set-editor e)]
                       [short-style-name (substring style-name
                                                    (string-length "drscheme:check-syntax:")
                                                    (string-length style-name))]
                       [_ (send* e
                            (insert short-style-name)
                            (set-position 0))]
                       [slant-check
                        (make-check "Slant"
                                    (lambda ()
                                      (send delta set-style-on 'slant)
                                      (send delta set-style-off 'base))
                                    (lambda ()
                                      (send delta set-style-on 'base)
                                      (send delta set-style-off 'slant)))]
                       [bold-check
                        (make-check "Bold"
                                    (lambda ()
                                      (send delta set-weight-on 'bold)
                                      (send delta set-weight-off 'base))
                                    (lambda ()
                                      (send delta set-weight-on 'base)
                                      (send delta set-weight-off 'bold)))]
                       [underline-check
                        (make-check "Underline"
                                    (lambda ()
                                      (send delta set-underlined-on #t)
                                      (send delta set-underlined-off #f))
                                    (lambda ()
                                      (send delta set-underlined-off #t)
                                      (send delta set-underlined-on #f)))]
                       [color-button
                        (and (>= (mred:get-display-depth) 8)
                             (make-object mred:button%
                               "Change Color"
                               h
                               (lambda (color-button evt)
                                 (let* ([add (send delta get-foreground-add)]
                                        [color (make-object mred:color%
                                                 (send add get-r)
                                                 (send add get-g)
                                                 (send add get-b))]
                                        [users-choice
                                         (mred:get-color-from-user
                                          (format "Choose a color for ~a~a"
                                                  short-style-name
                                                  (if (string=? "syntax" short-style-name)
                                                      ""
                                                      "s"))
                                          (send color-button get-top-level-window)
                                          color)])
                                   (when users-choice
                                     (send delta set-delta-foreground users-choice)
                                     (fw:preferences:set sym delta))))))]
                       [style (send (send e get-style-list) find-named-style style-name)])
                  (send slant-check set-value (eq? (send style get-style) 'slant))
                  (send bold-check set-value (eq? (send style get-weight) 'bold))
                  (send underline-check set-value (send style get-underlined))))])
         (lambda (parent)
           (let ([v (make-object mred:vertical-panel% parent)])
             (for-each (lambda (sym) (delta-panel sym v))
                       delta-symbols)
             v))))
      
      (define-struct graphic (pos* locs->thunks draw-fn click-fn))
      (define-struct arrow (start-pos-left start-pos-right end-pos-left end-pos-right
                                           start-x start-y end-x end-y
                                           id-name rename))
      
      (define tacked-brush (send mred:the-brush-list find-or-create-brush "BLUE" 'solid))
      (define untacked-brush (send mred:the-brush-list find-or-create-brush "WHITE" 'solid))
      (define the-pen (send mred:the-pen-list find-or-create-pen "BLUE" 1 'solid))
      
      (define make-graphics-text%
        (lambda (super%)
          (let* ([cursor-arrow (make-object mred:cursor% 'arrow)])
            (class super%
              (inherit set-cursor get-admin invalidate-bitmap-cache set-position
                       position-location
                       get-canvas last-position dc-location-to-editor-location
                       find-position begin-edit-sequence end-edit-sequence)
              
              (rename
               [super-after-insert after-insert]
               [super-after-delete after-delete]
               [super-on-paint on-paint]
               [super-on-local-event on-local-event])
              
              ;; (union #f (vector (union (-> (union #f menu)) (listof arrow))))
              (define arrow-vector #f)
              
              (field (tacked-hash-table (make-hash-table)))
              (field (cursor-location #f))
              (define (find-poss left-pos right-pos)
                (let ([xlb (box 0)]
                      [ylb (box 0)]
                      [xrb (box 0)]
                      [yrb (box 0)])
                  (position-location left-pos xlb ylb #t)
                  (position-location right-pos xrb yrb #f)
                  (values (/ (+ (unbox xlb) (unbox xrb)) 2)
                          (/ (+ (unbox ylb) (unbox yrb)) 2))))
              (define (update-poss arrow)
                (let-values ([(start-x start-y) (find-poss (arrow-start-pos-left arrow)
                                                           (arrow-start-pos-right arrow))]
                             [(end-x end-y) (find-poss (arrow-end-pos-left arrow)
                                                       (arrow-end-pos-right arrow))])
                  (set-arrow-start-x! arrow start-x)
                  (set-arrow-start-y! arrow start-y)
                  (set-arrow-end-x! arrow end-x)
                  (set-arrow-end-y! arrow end-y)))
              
              (public syncheck:init-arrows)
              (define (syncheck:init-arrows)
                (set! tacked-hash-table (make-hash-table))
                (set! arrow-vector (make-vector (add1 (last-position)) null)))
              (public syncheck:clear-arrows)
              (define (syncheck:clear-arrows)
                (when (or arrow-vector cursor-location)
                  (set! arrow-vector #f)
                  (set! cursor-location #f)
                  (invalidate-bitmap-cache)))
              (public syncheck:add-menu)
              (define (syncheck:add-menu start-pos end-pos make-menu)
                (when (and (<= 0 start-pos end-pos (last-position)))
                  (let loop ([p start-pos])
                    (when (<= p end-pos)
                      (vector-set! arrow-vector p make-menu)
                      (loop (+ p 1))))))
              (public syncheck:add-arrow)
              (define (syncheck:add-arrow start-pos-left start-pos-right
                                          end-pos-left end-pos-right
                                          id-name rename)
                (let* ([arrow (make-arrow start-pos-left start-pos-right
                                          end-pos-left end-pos-right
                                          0 0 0 0
                                          id-name rename)]
                       [add-to-range
                        (lambda (start end)
                          (let loop ([p start])
                            (when (<= p end)
                              (let ([r (vector-ref arrow-vector p)])
                                (vector-set! arrow-vector p (cons arrow (if (list? r) r null))))
                              (loop (add1 p)))))])
                  (add-to-range start-pos-left start-pos-right)
                  (add-to-range end-pos-left end-pos-right)))
              (inherit get-top-level-window)
              (override after-delete)
              (define (after-delete start len)
                (super-after-delete start len)
                (syncheck:clear-arrows))
              
              (override after-insert)
              (define (after-insert start len)
                (super-after-insert start len)
                (syncheck:clear-arrows))
              
              (override on-change)
              (define (on-change)
                (when arrow-vector
                  (let loop ([n (vector-length arrow-vector)])
                    (unless (zero? n)
                      (let ([ele (vector-ref arrow-vector (- n 1))])
                        (when (pair? ele)
                          (for-each update-poss ele)))
                      (loop (- n 1))))
                  (invalidate-bitmap-cache)))
              
              (override on-paint)
              (define (on-paint before dc left top right bottom dx dy draw-caret)
                (super-on-paint before dc left top right bottom dx dy draw-caret)
                (when (and arrow-vector
                           cursor-location
                           (not before))
                  (let ([draw-arrow2
                         (lambda (arrow)
                           (let ([start-x (arrow-start-x arrow)]
                                 [start-y (arrow-start-y arrow)]
                                 [end-x   (arrow-end-x arrow)]
                                 [end-y   (arrow-end-y arrow)])
                             (drscheme:arrow:draw-arrow dc start-x start-y end-x end-y dx dy)))]
                        [old-brush (send dc get-brush)]
                        [old-pen   (send dc get-pen)])
                    (send dc set-pen the-pen)
                    (send dc set-brush tacked-brush)
                    (hash-table-for-each tacked-hash-table
                                         (lambda (arrow v) 
                                           (when v 
                                             (draw-arrow2 arrow))))
                    (send dc set-brush untacked-brush)
                    (let ([ele (vector-ref arrow-vector cursor-location)])
                      (when (pair? ele)
                        (for-each draw-arrow2 ele)))
                    (send dc set-brush old-brush)
                    (send dc set-pen old-pen))))
              
              (define (get-pos event)
                (let*-values ([(event-x event-y)
                               (values (send event get-x)
                                       (send event get-y))]
                              [(x y) (dc-location-to-editor-location
                                      event-x event-y)])
                  (find-position x y)))
              
              (override on-local-event)
              (define (on-local-event event)
                (if arrow-vector
                    (cond
                      [(send event moving?)
                       (let ([pos (get-pos event)])
                         (unless (and cursor-location
                                      (= pos cursor-location))
                           (let ([old-pos pos])
                             (set! cursor-location pos)
                             (let ([ele (vector-ref arrow-vector cursor-location)])
                               (when (pair? ele)
                                 (for-each update-poss ele)))
                             (invalidate-bitmap-cache))))]
                      [(send event button-down? 'right)
                       (let* ([pos (get-pos event)]
                              [arrows (vector-ref arrow-vector pos)])
                         (cond
                           [(null? arrows)
                            (super-on-local-event event)]
                           [else
                            (let ([menu 
                                   (cond
                                     [(procedure? arrows) (arrows)]
                                     [else
                                      (let* ([menu (make-object mred:popup-menu% #f)])
                                        (make-object mred:menu-item%
                                          "Tack/Untack Arrow"
                                          menu
                                          (lambda (item evt) (tack/untack-callback arrows)))
                                        (make-object mred:menu-item%
                                          "Jump"
                                          menu
                                          (lambda (item evt) (jump-callback pos arrows)))
                                        (make-object mred:menu-item%
                                          "Rename"
                                          menu
                                          (lambda (item evt) (rename-callback arrows)))
                                        menu)])])
                              (if menu
                                  (send (get-canvas) popup-menu menu
                                        (+ 1 (inexact->exact (floor (send event get-x))))
                                        (+ 1 (inexact->exact (floor (send event get-y)))))
                                  (super-on-local-event event)))]))]
                      [else (super-on-local-event event)])
                    (super-on-local-event event)))

              ;; tack/untack-callback : (listof arrow) -> void
              ;; callback for the tack/untack menu item
              (define (tack/untack-callback arrows)
                (for-each 
                 (lambda (arrow)
                   (hash-table-put! tacked-hash-table 
                                    arrow 
                                    (not (hash-table-get
                                          tacked-hash-table
                                          arrow
                                          (lambda () #f)))))
                 arrows)
                (invalidate-bitmap-cache))
              
              ;; rename-callback : (listof arrow) -> void
              ;; callback for the rename popup menu item
              (define (rename-callback arrows)
                (unless (null? arrows)
                  (let* ([arrow (car arrows)]
                         [id-name (arrow-id-name arrow)]
                         [new-id 
                          (fw:keymap:call/text-keymap-initializer
                           (lambda ()
                             (mred:get-text-from-user
                              "Rename Identifier"
                              (format "Rename ~a to:" id-name)
                              #f
                              (format "~a" id-name))))])
                    ((arrow-rename arrow) new-id))
                  (invalidate-bitmap-cache)
                  (send (get-top-level-window)
                        syncheck:button-callback)))
              
              ;; jump-callback : (listof arrow) -> void
              ;; callback for the jump popup menu item
              (define (jump-callback pos arrows)
                (unless (null? arrows)
                  (let* ([arrow (car arrows)]
                         [start-pos-left (arrow-start-pos-left arrow)]
                         [start-pos-right (arrow-start-pos-right arrow)]
                         [end-pos-left (arrow-end-pos-left arrow)]
                         [end-pos-right (arrow-end-pos-right arrow)])
                    (if (<= start-pos-left pos start-pos-right)
                        (set-position end-pos-left end-pos-right)
                        (set-position start-pos-left start-pos-right)))))              
              
              (super-instantiate ())))))
      
      (define (make-clear-text super%)
        (class super%
          (super-instantiate ())))
      
      (define syncheck-bitmap
        (drscheme:unit:make-bitmap
         "Check Syntax"
         (build-path (collection-path "icons") "syncheck.bmp")))
      
      (define (make-new-unit-frame% super%)
        (class super%
          (rename [super-clear-annotations clear-annotations])
          (override clear-annotations)
          (define (clear-annotations)
            (super-clear-annotations)
            (syncheck:clear-highlighting))
          
          (inherit get-button-panel 
                   get-definitions-canvas 
                   get-definitions-text 
                   get-directory)
          
          (rename [super-disable-evaluation disable-evaluation]
                  [super-enable-evaluation enable-evaluation])
          (field
            [button-visible? #t])
          
          (override enable-evaluation)
          (define (enable-evaluation)
            (send check-syntax-button enable #t)
            (super-enable-evaluation))

          (override disable-evaluation)
          (define (disable-evaluation)
            (send check-syntax-button enable #f)
            (super-disable-evaluation))
          
          (define (clear-highlighting)
            (send (get-definitions-text) syncheck:clear-arrows)
            (let* ([list (send (get-definitions-text) get-style-list)]
                   [style (send list find-named-style "Standard")])
              (if style
                  (send (get-definitions-text)
                        change-style style 0 (send (get-definitions-text) last-position))
                  (begin
                    (set! clear-highlighting (lambda () (send (get-definitions-text) syncheck:clear-arrows)))
                    (mred:message-box "DrScheme: Syntax Check" "Warning: couldn't find Standard style")))))
          
          (public syncheck:clear-highlighting)
          (define (syncheck:clear-highlighting)
            (hide-error-report-window)
            (clear-highlighting))
          
          (public syncheck:enable-checking)
          (define (syncheck:enable-checking on?)
            (set! button-visible? on?)
            (when (object? check-syntax-button)
              (send check-syntax-button show on?)))
          
          (rename [super-make-root-area-container make-root-area-container])
          (field
           [rest-panel 'uninitialized-root]
           [super-root 'uninitialized-super-root]
           [docs-panel 'uninitialized-docs-panel]
           [docs-messages 'uninitialized-docs-lines])
          (override make-root-area-container)
          (define (make-root-area-container % parent)
            (let* ([s-root (super-make-root-area-container
                            mred:vertical-panel%
                            parent)]
                   [r-root (make-object % s-root)])
              (set! super-root s-root)
              (set! rest-panel r-root)
              (set! docs-panel (make-object mred:vertical-panel% super-root))
              (set! docs-messages null)
              (send docs-panel set-label-font
                    (send mred:the-font-list find-or-create-font 
                          (send (send docs-panel get-label-font) get-point-size)
                          'modern 'normal 'normal #f))
              (send docs-panel stretchable-height #f)
              (send super-root change-children (lambda (l) (list rest-panel)))
              r-root))
          (field
           [docs-messages-shown? #f]
           [docs-lines-shown? #f])
          (define (hide-docs-messages)
            (when docs-messages-shown?
              (set! docs-messages-shown? #f)
              (send super-root change-children
                    (lambda (l)
                      (list rest-panel)))))
          (define (set-docs-messages lines)
            (when (< (length docs-messages) (length lines))
              (set! docs-messages
                    (append
                     docs-messages
                     (let loop ([n (- (length lines) (length docs-messages))])
                       (cond
                         [(zero? n) null]
                         [else
                          (let ([m (make-object mred:message% "" docs-panel)])
                            (send m stretchable-width #t)
                            (cons m (loop (- n 1))))])))))
            (let ([to-be-shown
                   (let loop ([lines lines]
                              [docs-messages docs-messages])
                     (cond
                       [(null? lines) null]
                       [else
                        (send (car docs-messages) set-label (car lines))
                        (cons (car docs-messages)
                              (loop (cdr lines)
                                    (cdr docs-messages)))]))])
              (unless (= (length to-be-shown) (length (send docs-panel get-children)))
                (send docs-panel change-children (lambda (l) to-be-shown)))
              (unless docs-messages-shown?
                (set! docs-messages-shown? #t)
                (send super-root change-children
                      (lambda (l)
                        (list rest-panel docs-panel))))))
          
          (field
            [report-error-frame (make-object mred:frame% "Check Syntax Error" #f 400 10)]
            [report-error-text (make-object mred:text%)]
            [report-error-canvas (make-object mred:editor-canvas% report-error-frame report-error-text
                                   '(hide-hscroll hide-vscroll))]
            [hide-error-report-window (lambda () (send report-error-frame show #f))])
          
          (define (report-error message)
            (send* report-error-text
              (begin-edit-sequence)
              (lock #f)
              (erase)
              (insert message)
              (lock #t)
              (end-edit-sequence))
            (send report-error-frame show #t))
          (send report-error-text hide-caret #t)
          (send report-error-canvas set-line-count 1)
          
          (public syncheck:button-callback)
          (define (syncheck:button-callback)
            (check-syntax (get-definitions-text)))
          
          (super-instantiate ())
          
          (field
           [check-syntax-button
            (make-object mred:button%
              (syncheck-bitmap this)
              (get-button-panel)
              (lambda (button evt) (syncheck:button-callback)))])
          (public syncheck:get-button)
          (define (syncheck:get-button) check-syntax-button)
          (send (get-definitions-text) set-styles-fixed #t)
          (send check-syntax-button show button-visible?)
          (send (get-button-panel) change-children
                (lambda (l)
                  (cons check-syntax-button
                        (remove check-syntax-button l))))))

      (define (check-syntax) (void))
      
      (drscheme:get/extend:extend-definitions-text make-graphics-text%)
      (drscheme:get/extend:extend-unit-frame make-new-unit-frame% #f))))
