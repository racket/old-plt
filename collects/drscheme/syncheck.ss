
(module syncheck mzscheme
  (require (lib "unitsig.ss")
           "tool.ss"
           "default-code-style.ss"
           (lib "class.ss")
           (lib "list.ss")
           (prefix drscheme:arrow: "arrow.ss")
           (prefix fw: (lib "framework.ss" "framework"))
           (lib "mred.ss" "mred"))
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
          (let ([style (make-object style-delta%)])
            (for-each (lambda (fs v) ((cdr fs) style v)) style-delta-get/set info)
            style)))
      
      ;; prefix-style : (union symbol string) -> string
      (define (prefix-style x) (format "drscheme:check-syntax:~a" x))
      
      (define (prefix-style/check x)
        (unless (and (assq x color-default-code-styles)
                     (assq x bw-default-code-styles))
          (error 'prefix-style/check "unknown style: ~e" x))
        (prefix-style x))
      
      (define prefixed-code-styles 
        (map (lambda (x) 
               (cons
                (string->symbol (prefix-style (car x)))
                (cdr x)))
             (if ((get-display-depth) . < . 8)
                 bw-default-code-styles
                 color-default-code-styles)))
      
      (define delta-symbols (map car prefixed-code-styles))
      
      
      ;; all strings naming styles
      (define keyword-style-str (prefix-style/check 'keyword))
      (define unbound-variable-style-str (prefix-style/check 'unbound-variable))
      (define bound-variable-style-str (prefix-style/check 'bound-variable))
      (define primitive-style-str (prefix-style/check 'primitive))
      (define constant-style-str (prefix-style/check 'constant))
      (define base-style-str (prefix-style/check 'base))
      
      (let ([set-default
             (lambda (default)
               (let* ([sym (car default)]
                      [code-style (cadr default)]
                      [color (code-style-color code-style)])
                 (fw:preferences:set-default
                  sym
                  (let ([s (make-object style-delta%)])
                    (send s set-delta-foreground (if (string? color)
                                                     color
                                                     (make-object color%
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
                    (is-a? x style-delta%)))))])
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
           (let ([frame (make-object frame% name)])
             (f frame)
             (send frame show #t))))
      
      (define simple-scheme-text% (fw:scheme:text-mixin (fw:editor:keymap-mixin fw:text:basic%)))
      
      (fw:preferences:add-panel
       "Check Syntax"
       (let ([delta-panel
              (lambda (sym parent)
                (let* ([delta (fw:preferences:get sym)]
                       [style-name (symbol->string sym)]
                       [h (make-object horizontal-panel% parent '(border))]
                       [c (make-object editor-canvas% h
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
                                                              find-named-style
                                                              style-name)])
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
                                 [check (make-object check-box% name h c)])
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
                        (and (>= (get-display-depth) 8)
                             (make-object button%
                               "Change Color"
                               h
                               (lambda (color-button evt)
                                 (let* ([add (send delta get-foreground-add)]
                                        [color (make-object color%
                                                 (send add get-r)
                                                 (send add get-g)
                                                 (send add get-b))]
                                        [users-choice
                                         (get-color-from-user
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
           (let ([v (make-object vertical-panel% parent)])
             (for-each (lambda (sym) (delta-panel sym v))
                       delta-symbols)
             v))))
      
      (define-struct graphic (pos* locs->thunks draw-fn click-fn))
      (define-struct arrow (start-pos-left start-pos-right end-pos-left end-pos-right
                                           start-x start-y end-x end-y
                                           id-name same-ids))
      
      (define tacked-brush (send the-brush-list find-or-create-brush "BLUE" 'solid))
      (define untacked-brush (send the-brush-list find-or-create-brush "WHITE" 'solid))
      (define the-pen (send the-pen-list find-or-create-pen "BLUE" 1 'solid))
      
      (define syncheck-text<%>
        (interface ()
          syncheck:init-arrows
          syncheck:clear-arrows
          syncheck:add-menu
          syncheck:add-arrow))
      
      (define make-graphics-text%
        (lambda (super%)
          (let* ([cursor-arrow (make-object cursor% 'arrow)])
            (class* super% (syncheck-text<%>)
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
                                          id-name same-names)
                (let* ([arrow (make-arrow start-pos-left start-pos-right
                                          end-pos-left end-pos-right
                                          0 0 0 0
                                          id-name 
                                          same-names)]
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
                                      (let* ([menu (make-object popup-menu% #f)])
                                        (make-object menu-item%
                                          "Tack/Untack Arrow"
                                          menu
                                          (lambda (item evt) (tack/untack-callback arrows)))
                                        (make-object menu-item%
                                          "Jump"
                                          menu
                                          (lambda (item evt) (jump-callback pos arrows)))
                                        (make-object menu-item%
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
                             (get-text-from-user
                              "Rename Identifier"
                              (format "Rename ~a to:" id-name)
                              #f
                              (format "~a" id-name))))])
                    (when new-id
                      (let ([to-be-renamed (quicksort 
                                            (arrow-same-ids arrow)
                                            (lambda (x y) 
                                              ((syntax-position x) . >= . (syntax-position y))))])
                        (unless (null? to-be-renamed)
                          (let ([first-one-source (syntax-source (car to-be-renamed))])
                            (when (is-a? first-one-source text%)
                              (send first-one-source begin-edit-sequence)
                              (for-each (lambda (stx) 
                                          (let ([source (syntax-source stx)])
                                            (when (is-a? source text%)
                                              (let* ([start (- (syntax-position stx) 1)]
                                                     [end (+ start (syntax-span stx))])
                                                (send source delete start end #f)
                                                (send source insert new-id start start #f)))))
                                        to-be-renamed)
                              (invalidate-bitmap-cache)
                              (send (get-top-level-window) syncheck:button-callback)
                              (send first-one-source end-edit-sequence)))))))))
              
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
                   get-interactions-text
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
              (when style
                (send (get-definitions-text) change-style
                      style 0 (send (get-definitions-text) last-position)))))
          
          (public syncheck:clear-highlighting)
          (define (syncheck:clear-highlighting)
            (hide-error-report)
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
           [docs-panel-visible? #f]
           [docs-messages 'uninitialized-docs-lines]
           [report-error-panel 'uninitialized-report-error-panel]
           [report-error-panel-visible? #t]
           [report-error-text (make-object fw:text:hide-caret/selection%)])
          (override make-root-area-container)
          (define (make-root-area-container % parent)
            (let* ([s-root (super-make-root-area-container
                            vertical-panel%
                            parent)]
                   [r-root (make-object % s-root)])
              (set! super-root s-root)
              (set! rest-panel r-root)
              (set! docs-panel (make-object vertical-panel% super-root))
              (set! docs-messages null)
              (send docs-panel set-label-font
                    (send the-font-list find-or-create-font 
                          (send (send docs-panel get-label-font) get-point-size)
                          'modern 'normal 'normal #f))
              (send docs-panel stretchable-height #f)
              
              (set! report-error-panel (instantiate horizontal-panel% ()
                                         (parent super-root)
                                         (stretchable-height #f)))
              (let ([message-panel (instantiate vertical-panel% ()
                                     (parent report-error-panel)
                                     (stretchable-width #f)
                                     (alignment '(left center)))])
                (make-object message% "Check Syntax" message-panel)
                (make-object message% "Error Message" message-panel))
              (send (make-object editor-canvas% report-error-panel report-error-text) set-line-count 2)

              (update-docs/report-error-visibility)
              
              r-root))

          (define (update-docs/report-error-visibility)
            (send super-root change-children 
                  (lambda (l) 
                    (let* ([first (if docs-panel-visible?
                                      (list docs-panel)
                                      null)]
                           [snd (cons rest-panel first)]
                           [thrd (if report-error-panel-visible?
                                     (cons report-error-panel snd)
                                     snd)])
                      thrd))))


          (define (hide-docs-messages)
            (when docs-panel-visible?
              (set! docs-panel-visible? #f)
              (update-docs/report-error-visibility)))
          (define (set-docs-messages lines)
            (when (< (length docs-messages) (length lines))
              (set! docs-messages
                    (append
                     docs-messages
                     (let loop ([n (- (length lines) (length docs-messages))])
                       (cond
                         [(zero? n) null]
                         [else
                          (let ([m (make-object message% "" docs-panel)])
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
              (unless docs-panel-visible?
                (set! docs-panel-visible? #t)
                (update-docs/report-error-visibility))))
           
          (define (hide-error-report) 
            (when report-error-panel-visible?
              (set! report-error-panel-visible? #f)
              (update-docs/report-error-visibility)))
          
          (define (show-error-report) 
            (unless report-error-panel-visible?
              (set! report-error-panel-visible? #t)
              (update-docs/report-error-visibility)))
          
          (define (report-error message exn)
            (send* report-error-text
              (begin-edit-sequence)
              (lock #f)
              (erase)
              (insert message)
              (change-style
               report-error-style
               0
               (send report-error-text last-position))
              (lock #t)
              (end-edit-sequence))
            (show-error-report))
          
          (public syncheck:button-callback)
          (define (syncheck:button-callback)
            (send (get-definitions-text) begin-edit-sequence)
            (color-range (get-definitions-text)
                         0
                         (send (get-definitions-text) last-position)
                         base-style-str)
            (let ([binders null]
                  [varrefs null]
                  [tops null]
                  [users-namespace #f]
                  [err-termination? #f])
              (send (get-interactions-text)
                    expand-program
                    (drscheme:language:make-text/pos (get-definitions-text) 
                                                     0
                                                     (send (get-definitions-text)
                                                           last-position))
                    (fw:preferences:get
                     (drscheme:language-configuration:get-settings-preferences-symbol))
                    (lambda (err? sexp run-in-evaluation-thread loop)
                      (unless users-namespace
                        (set! users-namespace (run-in-evaluation-thread current-namespace)))
                      (if err?
                          (begin
                            (set! err-termination? #t)
                            (report-error (car sexp) (cdr sexp)))
                          (let-values ([(new-binders new-varrefs new-tops) (annotate-basic sexp)])
                            (set! binders (append new-binders binders))
                            (set! varrefs (append new-varrefs varrefs))
                            (set! tops (append new-varrefs tops))))
                      (loop)))
              (unless err-termination? 
                (annotate-variables users-namespace binders varrefs tops)))
            (send (get-definitions-text) end-edit-sequence))

          ;; annotate-variables : namespace (listof syntax) (listof syntax) -> void
          ;; colors the variables, free are turned unbound color, bound are turned
          ;; bound color and all binders are turned bound color.
          (define (annotate-variables users-namespace binders varrefs tops)
            (send (get-definitions-text) syncheck:init-arrows)
            (for-each (lambda (binder) 
                        (when (syntax-original? binder)
                          (color binder bound-variable-style-str)))
                      binders)
            (for-each (annotate-varref handle-no-binders/lexical binders varrefs) varrefs)
            (for-each (annotate-varref (handle-no-binders/top users-namespace) binders varrefs) tops))
          
          ;; annotate-varref : (listof syntax) -> syntax -> void
          ;; annotates a variable reference with either green or red,
          ;; and adds the arrows from the varref to the 
          ;; (possibly multiple) binding locations.
          (define (annotate-varref handle-no-binders binders all-varrefs)
            (lambda (varref)
              (when (syntax-original? varref)
                (let* ([same-as-varref? (lambda (x) (module-identifier=? x varref))]
                       [binders (filter same-as-varref? binders)]
                       [same-names (append binders (filter same-as-varref? all-varrefs))])
                  (cond
                    [(null? binders) (handle-no-binders varref)]
                    [else
                     (for-each 
                      (lambda (binder) 
                        (when (syntax-original? binder)
                          (connect-variables binder varref same-names)))
                      binders)
                     (color varref bound-variable-style-str)])))))
          
          ;; handle-no-binders/top : top-level-info -> syntax[original] -> void
          (define (handle-no-binders/top users-namespace)
            (lambda (varref)
              (let ([defined-in-users-namespace?
                     (with-handlers ([exn:variable? (lambda (x) #f)])
                       (parameterize ([current-namespace users-namespace])
                         (eval (syntax-e varref))
                         #t))])
                (if defined-in-users-namespace?
                    (color varref bound-variable-style-str)
                    (color varref unbound-variable-style-str)))))
          
          ;; handle-no-binders/lexical : syntax[original] -> void
          (define (handle-no-binders/lexical varref)
            (color varref unbound-variable-style-str))
          
          ;; connect-variable : syntax syntax (listof syntax) -> void
          (define (connect-variables binding bound same-names)
            (let* ([binding-source (syntax-source binding)]
                   [binding-pos-left (- (syntax-position binding) 1)]
                   [binding-pos-right (+ binding-pos-left (syntax-span binding))]
                   [bound-pos-left (- (syntax-position bound) 1)]
                   [bound-pos-right (+ bound-pos-left (syntax-span bound))])
              (when (is-a? binding-source syncheck-text<%>)
                (send binding-source syncheck:add-arrow
                      binding-pos-left binding-pos-right
                      bound-pos-left bound-pos-right
                      (syntax-object->datum binding)
                      same-names))))
          
          ;; annotate-basic : syntax -> (values (listof syntax) (listof syntax)
          ;; annotates the lexical structure of the program `sexp', except
          ;; for the variables in the program. returns the variables in two
          ;; lists -- the first is the ones that occur in binding positions
          ;; and the second is those that occur in bound positions
          (define (annotate-basic sexp)
            (let ([binders null]
                  [varrefs null]
                  [tops null])
              (let loop ([sexp sexp])
                (annotate-original-keywords sexp)
                (syntax-case sexp (lambda case-lambda if begin begin0 let-value letrec-values set!
                                    quote quote-syntax with-continuation-mark 
                                    #%app #%datum #%top #%module-begin
                                    define-values define-syntaxes module
                                    require require-for-syntax provide)
                  [(lambda args bodies ...)
                   (begin
                     (annotate-raw-keyword sexp)
                     (set! binders (combine-binders (syntax args) binders))
                     (for-each loop (syntax->list (syntax (bodies ...)))))]
                  [(case-lambda [argss bodiess ...]...)
                   (begin
                     (annotate-raw-keyword sexp)
                     (for-each
                      (lambda (args bodies)
                        (set! binders (combine-binders args binders))
                        (for-each loop (syntax->list bodies)))
                      (syntax->list (syntax (argss ...)))
                      (syntax->list (syntax ((bodiess ...) ...)))))]
                  [(if test then else)
                   (begin
                     (annotate-raw-keyword sexp)
                     (loop (syntax test))
                     (loop (syntax then))
                     (loop (syntax else)))]
                  [(if test then)
                   (begin
                     (annotate-raw-keyword sexp)
                     (loop (syntax test))
                     (loop (syntax then)))]
                  [(begin bodies ...)
                   (begin
                     (annotate-raw-keyword sexp)
                     (for-each loop (syntax->list (syntax (bodies ...)))))]
                  
                  [(begin0 bodies ...)
                   (begin
                     (annotate-raw-keyword sexp)
                     (for-each loop (syntax->list (syntax (bodies ...)))))]
                  
                  [(let-values (((xss ...) es) ...) bs ...)
                   (begin
                     (annotate-raw-keyword sexp)
                     (for-each (lambda (x) (set! binders (combine-binders x binders)))
                               (syntax->list (syntax ((xss ...) ...))))
                     (for-each loop (syntax->list (syntax (es ...))))
                     (for-each loop (syntax->list (syntax (bs ...)))))]
                  [(letrec-values (((xss ...) es) ...) bs ...)
                   (begin
                     (annotate-raw-keyword sexp)
                     (for-each (lambda (x) (set! binders (combine-binders x binders)))
                               (syntax->list (syntax ((xss ...) ...))))
                     (for-each loop (syntax->list (syntax (es ...))))
                     (for-each loop (syntax->list (syntax (bs ...)))))]
                  [(set! var E)
                   (begin
                     (annotate-raw-keyword sexp)
                     (set! varrefs (cons (syntax var) varrefs))
                     (loop (syntax E)))]
                  [(quote datum)
                   (begin 
                     (when (syntax-original? sexp)
                       (annotate-raw-keyword sexp))
                     (when (syntax-original? (syntax datum))
                       (color (syntax datum) constant-style-str)))]
                  [(quote-syntax datum)
                   (begin 
                     (when (syntax-original? sexp)
                       (annotate-raw-keyword sexp))
                     (when (syntax-original? (syntax datum))
                       (color (syntax datum) constant-style-str)))]
                  [(with-continuation-mark a b c)
                   (begin
                     (annotate-raw-keyword sexp)
                     (loop (syntax a))
                     (loop (syntax b))
                     (loop (syntax c)))]
                  [(#%app pieces ...)
                   (begin
                     (annotate-raw-keyword sexp)
                     (for-each loop (syntax->list (syntax (pieces ...)))))]
                  [(#%datum . datum)
                   (begin
                     (when (syntax-original? sexp)
                       (color sexp constant-style-str)))]
                  [(#%top . var)
                   (begin
                     (set! tops (cons (syntax var) tops)))]
                  
                  [(define-values vars b)
                   (begin
                     (annotate-raw-keyword sexp)
                     (set! binders (combine-binders (syntax vars) binders))
                     (loop (syntax b)))]
                  [(define-syntaxes names exp)
                   (begin
                     (annotate-raw-keyword sexp)
                     (set! binders (combine-binders (syntax names) binders))
                     (loop (syntax exp)))]
                  [(module m-name lang (#%module-begin bodies ...))
                   (begin
                     (annotate-raw-keyword sexp)
                     (for-each loop (syntax->list (syntax (bodies ...)))))]
                  
                  ; top level or module top level only:
                  [(require require-spec)
                   (begin
                     (annotate-raw-keyword sexp))]
                  [(require-for-syntax require-spec)
                   (begin
                     (annotate-raw-keyword sexp))]
                  
                  ; module top level only:
                  [(provide vars)
                   (begin
                     (annotate-raw-keyword sexp))]
                  
                  [_
                   (begin
                     (cond
                       [(identifier? sexp)
                        (set! varrefs (cons sexp varrefs))]
                       [else 
;                        (printf "unknown stx: ~e (datum: ~e) (source: ~e)~n"
;                                 sexp
;                                 (and (syntax? sexp)
;                                      (syntax-object->datum sexp))
;                                 (and (syntax? sexp)
;                                      (syntax-source sexp)))
                        (void)
                        ]))]))
              (values binders varrefs tops)))

          ;; combine-binders : syntax (listof syntax) -> (listof syntax)
          ;; transforms an argument list into a bunch of symbols/symbols and puts 
          ;; them on `incoming'
          ;; [could be more efficient if it processed the stx itself instead of append]
          (define (combine-binders stx incoming)
            (let ([lst (syntax->list stx)])
              (if lst
                  (append lst incoming)
                  (cons stx incoming)))
;            (if (null? stx)
;                incoming
;                (let ([first (syntax-e stx)])
;                  (cond
;                    [(cons? first) (args->vars (cdr first) (cons (car first) incoming))]
;                    [(null? first) incoming]
;                    [else (cons first incoming)])))
            )

          
          ;; annotate-original-keywords : syntax -> void
          ;; annotates the origin of the stx with style-name's style.
          (define (annotate-original-keywords stx)
            (let* ([origin (syntax-property stx 'origin)])
              (when origin
                (let loop ([origin origin])
                  (cond
                    [(cons? origin)
                     (loop (car origin))
                     (loop (cdr origin))]
                    [(syntax? origin)
                     (when (syntax-original? origin)
                       (color origin keyword-style-str))])))))

          ;; annotate-raw-keyword : syntax -> void
          ;; annotates keywords when they were never expanded. eg.
          ;; if someone just types `(lambda (x) x)' it has no 'origin
          ;; field, but there still are keywords.
          (define (annotate-raw-keyword stx)
            (unless (syntax-property stx 'origin)
              (let ([lst (syntax-e stx)])
                (when (pair? lst)
                  (let ([f-stx (car lst)])
                    (when (syntax-original? f-stx)
                      (color f-stx keyword-style-str)))))))
          
          ;; annotate-variable : syntax (listof identifer) -> void
          (define (annotate-variable sexp bound-vars)
            (cond
              [(ormap (lambda (x) (and (module-identifier=? sexp x) x)) bound-vars)
               (when (syntax-original? sexp)
                 (color sexp bound-variable-style-str))]
              [else
               (when (syntax-original? sexp)
                 (color sexp unbound-variable-style-str))]))

          ;; annotate-arglist : syntax -> void
          ;; annotates the (possibly improper) syntax list as bound variables
          (define (annotate-arglist stx)
            (for-each (lambda (stx) 
                        (when (syntax-original? stx)
                          (color stx bound-variable-style-str)))
                      (syntax->list stx)))
          
          ;; color : syntax str -> void
          ;; colors the syntax with style-name's style
          (define (color stx style-name)
            (let ([source (syntax-source stx)])
              (when (is-a? source text%)
                (let ([pos (- (syntax-position stx) 1)]
                      [span (syntax-span stx)])
                  (color-range source pos (+ pos span) style-name)))))

          ;; color-range : text start finish style-name 
          ;; colors a range in the text based on `style-name'
          (define (color-range source start finish style-name)
            (let ([style (send (send source get-style-list)
                               find-named-style
                               style-name)])
              (send source change-style style start finish)))
          
          (super-instantiate ())
          
          (field
           [check-syntax-button
            (make-object button%
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
      
      (define report-error-style 
        (send (make-object style-delta% 'change-family 'modern)
              set-delta
              'change-italic))
      (send report-error-style set-delta-foreground "red")

      (drscheme:get/extend:extend-definitions-text make-graphics-text%)
      (drscheme:get/extend:extend-unit-frame make-new-unit-frame% #f))))
