#|

  TODO:
       
     - write test suite for arrows and menus
|#

(module syncheck mzscheme
  (require (lib "string-constant.ss" "string-constants")
           (lib "unitsig.ss")
           "tool.ss"
           "default-code-style.ss"
           (lib "class.ss")
           (lib "list.ss")
           (prefix drscheme:arrow: "arrow.ss")
           (prefix fw: (lib "framework.ss" "framework"))
           (lib "mred.ss" "mred"))
  (provide tool@)

  (define o (current-output-port))
  
  (define tool@
    (unit/sig ()
      (import drscheme:tool^)

      (define (printf . args) (apply fprintf o args))


                     
                     
                      ;;;  ;;; ;;; ;;;;; 
                     ;   ;  ;   ;    ;   
                     ;   ;  ;   ;    ;   
                     ;      ;   ;    ;   
                     ;  ;;  ;   ;    ;   
                     ;   ;  ;   ;    ;   
                     ;   ;  ;; ;;    ;   
                      ;;;    ;;;   ;;;;; 
                     
                     
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
       (string-constant check-syntax)
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
                        (make-check (string-constant cs-italic)
                                    (lambda ()
                                      (send delta set-style-on 'slant)
                                      (send delta set-style-off 'base))
                                    (lambda ()
                                      (send delta set-style-on 'base)
                                      (send delta set-style-off 'slant)))]
                       [bold-check
                        (make-check (string-constant cs-bold)
                                    (lambda ()
                                      (send delta set-weight-on 'bold)
                                      (send delta set-weight-off 'base))
                                    (lambda ()
                                      (send delta set-weight-on 'base)
                                      (send delta set-weight-off 'bold)))]
                       [underline-check
                        (make-check (string-constant cs-underline)
                                    (lambda ()
                                      (send delta set-underlined-on #t)
                                      (send delta set-underlined-off #f))
                                    (lambda ()
                                      (send delta set-underlined-off #t)
                                      (send delta set-underlined-on #f)))]
                       [color-button
                        (and (>= (get-display-depth) 8)
                             (make-object button%
                               (string-constant cs-change-color)
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
                                           start-x start-y end-x end-y))
      
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
              
              ;; arrow-vector : (union #f (vector (listof (union (cons (union #f sym) (menu -> void))
              ;;                                                 arrow))))
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
              
              (define/public (syncheck:init-arrows)
                (set! tacked-hash-table (make-hash-table))
                (set! arrow-vector (make-vector (add1 (last-position)) null)))
              (define/public (syncheck:clear-arrows)
                (when (or arrow-vector cursor-location)
                  (set! arrow-vector #f)
                  (set! cursor-location #f)
                  (invalidate-bitmap-cache)))
              (define/public (syncheck:add-menu start-pos end-pos key make-menu)
                (when (and (<= 0 start-pos end-pos (last-position)))
                  (add-to-range/key start-pos end-pos make-menu key #t)))
              (define/public (syncheck:add-arrow start-pos-left start-pos-right
                                                 end-pos-left end-pos-right)
                (let* ([arrow (make-arrow start-pos-left start-pos-right
                                          end-pos-left end-pos-right
                                          0 0 0 0)])
                  (add-to-range/key start-pos-left start-pos-right arrow #f #f)
                  (add-to-range/key end-pos-left end-pos-right arrow #f #f)))

              (define (add-to-range/key start end to-add key use-key?)
                (let loop ([p start])
                  (when (<= p end)
                    (let ([r (vector-ref arrow-vector p)])
                      (cond
                        [use-key?
                         (unless (ormap (lambda (x) (and (pair? x) 
                                                         (car x)
                                                         (eq? (car x) key)))
                                        r)
                           (vector-set! arrow-vector p (cons (cons key to-add) r)))]
                        [else
                         (vector-set! arrow-vector p (cons to-add r))]))
                    (loop (add1 p)))))

              (inherit get-top-level-window)
              (override after-delete)
              (define (after-delete start len)
                (super-after-delete start len)
                (syncheck:clear-arrows))
              
              (override after-insert)
              (define (after-insert start len)
                (super-after-insert start len)
                (syncheck:clear-arrows))
              
              ;; flush-arrow-coordinates-cache : -> void
              ;; pre-condition: arrow-vector is not #f.
              (define/private (flush-arrow-coordinates-cache)
                (let loop ([n (vector-length arrow-vector)])
                  (unless (zero? n)
                    (let ([eles (vector-ref arrow-vector (- n 1))])
                      (for-each (lambda (ele)
                                  (when (arrow? ele)
                                    (update-poss ele)))
                                eles))
                    (loop (- n 1)))))
              
              (rename [super-on-change on-change])
              (define/override (on-change)
                (super-on-change)
                (when arrow-vector
                  (flush-arrow-coordinates-cache)
                  (invalidate-bitmap-cache)))
              
              (override on-paint)
              (define (on-paint before dc left top right bottom dx dy draw-caret)
                (super-on-paint before dc left top right bottom dx dy draw-caret)
                (when (and arrow-vector 
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
                    (when cursor-location
                      (send dc set-brush untacked-brush)
                      (let ([eles (vector-ref arrow-vector cursor-location)])
                        (for-each (lambda (ele) 
                                    (when (arrow? ele)
                                      (draw-arrow2 ele)))
                                  eles)))
                    (send dc set-brush old-brush)
                    (send dc set-pen old-pen))))
              
              (define (get-pos event)
                (let*-values ([(event-x event-y)
                               (values (send event get-x)
                                       (send event get-y))]
                              [(x y) (dc-location-to-editor-location
                                      event-x event-y)])
                  (let* ([on-it? (box #f)]
                         [pos (find-position x y #f on-it?)])
                    (and (unbox on-it?)
                         pos))))
              
              (define/override (on-local-event event)
                (if arrow-vector
                    (cond
                      [(send event leaving?)
                       (when cursor-location
                         (set! cursor-location #f)
                         (invalidate-bitmap-cache))
                       (super-on-local-event event)]
                      [(or (send event moving?)
                           (send event entering?))
                       (let ([pos (get-pos event)])
                         (cond
                           [pos
                            (unless (and cursor-location
                                         (= pos cursor-location))
                              (set! cursor-location pos)
                              (let ([eles (vector-ref arrow-vector cursor-location)])
                                (for-each (lambda (ele)
                                            (when (arrow? ele)
                                              (update-poss ele)))
                                          eles))
                              (invalidate-bitmap-cache))]
                           [else
                            (when cursor-location
                              (set! cursor-location #f)
                              (invalidate-bitmap-cache))]))
                       (super-on-local-event event)]
                      [(or (send event button-down? 'right)
                           (and (send event button-down? 'left)
                                (send event get-control-down)))
                       (let* ([pos (get-pos event)])
                         (if pos
                             (let ([vec-ents (vector-ref arrow-vector pos)])
                               (cond
                                 [(null? vec-ents)
                                  (super-on-local-event event)]
                                 [else
                                  (let ([menu (make-object popup-menu% #f)]
                                        [arrows (filter arrow? vec-ents)]
                                        [add-menus (map cdr (filter cons? vec-ents))])
                                    (unless (null? arrows)
                                      (make-object menu-item%
                                        (string-constant cs-tack/untack-arrow)
                                        menu
                                        (lambda (item evt) (tack/untack-callback arrows)))
                                      (make-object menu-item%
                                        (string-constant cs-jump)
                                        menu
                                        (lambda (item evt) (jump-callback pos arrows))))
                                    (for-each (lambda (f) (f menu)) add-menus)
                                    (send (get-canvas) popup-menu menu
                                          (+ 1 (inexact->exact (floor (send event get-x))))
                                          (+ 1 (inexact->exact (floor (send event get-y))))))]))
                             (super-on-local-event event)))]
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
         (string-constant check-syntax)
         (build-path (collection-path "icons") "syncheck.bmp")))
      
      (define (make-new-unit-frame% super%)
        (class super%
          (rename [super-clear-annotations clear-annotations])
          (define/override (clear-annotations)
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
          
          (define/override (enable-evaluation)
            (send check-syntax-button enable #t)
            (super-enable-evaluation))

          (define/override (disable-evaluation)
            (send check-syntax-button enable #f)
            (super-disable-evaluation))
          
          (define/public (syncheck:clear-highlighting)
            (hide-error-report)
            (let* ([definitions (get-definitions-text)]
                   [locked? (send definitions is-locked?)])
              (send definitions begin-edit-sequence #f)
              (send definitions lock #f)
              (send definitions syncheck:clear-arrows)
              (let* ([list (send definitions get-style-list)]
                     [style (send list find-named-style "Standard")])
                (when style
                  (send definitions change-style
                        style 0 (send (get-definitions-text) last-position))))
              (send definitions lock locked?)
              (send definitions end-edit-sequence)))
          
          (field
           [report-error-parent-panel 'uninitialized-report-error-parent-panel]
           [report-error-panel 'uninitialized-report-error-panel]
           [report-error-text (make-object (fw:scheme:text-mixin 
                                            (fw:editor:keymap-mixin
                                             fw:text:hide-caret/selection%)))])
          (send report-error-text auto-wrap #t)
          (send report-error-text set-autowrap-bitmap #f)
          (send report-error-text lock #t)
          (rename [super-get-definitions/interactions-panel-parent 
                   get-definitions/interactions-panel-parent])
          (define/override (get-definitions/interactions-panel-parent)
            (set! report-error-parent-panel
                  (make-object vertical-panel%
                    (super-get-definitions/interactions-panel-parent)))
            (set! report-error-panel (instantiate horizontal-panel% ()
                                       (parent report-error-parent-panel)
                                       (stretchable-height #f)
                                       (alignment '(center center))
                                       (style '(border))))
            (send report-error-parent-panel change-children (lambda (l) null))
            (let ([message-panel (instantiate vertical-panel% ()
                                   (parent report-error-panel)
                                   (stretchable-width #f)
                                   (stretchable-height #f)
                                   (alignment '(left center)))])
              (make-object message% (string-constant check-syntax) message-panel)
              (make-object message% (string-constant cs-error-message) message-panel))
            (let ([editor-canvas (make-object editor-canvas% 
                                   report-error-panel
                                   report-error-text
                                   '(no-hscroll))])
              (send editor-canvas set-line-count 3))
            (instantiate button% () 
              (label (string-constant hide))
              (parent report-error-panel)
              (callback (lambda (x y) (hide-error-report)))
              (stretchable-height #t))
            (make-object vertical-panel% report-error-parent-panel))
          
          (define (hide-error-report) 
            (when (member report-error-panel (send report-error-parent-panel get-children))
              (send report-error-parent-panel change-children
                    (lambda (l) (remq report-error-panel l)))))
          
          (define (show-error-report) 
            (unless (member report-error-panel (send report-error-parent-panel get-children))
              (send report-error-parent-panel change-children
                    (lambda (l) (cons report-error-panel l)))))
          
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
              (set-position 0 0)
              (lock #t)
              (end-edit-sequence))

            (let ([do-highlight
                   (lambda (src start span)
                     (when (and (is-a? src fw:text:basic<%>)
                                (number? start)
                                (number? span))
                       (send (get-interactions-text) highlight-error
                             src
                             (- start 1)
                             (+ start span -1))))])
              (cond
                [(exn:read? exn)
                 (do-highlight (exn:read-source exn)
                               (exn:read-position exn)
                               (exn:read-span exn))]
                [(exn:syntax? exn)
                 (let ([stx (exn:syntax-expr exn)])
                   (when stx
                     (do-highlight (syntax-source stx)
                                   (syntax-position stx)
                                   (syntax-span stx))))]
                [else (void)]))
            
            (show-error-report))
          
          (rename [super-make-root-area-container make-root-area-container])
          (field
           [rest-panel 'uninitialized-root]
           [super-root 'uninitialized-super-root]
           [docs-panel 'uninitialized-docs-panel]
           [docs-panel-visible? #f]
           [docs-messages 'uninitialized-docs-lines])
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

              (update-docs-visibility)
              
              r-root))

          (define (update-docs-visibility)
            (send super-root change-children 
                  (lambda (l) 
                    (let* ([first (if docs-panel-visible?
                                      (list docs-panel)
                                      null)]
                           [snd (cons rest-panel first)])
                      snd))))

          (define (hide-docs-messages)
            (when docs-panel-visible?
              (set! docs-panel-visible? #f)
              (update-docs-visibility)))
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
                (update-docs-visibility))))
          
          (public syncheck:button-callback)
          (define (syncheck:button-callback)
            (let* ([definitions (get-definitions-text)]
                   [locked? (send definitions is-locked?)])
              (send definitions begin-edit-sequence #f)
              (send definitions lock #f)
              (clear-annotations)
              (send definitions syncheck:init-arrows)            
              (color-range definitions
                           0
                           (send (get-definitions-text) last-position)
                           base-style-str)
              (let ([tl-binders null]
                    [tl-varrefs null]
                    [tl-requires null]
                    [tl-require-for-syntaxes null]
                    [tl-tops null]
                    [tl-referenced-macros null]
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
                      (lambda (err? sexp run-in-expansion-thread loop)
                        (unless users-namespace
                          (set! users-namespace (run-in-expansion-thread current-namespace)))
                        (cond
                          [err?
                           (begin
                             (set! err-termination? #t)
                             (syncheck:clear-highlighting)
                             (report-error (car sexp) (cdr sexp)))]
                          [(eof-object? sexp)
                           (custodian-shutdown-all (run-in-expansion-thread current-custodian))]
                          [else
                           (let-values ([(new-binders
                                          new-varrefs
                                          new-tops
                                          new-requires
                                          new-require-for-syntaxes
                                          new-referenced-macros
                                          has-module?)
                                         (annotate-basic sexp run-in-expansion-thread)])
                             (if has-module?
                                 (annotate-complete users-namespace
                                                    new-binders
                                                    new-varrefs
                                                    new-tops
                                                    new-requires
                                                    new-require-for-syntaxes
                                                    new-referenced-macros)
                                 (begin
                                   (set! tl-binders (append new-binders tl-binders))
                                   (set! tl-varrefs (append new-varrefs tl-varrefs))
                                   (set! tl-requires (append new-requires tl-requires))
                                   (set! tl-require-for-syntaxes (append new-require-for-syntaxes tl-require-for-syntaxes))
                                   (set! tl-referenced-macros (append new-referenced-macros tl-referenced-macros))
                                   (set! tl-tops (append new-tops tl-tops)))))
                           (loop)])))
                (unless err-termination? 
                  (annotate-complete users-namespace
                                     tl-binders
                                     tl-varrefs
                                     tl-tops
                                     tl-requires
                                     tl-require-for-syntaxes
                                     tl-referenced-macros)))
              (send definitions lock locked?)
              (send definitions end-edit-sequence)))

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
      
      (define report-error-style (make-object style-delta% 'change-style 'slant))
      (send report-error-style set-delta-foreground "red")
      

                                          
                                          
  ;;;;                 ;                  
 ;   ;                 ;                  
 ;     ;;; ;;;; ;;;   ;;;;;  ;;;;  ;;; ;;;
 ;;;    ;   ;  ;;  ;   ;         ;   ; ;  
   ;;;  ;   ;  ;   ;   ;      ;;;;    ;   
     ;   ; ;   ;   ;   ;     ;   ;   ; ;  
 ;   ;   ;;;   ;   ;   ;   ; ;   ;  ;   ; 
 ;;;;     ;   ;;;  ;;   ;;;   ;;; ;;;   ;;
          ;                               
          ;                               
        ;;                                

 
                                                                      
                                                         ;;;          
;;;;;;;                                                    ;          
;  ;  ;                                                    ;          
;  ;  ; ; ;;;  ;;;;  ;;; ;;;  ;;;   ; ;;;   ;;;   ;;;;     ;     ;;;  
   ;     ;         ;  ;   ;  ;   ;   ;     ;   ;      ;    ;    ;   ; 
   ;     ;      ;;;;  ;   ;  ;;;;;   ;      ;;;    ;;;;    ;     ;;;  
   ;     ;     ;   ;   ; ;   ;       ;         ;  ;   ;    ;        ; 
   ;     ;     ;   ;   ;;;   ;   ;   ;     ;   ;  ;   ;    ;    ;   ; 
 ;;;;;  ;;;;    ;;; ;   ;     ;;;   ;;;;    ;;;    ;;; ; ;;;;;;  ;;;  
                                                                      
                                                                      
                                                                      

      
      ;; type req/tag = (make-req/tag syntax sexp boolean)
      (define-struct req/tag (req-stx req-sexp used?))
      
      ;; annotate-complete :    namespace
      ;;                        (listof syntax)
      ;;                        (listof (cons boolean syntax))
      ;;                        (listof syntax)
      ;;                        (listof syntax)
      ;;                        (listof (cons boolean syntax[original]))
      ;;                     -> void
      ;;
      ;; annotates the non-local portions of a complete program.
      ;; for the purposes of check syntax, a complete program is either
      ;; a module expression, or everything at the top level.
      ;;
      ;; the inputs match the outputs of annotate-basic, except this
      ;; accepts the user's namespace in addition and doesn't accept
      ;; the boolean from annotate-basic.
      (define (annotate-complete users-namespace
                                 binders
                                 varrefs
                                 tops
                                 requires
                                 require-for-syntaxes
                                 referenced-macros)
        (let-values ([(non-require-varrefs non-require-referenced-macros)
                      (annotate-require-vars 
                       varrefs
                       requires
                       require-for-syntaxes
                       referenced-macros)])
          (annotate-variables users-namespace binders non-require-varrefs non-require-referenced-macros tops)))

      ;; annotate-require-vars :    (listof (cons boolean syntax[identifier]))
      ;;                            (listof syntax)
      ;;                            (listof syntax)
      ;;                            (listof (cons boolean syntax[original]))
      ;;                         -> (listof syntax) (listof syntax)
      ;; returns the sublist of `varrefs' that did not come from module imports and
      ;;     and the sublist of `referenced-macros' that didn't come from module imports
      ;; effect: colors all require-bound ids from `varrefs' and draws arrow for them. 
      (define (annotate-require-vars varrefs/levels requires require-for-syntaxes referenced-macros)
        (let* ([maker (lambda (x) (make-req/tag x (syntax-object->datum x) #f))]
               [req/tags (map maker requires)]
               [req-syn/tags (map maker require-for-syntaxes)]
               [reduced-varrefs
                (let loop ([varrefs/levels varrefs/levels])
                  (cond
                    [(null? varrefs/levels) null]
                    [else (let* ([varref/level (car varrefs/levels)]
                                 [high-level? (car varref/level)]
                                 [varref (cdr varref/level)]
                                 [include?
                                  (if high-level?
                                      (annotate-require-var req-syn/tags varref #t)
                                      (annotate-require-var req/tags varref #f))])
                            (if include?
                                (loop (cdr varrefs/levels))
                                (cons varref (loop (cdr varrefs/levels)))))]))]

               [reduced-referenced-macros
                (filter (annotate-macro req/tags #f)
                        (map cdr (filter (lambda (x) (not (car x))) referenced-macros)))]

               ;; hopefully, this is the empty list (no idea if it ever can be non-empty or what it would mean...)
               [reduced-hl-referenced-macros
                (filter (annotate-macro req-syn/tags #t)
                        (map cdr (filter car referenced-macros)))])

          (for-each annotate-unused-require req/tags)
          (for-each annotate-unused-require req-syn/tags)
          (values reduced-varrefs
                  reduced-referenced-macros)))
      
      ;; annotate-macro : (listof req/tag) boolean -> syntax[original] -> boolean
      ;; result indicates if this macro definition came from a require
      ;; #f means from require, #t means not from require
      (define (annotate-macro req/tags high-level?)
        (lambda (stx)
          (let ([mod-req-path (get-module-req-path ((if high-level?
                                                        identifier-transformer-binding
                                                        identifier-binding)
                                                    stx))]
                [unused? #t])
            (for-each (lambda (req/tag)
                        (when (equal? (req/tag-req-sexp req/tag) mod-req-path)
                          (set! unused? #t)
                          (connect-syntaxes (req/tag-req-stx req/tag) stx)
                          (set-req/tag-used?! req/tag #t)))
                      req/tags)
            unused?)))

      ;; annotate-unused-require : syntax -> void
      (define (annotate-unused-require req/tag)
        (unless (req/tag-used? req/tag)
          (color (req/tag-req-stx req/tag) unbound-variable-style-str)))
      
      ;; annotate-require-var : (listof req/tags) syntax boolean -> boolean
      ;; returns #t if `varref' comes from a module import
      ;; effect: colors `varref' and adds binding structure arrows,
      ;;         if it is a require-bound ids,
      (define (annotate-require-var req/tags varref high-level?)
        (let ([id-mod-path (get-module-req-path ((if high-level? 
                                                     identifier-transformer-binding
                                                     identifier-binding)
                                                 varref))])
          (and id-mod-path
               (let ([req/tag/f (memf (lambda (x) (equal? (req/tag-req-sexp x) id-mod-path))
                                      req/tags)])
                 (and req/tag/f
                      (let ([req/tag (car req/tag/f)])
                        (set-req/tag-used?! req/tag #t)
                        (when (syntax-original? varref)
                          (color varref bound-variable-style-str)
                          (when (syntax-original? (req/tag-req-stx req/tag))
                            (connect-syntaxes (req/tag-req-stx req/tag) varref)))
                        #t))))))
      
      ;; get-module-req-path : binding -> (union #f require-sexp)
      ;; argument is the result of identifier-binding or identifier-transformer-binding
      (define (get-module-req-path binding)
        (and (pair? binding)
             (let ([mod-path (caddr binding)])
               (if (module-path-index? mod-path)

                   (let-values ([(base offset) (module-path-index-split mod-path)])
                     base)
                   mod-path))))
      
      ;; annotate-variables : namespace (listof syntax) (listof syntax) (listof syntax) (listof syntax) -> void
      ;; colors the variables, free are turned unbound color, bound are turned
      ;; bound color and all binders are turned bound color.
      ;; vars-ht maps from the name of an identifier to all of the ids that
      ;;         have that name. Filter the result for
      ;;         access to variables that are all module-identifier=?
      ;; similarly for binders-ht, except it maps only binding location ids.
      (define (annotate-variables users-namespace binders varrefs referenced-macros tops)
        (let ([vars-ht (make-hash-table)]
              [binders-ht (make-hash-table)])
          (for-each (add-var vars-ht) varrefs)
          (for-each (add-var vars-ht) referenced-macros)
          (for-each (add-var vars-ht) tops)
          (for-each (add-var vars-ht) binders)
          (for-each (add-var binders-ht) binders)


          (for-each (annotate-binder vars-ht) binders)
          (for-each (annotate-varref (handle-no-binders/lexical #t) vars-ht binders-ht #f)
                    varrefs)
          (for-each (annotate-varref (handle-no-binders/lexical #t) vars-ht binders-ht #t)
                    referenced-macros)
          (for-each (annotate-varref (handle-no-binders/top users-namespace) vars-ht binders-ht #f)
                    tops)))
      
      ;; add-var : hash-table -> syntax -> void
      ;; adds the variable to the hash table.
      (define (add-var ht)
        (lambda (var)
          (let* ([key (syntax-e var)]
                 [prev (hash-table-get ht key (lambda () null))])
            (hash-table-put! ht key (cons var prev)))))
      
      ;; annotate-binder : vars-hash-table -> syntax -> void
      ;; annotates a variable in a binding position
      (define (annotate-binder vars-ht)
        (lambda (binder)
          (when (syntax-original? binder)
            (let ([same-as-binder? (lambda (x) (module-identifier=? x binder))])
              (make-rename-menu binder vars-ht)))))
      
      ;; annotate-varref : (syntax -> void) (listof syntax) (listof syntax) boolean -> syntax -> void
      ;; annotates a variable reference with green (if bound)
      ;; and adds the arrows from the varref to the 
      ;; (possibly multiple) binding locations.
      (define (annotate-varref handle-no-binders vars-ht binders-ht keyword?)
        (lambda (varref)
          (when (syntax-original? varref)
            (let* ([same-as-varref? (lambda (x) (module-identifier=? x varref))]
                   [binders (filter same-as-varref? 
                                    (hash-table-get 
                                     binders-ht
                                     (syntax-e varref)
                                     (lambda () null)))])
              (make-rename-menu varref vars-ht)
              (cond
                [(null? binders) (handle-no-binders varref)]
                [else
                 (for-each 
                  (lambda (binder) 
                    (when (syntax-original? binder)
                      (connect-syntaxes binder varref)))
                  binders)
                 (color varref
                        (if keyword?
                            keyword-style-str
                            bound-variable-style-str))])))))
      
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
      
      ;; handle-no-binders/lexical : boolean -> syntax[original] -> void
      (define (handle-no-binders/lexical keyword?)
        (lambda (varref)
          (let ([binding (identifier-binding varref)])
            (cond
              [(not binding)
               (color varref unbound-variable-style-str)]
              [(pair? binding)
               (color varref (if keyword? keyword-style-str bound-variable-style-str))]
              [else (void)]))))
      
      ;; connect-syntaxes : syntax[original] syntax[original] -> void
      ;; adds an arrow from `from' to `to', unless they have the same source loc. 
      (define (connect-syntaxes from to)
        (let* ([from-source (syntax-source from)]
	       [to-source (syntax-source to)])
	  (when (and (eq? from-source to-source)
		     (is-a? from-source syncheck-text<%>)
		     (syntax-position from)
		     (syntax-span from)
		     (syntax-position to)
		     (syntax-span to))
	    (let* ([from-pos-left (- (syntax-position from) 1)]
		   [from-pos-right (+ from-pos-left (syntax-span from))]
		   [to-pos-left (- (syntax-position to) 1)]
		   [to-pos-right (+ to-pos-left (syntax-span to))])
	      (unless (= from-pos-left to-pos-left)
		(send from-source syncheck:add-arrow
		      from-pos-left from-pos-right
		      to-pos-left to-pos-right))))))
      
      ;; annotate-basic : syntax -> (values (listof syntax)
      ;;                                    (listof (cons boolean syntax))
      ;;                                    (listof syntax)
      ;;                                    (listof syntax)
      ;;                                    (listof (cons boolean syntax[original]))
      ;;                                    boolean)
      ;; annotates the lexical structure of the program `sexp', except
      ;; for the variables in the program. returns the variables in several
      ;; lists -- the first is the ones that occur in binding positions
      ;; and the second is those that occur in bound positions. The third
      ;; is those that occur in #%top's. 
      ;; The next value is all of the require expressions and then all of the
      ;; require-for-syntax expressions.
      ;; the next to last is the list of all original macro references.
      ;; the last is a boolean indicating if there was a `module' in the expanded expression.

      ;; the booleans in the lists indicate if the variables or macro references
      ;; were on the rhs of a define-syntax (boolean is #t) or not (boolean is #f)
      (define (annotate-basic sexp run-in-expansion-thread)
        (let ([binders null]
              [varrefs null]
              [tops null]
              [has-module? #f]
              [requires null]
              [require-for-syntaxes null]
              [referenced-macros null])
          (let level-loop ([sexp sexp]
                           [high-level? #f])
            (annotate-original-keywords sexp)
            (annotate-bound-in-source sexp)
            (set! referenced-macros (get-referenced-macros high-level? sexp referenced-macros))
            (set! varrefs (flatten-cons-tree #t (syntax-property sexp 'bound-in-source) varrefs))
            (set! binders (flatten-cons-tree 'no-cons (syntax-property sexp 'binding-in-source) binders))
            (let ([loop (lambda (sexp) (level-loop sexp high-level?))])
              (syntax-case* sexp (lambda case-lambda if begin begin0 let-value letrec-values set!
                                   quote quote-syntax with-continuation-mark 
                                   #%app #%datum #%top #%plain-module-begin
                                   define-values define-syntaxes module
                                   require require-for-syntax provide)
                (if high-level? module-transformer-identifier=? module-identifier=?)
                [(lambda args bodies ...)
                 (begin
                   (annotate-raw-keyword sexp)
                   (set! binders (combine/color-binders (syntax args) binders bound-variable-style-str))
                   (for-each loop (syntax->list (syntax (bodies ...)))))]
                [(case-lambda [argss bodiess ...]...)
                 (begin
                   (annotate-raw-keyword sexp)
                   (for-each
                    (lambda (args bodies)
                      (set! binders (combine/color-binders args binders bound-variable-style-str))
                      (for-each loop (syntax->list bodies)))
                    (syntax->list (syntax (argss ...)))
                    (syntax->list (syntax ((bodiess ...) ...)))))]
                [(if test then else)
                 (begin
                   (annotate-raw-keyword sexp)
                   (loop (syntax test))
                   (loop (syntax else))
                   (loop (syntax then)))]
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
                   (for-each (lambda (x) (set! binders (combine/color-binders x binders bound-variable-style-str)))
                             (syntax->list (syntax ((xss ...) ...))))
                   (for-each loop (syntax->list (syntax (es ...))))
                   (for-each loop (syntax->list (syntax (bs ...)))))]
                [(letrec-values (((xss ...) es) ...) bs ...)
                 (begin
                   (annotate-raw-keyword sexp)
                   (for-each (lambda (x) (set! binders (combine/color-binders x binders bound-variable-style-str)))
                             (syntax->list (syntax ((xss ...) ...))))
                   (for-each loop (syntax->list (syntax (es ...))))
                   (for-each loop (syntax->list (syntax (bs ...)))))]
                [(set! var E)
                 (begin
                   (annotate-raw-keyword sexp)
                   (set! varrefs (cons (cons high-level? (syntax var)) varrefs))
                   (loop (syntax E)))]
                [(quote datum)
                 (begin 
                   (annotate-raw-keyword sexp)
                   (when (syntax-original? (syntax datum))
                     (color (syntax datum) constant-style-str)))]
                [(quote-syntax datum)
                 (begin 
                   (annotate-raw-keyword sexp)
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
                   (set! binders (combine/color-binders (syntax vars) binders bound-variable-style-str))
                   (loop (syntax b)))]
                [(define-syntaxes names exp)
                 (begin
                   (annotate-raw-keyword sexp)
                   (set! binders (combine/color-binders (syntax names) binders keyword-style-str))
                   (level-loop (syntax exp) #t))]
                [(module m-name lang (#%plain-module-begin bodies ...))
                 (begin
                   (set! has-module? #t)
                   ((annotate-require-open run-in-expansion-thread) (syntax lang))
                   (set! requires (cons (syntax lang) requires))
                   (annotate-raw-keyword sexp)
                   (for-each loop (syntax->list (syntax (bodies ...)))))]
                
                ; top level or module top level only:
                [(require require-specs ...)
                 (let ([new-specs (map trim-require-prefix
                                       (syntax->list (syntax (require-specs ...))))])
                   (for-each (annotate-require-open run-in-expansion-thread) new-specs)
                   (set! requires (append new-specs requires))
                   (annotate-raw-keyword sexp))]
                [(require-for-syntax require-specs ...)
                 (let ([new-specs (map trim-require-prefix (syntax->list (syntax (require-specs ...))))])
                   (for-each (annotate-require-open run-in-expansion-thread) new-specs)
                   (set! require-for-syntaxes (append new-specs require-for-syntaxes))
                   (annotate-raw-keyword sexp))]
                
                ; module top level only:
                [(provide provide-specs ...)
                 (begin
                   (set! varrefs
                         (append
                          (map
                           (lambda (x) (cons #f x))
                           (apply append (map extract-provided-vars 
                                              (syntax->list (syntax (provide-specs ...))))))
                          varrefs))
                   (annotate-raw-keyword sexp))]
                
                [id
                 (identifier? (syntax id))
                 (set! varrefs (cons (cons high-level? sexp) varrefs))]
                [_
                 (begin
                   '(printf "unknown stx: ~e (datum: ~e) (source: ~e)~n"
                            sexp
                            (and (syntax? sexp)
                                 (syntax-object->datum sexp))
                            (and (syntax? sexp)
                                 (syntax-source sexp)))
                   (void))])))
          (values binders 
                  varrefs
                  tops
                  requires
                  require-for-syntaxes
                  referenced-macros
                  has-module?)))

      ;; annotate-bound-in-source : syntax -> void
      ;; adds arrows between pairs found in the 'bound-in-source syntax property.
      (define (annotate-bound-in-source stx)
        (let loop ([bis (syntax-property stx 'bound-in-source)])
          (cond
            [(and (cons? bis)
                  (syntax? (car bis))
                  (syntax? (cdr bis)))
             (when (and (syntax-original? (car bis))
                        (syntax-original? (cdr bis)))
               (connect-syntaxes (car bis) (cdr bis)))]
            [(cons? bis)
             (loop (car bis))
             (loop (cdr bis))]
            [else (void)])))

      ;; annotate-require-open : ((-> void) -> stx -> void)
      (define (annotate-require-open run-in-expansion-thread)
        (lambda (require-spec)
          (when (syntax-original? require-spec)
            (let ([source (syntax-source require-spec)])
              (when (and (is-a? source syncheck-text<%>)
			 (syntax-position require-spec)
			 (syntax-span require-spec))
                (let* ([start (- (syntax-position require-spec) 1)]
                       [end (+ start (syntax-span require-spec))]
                       [datum (syntax-object->datum require-spec)]
                       [sym 
                        (and (not (symbol? datum))
                             (run-in-expansion-thread
                              (lambda ()
                                ((current-module-name-resolver)
                                 (syntax-object->datum require-spec)
                                 #f 
                                 #f))))]
                       [file (and (symbol? sym)
                                  (module-name-sym->filename sym))])
                  (when file
                    (send source syncheck:add-menu start end 
                          #f
                          (make-require-open-menu file)))))))))
      
      ;; make-require-open-menu : string[filename] -> menu -> void
      (define (make-require-open-menu file)
        (lambda (menu)
          (let-values ([(base name dir?) (split-path file)])
            (instantiate menu-item% ()
              (label (format (string-constant cs-open-file) name))
              (parent menu)
              (callback (lambda (x y) (fw:handler:edit-file file))))
            (void))))
      
      ;; possible-suffixes : (listof string)
      ;; these are the suffixes that are checked for the reverse 
      ;; module-path mapping.
      (define possible-suffixes '(".ss" ".scm" ""))
      
      ;; module-name-sym->filename : symbol -> (union #f string)
      (define (module-name-sym->filename sym)
        (let ([str (symbol->string sym)])
          (and ((string-length str) . > . 1)
               (char=? (string-ref str 0) #\,)
               (let ([fn (substring str 1 (string-length str))])
                 (ormap (lambda (x)
                          (let ([test (string-append fn x)])
                            (and (file-exists? test)
                                 test)))
                        possible-suffixes)))))

      ;; get-referenced-macros : boolean sexp -> (listof (cons boolean syntax[original]))
      (define (get-referenced-macros high-level? sexp acc)
        (let ([origin (syntax-property sexp 'origin)])
          (if origin
              (flatten-cons-tree high-level? origin acc)
              acc)))
      
      ;; type cons-tree : (union (cons cons-tree cons-tree) syntax)
      ;; flatten-cons-tree : (union 'no-cons boolean)
      ;;                     cons-tree
      ;;                     (union (listof syntax[original]) (listof (cons boolean syntax[original])))
      ;;                     -> (listof syntax[original])
      ;; flattens the cons tree in one of two ways. If the first
      ;; argument is 'no-cons, `acc' must be a list of syntax; if
      ;; the first argument is a boolean, `acc' must be a list of pairs.
      ;; Uses the first argument's value to match the acc, either
      ;; constructing pairs or not as elements of the list.
      (define (flatten-cons-tree level ct acc)
        (let loop ([ct ct]
                   [acc acc])
          (cond
            [(pair? ct) (loop (car ct) (loop (cdr ct) acc))]
            [(syntax? ct) (if (syntax-original? ct)
                              (if (boolean? level)
                                  (cons (cons level ct) acc)
                                  (cons ct acc))
                              acc)]
            [else acc])))

      ;; extract-provided-vars : syntax -> (listof syntax[identifier])
      (define (extract-provided-vars stx)
        (syntax-case stx (rename struct all-from all-from-except)
          [identifier
           (identifier? (syntax identifier))
           (list (syntax identifier))]
          
          [(rename local-identifier export-identifier) 
           (list (syntax local-identifier))]
          
          ;; why do I even see this?!?
          [(struct struct-identifier (field-identifier ...))
           null]
          
          [(all-from module-name) null] 
          [(all-from-except module-name identifer ...)
           null]
          [_ 
           null]))
          
      
       ;; trim-require-prefix : syntax -> syntax
      (define (trim-require-prefix require-spec)
        (let loop ([stx require-spec])
          (syntax-case stx (prefix all-except rename)
            [(prefix identifier module-name) (loop (syntax module-name))]
            [(all-except module-name identifer ...)
             (loop (syntax module-name))]
            [(rename module-name local-identifer exported-identifer)
             (loop (syntax module-name))]
            [_ stx])))
      
      
      ;; combine/color-binders : boolean syntax (listof syntax) str -> (listof syntax)
      ;; transforms an argument list into a bunch of symbols/symbols and puts 
      ;; them on `incoming'
      (define (combine/color-binders stx incoming style-str)
        (let loop ([stx stx]
                   [sofar incoming])
          (let ([e (if (syntax? stx) (syntax-e stx) stx)])
            (cond
              [(cons? e)
               (let ([fst (car e)]
                     [rst (cdr e)])
                 (if (syntax? fst)
                     (begin
                       (when (syntax-original? fst)
                         (color fst style-str))
                       (loop rst (cons fst sofar)))
                     (loop rst sofar)))]
              [(null? e) sofar]
              [else 
               (when (syntax-original? stx)
                 (color stx style-str))
               (cons stx sofar)]))))
      
      
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
      
      ;; color : syntax[original] str -> void
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
      
      ;; make-rename-menu : stx[original] (hash-table symbol (listof syntax)) -> void
      (define (make-rename-menu stx vars-ht)
        (let ([source (syntax-source stx)])
          (when (is-a? source syncheck-text<%>)
            (let* ([name-to-offer (format "~a" (syntax-object->datum stx))]
                   [start (- (syntax-position stx) 1)]
                   [fin (+ start (syntax-span stx))])
              (send source syncheck:add-menu
                    start fin (syntax-e stx)
                    (lambda (menu)
                      (instantiate menu-item% ()
                        (parent menu)
                        (label (format (string-constant cs-rename-var) name-to-offer))
                        (callback
                         (lambda (x y)
                           (rename-callback name-to-offer stx vars-ht))))))))))

      ;; rename-callback : string syntax[original] (listof syntax) -> void
      ;; callback for the rename popup menu item
      (define (rename-callback name-to-offer stx vars-ht)
        (let ([new-sym 
               (fw:keymap:call/text-keymap-initializer
                (lambda ()
                  (get-text-from-user
                   (string-constant cs-rename-id)
                   (format (string-constant cs-rename-var-to) name-to-offer)
                   #f
                   name-to-offer)))])
          (when new-sym
            (let* ([same-names
                    (filter (lambda (x) (module-identifier=? x stx))
                            (hash-table-get vars-ht (syntax-e stx)))]
                   [to-be-renamed 
                    (remove-duplicates
                     (quicksort 
                      (filter syntax-original? same-names)
                      (lambda (x y) 
                        ((syntax-position x) . >= . (syntax-position y)))))])
              (cond
                [(name-duplication? to-be-renamed vars-ht new-sym)
                 (message-box (string-constant check-syntax)
                              (format (string-constant cs-name-duplication-error) 
                                      new-sym))]
                [else
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
                                         (send source insert new-sym start start #f)))))
                                 to-be-renamed)
                       (send first-one-source invalidate-bitmap-cache)
                       (send first-one-source end-edit-sequence))))])))))
      
      ;; name-duplication? : (listof syntax) hash-table symbol -> boolean
      ;; returns #t if the name chosen would be the same as another name in this scope.
      (define (name-duplication? to-be-renamed vars-ht new-str)
        (let* ([new-sym (string->symbol new-str)]
               [possible-conflicts/with-tbr (hash-table-get vars-ht new-sym (lambda () null))]
               [possible-conflicts
                (filter (lambda (x) (not (memf (lambda (y) (module-identifier=? x y)) to-be-renamed)))
                        possible-conflicts/with-tbr)])
          (ormap (lambda (to-be-renamed-var)
                   (let ([new-identifier (datum->syntax-object to-be-renamed-var new-sym)])
                     (ormap (lambda (possible-conflict) (module-identifier=? possible-conflict new-identifier))
                            possible-conflicts)))
                 to-be-renamed)))
      
      ;; remove-duplicates : (listof syntax[original]) -> (listof syntax[original])
      ;; removes duplicates, based on the source locations of the identifiers
      (define (remove-duplicates ids)
        (cond
          [(null? ids) null]
          [else (let loop ([fst (car ids)]
                           [rst (cdr ids)])
                  (cond
                    [(null? rst) (list fst)]
                    [else (if (= (syntax-position fst)
                                 (syntax-position (car rst)))
                              (loop fst (cdr rst))
                              (cons fst (loop (car rst) (cdr rst))))]))]))
      
      (drscheme:get/extend:extend-definitions-text make-graphics-text%)
      (drscheme:get/extend:extend-unit-frame make-new-unit-frame% #f))))
