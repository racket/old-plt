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
           (lib "toplevel.ss" "syntax")
           (lib "kerncase.ss" "syntax")
           (prefix drscheme:arrow: "arrow.ss")
           (prefix fw: (lib "framework.ss" "framework"))
           (lib "mred.ss" "mred"))
  (provide tool@)

  (define o (current-output-port))
  
  (define status-init (string-constant cs-status-init))
  (define status-coloring-program (string-constant cs-status-coloring-program))
  (define status-eval-compile-time (string-constant cs-status-eval-compile-time))
  (define status-expanding-expression (string-constant cs-status-expanding-expression))
  
  (define mouse-over-variable-import (string-constant cs-mouse-over-variable-import))
  (define mouse-over-syntax-import (string-constant cs-mouse-over-syntax-import))
  
  (define jump-to-next-bound-occurrence (string-constant cs-jump-to-next-bound-occurrence))
  (define jump-to-binding (string-constant cs-jump-to-binding))
  (define jump-to-definition (string-constant cs-jump-to-definition))
  
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)

      (define (phase1) 
        (drscheme:unit:add-to-program-editor-mixin clearing-text-mixin))
      (define (phase2) (void))

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
      (define tail-call-style-str (prefix-style/check 'tail-call))
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
          (let* ([style-list (fw:editor:get-standard-style-list)]
                 [name (symbol->string sym)]
                 [style (send style-list find-named-style name)])
            (if style
                (send style set-delta delta)
                (send style-list new-named-style name
                      (send style-list find-or-create-style
                            (send style-list find-named-style "Standard")
                            delta))))))
      
      (for-each set-slatex-style delta-symbols (map fw:preferences:get delta-symbols))
      
      ;; used for quicker debugging of the preference panel
      '(define test-preference-panel
         (lambda (name f)
           (let ([frame (make-object frame% name)])
             (f frame)
             (send frame show #t))))
      
      (define standard-style-list-text% (fw:editor:standard-style-list-mixin text%))
      
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
                       [e (new (class standard-style-list-text%
                                 (inherit change-style get-style-list)
                                 (rename [super-after-insert after-insert])
                                 (override after-insert)
                                 (define (after-insert pos offset)
                                   (super-after-insert pos offset)
                                   (let ([style (send (get-style-list)
                                                      find-named-style
                                                      style-name)])
                                     (change-style style pos (+ pos offset) #f)))
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
      
      (define-struct arrow (start-x start-y end-x end-y))
      (define-struct (var-arrow arrow)
                     (start-text start-pos-left start-pos-right
                      end-text end-pos-left end-pos-right))
      (define-struct (tail-arrow arrow) (from-text from-pos to-text to-pos))
      
      (define-struct def-link (syntax filename) (make-inspector))
      
      (define tacked-var-brush (send the-brush-list find-or-create-brush "BLUE" 'solid))
      (define var-pen (send the-pen-list find-or-create-pen "BLUE" 1 'solid))
      (define tail-pen (send the-pen-list find-or-create-pen "orchid" 1 'solid))
      (define tacked-tail-brush (send the-brush-list find-or-create-brush "orchid" 'solid))
      (define untacked-brush (send the-brush-list find-or-create-brush "WHITE" 'solid))
      
      (define syncheck-text<%>
        (interface ()
          syncheck:init-arrows
          syncheck:clear-arrows
          syncheck:add-menu
          syncheck:add-arrow
          syncheck:add-tail-arrow
          syncheck:add-mouse-over-status
          syncheck:add-jump-to-definition
          syncheck:sort-bindings-table
          syncheck:jump-to-next-bound-occurrence
          syncheck:jump-to-binding-occurrence
          syncheck:jump-to-definition))

      ;; clearing-text-mixin : (mixin text%)
      ;; overrides methods that make sure the arrows go away appropriately.
      ;; adds a begin/end-edit-sequence to the insertion and deletion
      ;;  to ensure that the on-change method isn't called until after
      ;;  the arrows are cleared.
      (define clearing-text-mixin
        (fw:mixin ((class->interface text%)) ()
          (rename [super-after-insert after-insert]
                  [super-on-insert on-insert]
                  [super-after-delete after-delete]
                  [super-on-delete on-delete])
          (inherit begin-edit-sequence end-edit-sequence)
          (define/override (on-delete start len)
            (begin-edit-sequence)
            (super-on-delete start len))
          (define/override (after-delete start len)
            (super-after-delete start len)
            (let ([st (find-syncheck-text this)])
              (when st
                (send st syncheck:clear-arrows)))
            (end-edit-sequence))
          
          (define/override (on-insert start len)
            (begin-edit-sequence)
            (super-on-insert start len))
          (define/override (after-insert start len)
            (super-after-insert start len)
            (let ([st (find-syncheck-text this)])
              (when st
                (send st syncheck:clear-arrows)))
            (end-edit-sequence))

	  (super-instantiate ())))
      
      
      (define make-graphics-text%
        (lambda (super%)
          (let* ([cursor-arrow (make-object cursor% 'arrow)])
            (class* super% (syncheck-text<%>)
              (inherit set-cursor get-admin invalidate-bitmap-cache set-position
                       position-location
                       get-canvas last-position dc-location-to-editor-location
                       find-position begin-edit-sequence end-edit-sequence)
              
              (rename [super-on-paint on-paint]
                      [super-on-event on-event])
              
              ;; arrow-vectors : 
              ;; (union 
              ;;  #f
              ;;  (hash-table
              ;;    (text%
              ;;     . -o> .
              ;;    (vector (listof (union (cons (union #f sym) (menu -> void))
              ;;                           def-link
              ;;                           tail-link
              ;;                           arrow
              ;;                           string))))))
              (define arrow-vectors #f)
              
              
              ;; bindings-table : hash-table[(list text number number) -o> (listof (list text number number))]
              (field [bindings-table (make-hash-table 'equal)])
              (define (add-to-bindings-table start-text start-left start-right
                                             end-text end-left end-right)
                (unless (and (object=? start-text end-text)
                             (= start-left end-left)
                             (= start-right end-right))
                  (let ([key (list start-text start-left start-right)])
                    (hash-table-put!
                     bindings-table
                     key
                     (cons
                      (list end-text end-left end-right)
                      (hash-table-get bindings-table key (lambda () '())))))))
              
              (define/public (syncheck:sort-bindings-table)
                
                ;; compare-bindings : (list text number number) (list text number number) -> boolean
                (define (compare-bindings l1 l2)
                  (let ([start-text (first l1)]
                        [start-left (second l1)]
                        [end-text (first l2)]
                        [end-left (second l2)])
                    (let-values ([(sx sy) (find-dc-location start-text start-left)]
                                 [(ex ey) (find-dc-location end-text end-left)])
                      (cond
                        [(= sy ey) (< sx ex)]
                        [else (< sy ey)]))))
                
                ;; find-dc-location : text number -> (values number number)
                (define (find-dc-location text pos)
                  (let ([bx (box 0)]
                        [by (box 0)])
                    (send text position-location pos bx by)
                    (send text editor-location-to-dc-location (unbox bx) (unbox by))))
                
                (hash-table-for-each
                 bindings-table
                 (lambda (k v)
                   (hash-table-put! bindings-table k (quicksort v compare-bindings)))))
                    
              (field (tacked-hash-table (make-hash-table)))
              (field [cursor-location #f]
                     [cursor-text #f])
              (define (find-poss text left-pos right-pos)
                (let ([xlb (box 0)]
                      [ylb (box 0)]
                      [xrb (box 0)]
                      [yrb (box 0)])
                  (send text position-location left-pos xlb ylb #t)
                  (send text position-location right-pos xrb yrb #f)
                  (let*-values ([(xl-off yl-off) (send text editor-location-to-dc-location (unbox xlb) (unbox ylb))]
                                [(xl yl) (dc-location-to-editor-location xl-off yl-off)]
                                [(xr-off yr-off) (send text editor-location-to-dc-location (unbox xrb) (unbox yrb))]
                                [(xr yr) (dc-location-to-editor-location xr-off yr-off)])
                    (values (/ (+ xl xr) 2)
                            (/ (+ yl yr) 2)))))
              
              ;; find-char-box : text number number -> (values number number number number)
              ;; returns the bounding box (left, top, right, bottom) for the text range.
              ;; only works right if the text is on a single line.
              (define (find-char-box text left-pos right-pos)
                (let ([xlb (box 0)]
                      [ylb (box 0)]
                      [xrb (box 0)]
                      [yrb (box 0)])
                  (send text position-location left-pos xlb ylb #t)
                  (send text position-location right-pos xrb yrb #f)
                  (let*-values ([(xl-off yl-off) (send text editor-location-to-dc-location (unbox xlb) (unbox ylb))]
                                [(xl yl) (dc-location-to-editor-location xl-off yl-off)]
                                [(xr-off yr-off) (send text editor-location-to-dc-location (unbox xrb) (unbox yrb))]
                                [(xr yr) (dc-location-to-editor-location xr-off yr-off)])
                    (values 
                     xl
                     yl
                     xr 
                     yr))))
              
              (define (update-arrow-poss arrow)
                (cond
                  [(var-arrow? arrow) (update-var-arrow-poss arrow)]
                  [(tail-arrow? arrow) (update-tail-arrow-poss arrow)]))
              
              (define (update-var-arrow-poss arrow)
                (let-values ([(start-x start-y) (find-poss 
                                                 (var-arrow-start-text arrow)
                                                 (var-arrow-start-pos-left arrow)
                                                 (var-arrow-start-pos-right arrow))]
                             [(end-x end-y) (find-poss 
                                             (var-arrow-end-text arrow)
                                             (var-arrow-end-pos-left arrow)
                                             (var-arrow-end-pos-right arrow))])
                  (set-arrow-start-x! arrow start-x)
                  (set-arrow-start-y! arrow start-y)
                  (set-arrow-end-x! arrow end-x)
                  (set-arrow-end-y! arrow end-y)))
              
              (define (update-tail-arrow-poss arrow)
                (let-values ([(start-x start-y) (find-poss 
                                                 (tail-arrow-from-text arrow)
                                                 (tail-arrow-from-pos arrow)
                                                 (+ (tail-arrow-from-pos arrow) 1))]
                             [(end-x end-y) (find-poss 
                                             (tail-arrow-to-text arrow)
                                             (tail-arrow-to-pos arrow)
                                             (+ (tail-arrow-to-pos arrow) 1))])
                  (set-arrow-start-x! arrow start-x)
                  (set-arrow-start-y! arrow start-y)
                  (set-arrow-end-x! arrow end-x)
                  (set-arrow-end-y! arrow end-y)))
              
              (define/public (syncheck:init-arrows)
                (set! tacked-hash-table (make-hash-table))
                (set! arrow-vectors (make-hash-table))
                (set! bindings-table (make-hash-table 'equal))
                (let ([f (get-top-level-window)])
                  (when f
                    (send f open-status-line 'drscheme:check-syntax:mouse-over))))
              (define/public (syncheck:clear-arrows)
                (when (or arrow-vectors cursor-location cursor-text)
                  (let ([any-tacked? #f])
                    (when tacked-hash-table
                      (let/ec k
                        (hash-table-for-each
                         tacked-hash-table
                         (lambda (key val)
                           (set! any-tacked? #t)
                           (k (void))))))
                    (set! tacked-hash-table #f)
                    (set! arrow-vectors #f)
                    (set! cursor-location #f)
                    (set! cursor-text #f)
                    (when any-tacked?
                      (invalidate-bitmap-cache))
                    (let ([f (get-top-level-window)])
                      (when f
                        (send f close-status-line 'drscheme:check-syntax:mouse-over))))))
              (define/public (syncheck:add-menu text start-pos end-pos key make-menu)
                (when (and (<= 0 start-pos end-pos (last-position)))
                  (add-to-range/key text start-pos end-pos make-menu key #t)))
              
              ;; syncheck:add-arrow : symbol text number number text number number -> void
              ;; pre: start-editor, end-editor are embedded in `this' (or are `this')
              (define/public (syncheck:add-arrow start-text start-pos-left start-pos-right
                                                 end-text end-pos-left end-pos-right)
                (let* ([arrow (make-var-arrow #f #f #f #f
                                              start-text start-pos-left start-pos-right
                                              end-text end-pos-left end-pos-right)])
                  (add-to-bindings-table
                   start-text start-pos-left start-pos-right
                   end-text end-pos-left end-pos-right)
                  (add-to-range/key start-text start-pos-left start-pos-right arrow #f #f)
                  (add-to-range/key end-text end-pos-left end-pos-right arrow #f #f)))
              
              ;; syncheck:add-tail-arrow : text number text number -> void
              (define/public (syncheck:add-tail-arrow from-text from-pos to-text to-pos)
                (let ([tail-arrow (make-tail-arrow #f #f #f #f to-text to-pos from-text from-pos)])
                  (add-to-range/key from-text from-pos (+ from-pos 1) tail-arrow #f #f)
                  (add-to-range/key from-text to-pos (+ to-pos 1) tail-arrow #f #f)))
              
              ;; syncheck:add-jump-to-definition : text start end syntax filename -> void
              (define/public (syncheck:add-jump-to-definition text start end stx filename)
                (add-to-range/key text start end (make-def-link stx filename) #f #f))
              
              ;; syncheck:add-mouse-over-status : text pos-left pos-right string -> void
              (define/public (syncheck:add-mouse-over-status text pos-left pos-right str)
                (add-to-range/key text pos-left pos-right str #f #f))

              ;; add-to-range/key : text number number any any boolean -> void
              ;; adds `key' to the range `start' - `end' in the editor
              ;; If use-key? is #t, it adds `to-add' with the key, and does not
              ;; replace a value with that key already there.
              ;; If use-key? is #f, it adds `to-add' without a key.
              ;; pre: arrow-vectors is not #f
              (define (add-to-range/key text start end to-add key use-key?)
                (let ([arrow-vector (hash-table-get 
                                     arrow-vectors
                                     text 
                                     (lambda ()
                                       (let ([new-vec 
                                              (make-vector
                                               (add1 (send text last-position))
                                               null)])
                                         (hash-table-put! 
                                          arrow-vectors 
                                          text
                                          new-vec)
                                         new-vec)))])
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
                      (loop (add1 p))))))

              (inherit get-top-level-window)

              (rename [super-on-change on-change])
              (define/override (on-change)
                (super-on-change)
                (when arrow-vectors
                  (flush-arrow-coordinates-cache)
                  (let ([any-tacked? #f])
                    (when tacked-hash-table
                      (let/ec k
                        (hash-table-for-each
                         tacked-hash-table
                         (lambda (key val)
                           (set! any-tacked? #t)
                           (k (void))))))
                    (when any-tacked?
                      (invalidate-bitmap-cache)))))
              
              ;; flush-arrow-coordinates-cache : -> void
              ;; pre-condition: arrow-vector is not #f.
              (define/private (flush-arrow-coordinates-cache)
                (hash-table-for-each
                 arrow-vectors
                 (lambda (text arrow-vector)
                   (let loop ([n (vector-length arrow-vector)])
                     (unless (zero? n)
                       (let ([eles (vector-ref arrow-vector (- n 1))])
                         (for-each (lambda (ele)
                                     (cond
                                       [(arrow? ele)
                                        (set-arrow-start-x! ele #f)
                                        (set-arrow-start-y! ele #f)
                                        (set-arrow-end-x! ele #f)
                                        (set-arrow-end-y! ele #f)]))
                                   eles))
                       (loop (- n 1)))))))
              
              (define/override (on-paint before dc left top right bottom dx dy draw-caret)
                (super-on-paint before dc left top right bottom dx dy draw-caret)
                (when (and arrow-vectors (not before))
                  (let ([draw-arrow2
                         (lambda (arrow)
                           (unless (arrow-start-x arrow)
                             (update-arrow-poss arrow))
                           (let ([start-x (arrow-start-x arrow)]
                                 [start-y (arrow-start-y arrow)]
                                 [end-x   (arrow-end-x arrow)]
                                 [end-y   (arrow-end-y arrow)])
                             (unless (and (= start-x end-x)
                                          (= start-y end-y))
                               (drscheme:arrow:draw-arrow dc start-x start-y end-x end-y dx dy))))]
                        [old-brush (send dc get-brush)]
                        [old-pen   (send dc get-pen)])
                    (hash-table-for-each tacked-hash-table
                                         (lambda (arrow v) 
                                           (when v 
                                             (cond
                                               [(var-arrow? arrow)
                                                (send dc set-pen var-pen)
                                                (send dc set-brush tacked-var-brush)]
                                               [(tail-arrow? arrow)
                                                (send dc set-pen tail-pen)
                                                (send dc set-brush tacked-tail-brush)])
                                             (draw-arrow2 arrow))))
                    (when (and cursor-location
                               cursor-text)
                      (let* ([arrow-vector (hash-table-get arrow-vectors cursor-text (lambda () #f))])
                        (when arrow-vector
                          (let ([eles (vector-ref arrow-vector cursor-location)])
                            (for-each (lambda (ele) 
                                        (cond
                                          [(var-arrow? ele)
                                           (send dc set-pen var-pen)
                                           (send dc set-brush untacked-brush)
                                           (draw-arrow2 ele)]
                                          [(tail-arrow? ele)
                                           (send dc set-pen tail-pen)
                                           (send dc set-brush untacked-brush)
                                           (for-each-tail-arrows draw-arrow2 ele)]))
                                      eles)))))
                    (send dc set-brush old-brush)
                    (send dc set-pen old-pen))))
              
              ;; for-each-tail-arrows : (tail-arrow -> void) tail-arrow -> void
              (define (for-each-tail-arrows f tail-arrow)
                ;; call-f-ht ensures that `f' is only called once per arrow
                (define call-f-ht (make-hash-table))

                (define (for-each-tail-arrows/to/from tail-arrow-pos tail-arrow-text
                                                      tail-arrow-other-pos tail-arrow-other-text)

                  ;; traversal-ht ensures that we don't loop in the arrow traversal.
                  (let ([traversal-ht (make-hash-table)])
                    (let loop ([tail-arrow tail-arrow])
                      (unless (hash-table-get traversal-ht tail-arrow (lambda () #f))
                        (hash-table-put! traversal-ht tail-arrow #t)
                        (unless (hash-table-get call-f-ht tail-arrow (lambda () #f))
                          (hash-table-put! call-f-ht tail-arrow #t)
                          (f tail-arrow))
                        (let* ([next-pos (tail-arrow-pos tail-arrow)]
                               [next-text (tail-arrow-text tail-arrow)]
                               [arrow-vector (hash-table-get arrow-vectors next-text (lambda () #f))])
                          (when arrow-vector
                            (let ([eles (vector-ref arrow-vector next-pos)])
                              (for-each (lambda (ele) 
                                          (cond
                                            [(tail-arrow? ele)
                                             (let ([other-pos (tail-arrow-other-pos ele)]
                                                   [other-text (tail-arrow-other-text ele)])
                                               (when (and (= other-pos next-pos)
                                                          (eq? other-text next-text))
                                                 (loop ele)))]))
                                        eles))))))))
                
                (for-each-tail-arrows/to/from tail-arrow-to-pos tail-arrow-to-text
                                              tail-arrow-from-pos tail-arrow-from-text)
                (for-each-tail-arrows/to/from tail-arrow-from-pos tail-arrow-from-text
                                              tail-arrow-to-pos tail-arrow-to-text))
              
              
              
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
              
            (define/override (on-event event)
              (if arrow-vectors
                  (cond
                    [(send event leaving?)
                     (when (and cursor-location cursor-text)
                       (set! cursor-location #f)
                       (set! cursor-text #f)
                       (let ([f (get-top-level-window)])
                         (when f
                           (send f update-status-line 'drscheme:check-syntax:mouse-over #f)))
                       (invalidate-bitmap-cache))
                     (super-on-event event)]
                    [(or (send event moving?)
                         (send event entering?))
                     (let-values ([(pos text) (get-pos/text event)])
                       (cond
                         [(and pos text)
                          (unless (and (equal? pos cursor-location)
                                       (eq? cursor-text text))
                            (set! cursor-location pos)
                            (set! cursor-text text)
                            
                            (let* ([arrow-vector (hash-table-get arrow-vectors cursor-text (lambda () #f))]
                                   [eles (and arrow-vector (vector-ref arrow-vector cursor-location))])
                              (when eles
                                (let ([has-txt? #f])
                                  (for-each (lambda (ele)
                                              (cond
                                                [(string? ele)
                                                 (set! has-txt? #t)
                                                 (let ([f (get-top-level-window)])
                                                   (when f
                                                     (send f update-status-line 
                                                           'drscheme:check-syntax:mouse-over 
                                                           ele)))]))
                                            eles)
                                  (unless has-txt?
                                    (let ([f (get-top-level-window)])
                                      (when f
                                        (send f update-status-line 'drscheme:check-syntax:mouse-over #f))))))
                              
                              (when eles
                                (for-each (lambda (ele)
                                            (cond
                                              [(arrow? ele)
                                               (update-arrow-poss ele)]))
                                          eles)
                                (invalidate-bitmap-cache))))]
                         [else
                          (let ([f (get-top-level-window)])
                            (when f
                              (send f update-status-line 'drscheme:check-syntax:mouse-over #f)))
                          (when (or cursor-location cursor-text)
                            (set! cursor-location #f)
                            (set! cursor-text #f)
                            (invalidate-bitmap-cache))]))
                     (super-on-event event)]
                    [(send event button-down? 'right)
                     (let-values ([(pos text) (get-pos/text event)])
                       (if (and pos text)
                           (let ([arrow-vector (hash-table-get arrow-vectors text (lambda () #f))])
                             (when arrow-vector
                               (let ([vec-ents (vector-ref arrow-vector pos)])
                                 (cond
                                   [(null? vec-ents)
                                    (super-on-event event)]
                                   [else
                                    (let* ([menu (make-object popup-menu% #f)]
                                           [arrows (filter arrow? vec-ents)]
                                           [def-links (filter def-link? vec-ents)]
                                           [var-arrows (filter var-arrow? arrows)]
                                           [add-menus (map cdr (filter cons? vec-ents))])
                                      (unless (null? arrows)
                                        (make-object menu-item%
                                          (string-constant cs-tack/untack-arrow)
                                          menu
                                          (lambda (item evt) (tack/untack-callback arrows))))
                                      (unless (null? def-links)
                                        (let ([def-link (car def-links)])
                                          (make-object menu-item%
                                            jump-to-definition
                                            menu
                                            (lambda (item evt)
                                              (jump-to-definition-callback def-link)))))
                                      (unless (null? var-arrows)
                                        (make-object menu-item%
                                          jump-to-next-bound-occurrence
                                          menu
                                          (lambda (item evt) (jump-to-next-callback pos text arrows)))
                                        (make-object menu-item%
                                          jump-to-binding
                                          menu
                                          (lambda (item evt) (jump-to-binding-callback arrows))))
                                      (for-each (lambda (f) (f menu)) add-menus)
                                      (send (get-canvas) popup-menu menu
                                            (+ 1 (inexact->exact (floor (send event get-x))))
                                            (+ 1 (inexact->exact (floor (send event get-y))))))]))))
                           (super-on-event event)))]
                    [else (super-on-event event)])
                  (super-on-event event)))

              ;; tack/untack-callback : (listof arrow) -> void
              ;; callback for the tack/untack menu item
              (define (tack/untack-callback arrows)
                (let ([arrow-tacked?
                       (lambda (arrow)
                         (hash-table-get
                          tacked-hash-table
                          arrow
                          (lambda () #f)))]
                      [untack-arrows? #f])
                  (for-each 
                   (lambda (arrow)
                     (cond
                       [(var-arrow? arrow)
                        (set! untack-arrows? (or untack-arrows? (arrow-tacked? arrow)))]
                       [(tail-arrow? arrow)
                        (for-each-tail-arrows
                         (lambda (arrow) (set! untack-arrows? (or untack-arrows? (arrow-tacked? arrow))))
                         arrow)]))
                   arrows)
                  (for-each 
                   (lambda (arrow)
                     (cond
                       [(var-arrow? arrow)
                        (hash-table-put! tacked-hash-table arrow (not untack-arrows?))]
                       [(tail-arrow? arrow)
                        (for-each-tail-arrows
                         (lambda (arrow) 
                           (hash-table-put! tacked-hash-table arrow (not untack-arrows?)))
                         arrow)]))
                   arrows))
                (invalidate-bitmap-cache))
              
              ;; syncheck:jump-to-binding-occurrence : text -> void
              ;; jumps to the next occurrence, based on the insertion point
              (define/public (syncheck:jump-to-next-bound-occurrence text)
                (jump-to-binding/bound-helper 
                 text 
                 (lambda (pos text vec-ents)
                   (jump-to-next-callback pos text vec-ents))))
              
              ;; syncheck:jump-to-binding-occurrence : text -> void
              (define/public (syncheck:jump-to-binding-occurrence text)
                (jump-to-binding/bound-helper 
                 text 
                 (lambda (pos text vec-ents)
                   (jump-to-binding-callback vec-ents))))
              
              (define (jump-to-binding/bound-helper text do-jump)
                (let ([pos (send text get-start-position)])
                  (when arrow-vectors
                    (let ([arrow-vector (hash-table-get arrow-vectors text (lambda () #f))])
                      (when arrow-vector
                        (let ([vec-ents (filter var-arrow? (vector-ref arrow-vector pos))])
                          (unless (null? vec-ents)
                            (do-jump pos text vec-ents))))))))
              
              ;; jump-to-next-callback : (listof arrow) -> void
              ;; callback for the jump popup menu item
              (define (jump-to-next-callback pos txt input-arrows)
                (unless (null? input-arrows)
                  (let* ([arrow-key (car input-arrows)]
                         [orig-arrows (hash-table-get bindings-table
                                                      (list (var-arrow-start-text arrow-key)
                                                            (var-arrow-start-pos-left arrow-key)
                                                            (var-arrow-start-pos-right arrow-key))
                                                      (lambda () '()))])
                    (unless (or (null? orig-arrows)
                                (null? (cdr orig-arrows))
                                (null? (cddr orig-arrows))) ;; need at least 2 arrows
                      (let loop ([arrows orig-arrows])
                        (cond
                          [(null? arrows) (jump-to (car orig-arrows))]
                          [else (let ([arrow (car arrows)])
                                  (cond
                                    [(and (object=? txt (first arrow))
                                          (<= (second arrow) pos (third arrow)))
                                     (jump-to (if (null? (cdr arrows))
                                                  (car orig-arrows)
                                                  (cadr arrows)))]
                                    [else (loop (cdr arrows))]))]))))))
              
              ;; jump-to : (list text number number) -> void
              (define (jump-to to-arrow)
                (let ([end-text (first to-arrow)]
                      [end-pos-left (second to-arrow)]
                      [end-pos-right (third to-arrow)])
                  (send end-text set-position end-pos-left end-pos-right)
                  (send end-text set-caret-owner #f 'global)))
              
              ;; jump-to-binding-callback : (listof arrow) -> void
              ;; callback for the jump popup menu item
              (define (jump-to-binding-callback arrows)
                (unless (null? arrows)
                  (let* ([arrow (car arrows)]
                         [start-text (var-arrow-start-text arrow)]
                         [start-pos-left (var-arrow-start-pos-left arrow)]
                         [start-pos-right (var-arrow-start-pos-right arrow)])
                    (send start-text set-position start-pos-left start-pos-right)
                    (send start-text set-caret-owner #f 'global))))

              ;; syncheck:jump-to-definition : text -> void
              (define/public (syncheck:jump-to-definition text)
                (let ([pos (send text get-start-position)])
                  (when arrow-vectors
                    (let ([arrow-vector (hash-table-get arrow-vectors text (lambda () #f))])
                      (when arrow-vector
                        (let ([vec-ents (filter def-link? (vector-ref arrow-vector pos))])
                          (unless (null? vec-ents)
                            (jump-to-definition-callback (car vec-ents)))))))))
              
              (define (jump-to-definition-callback def-link)
                (let* ([filename (def-link-filename def-link)]
                       [stx (def-link-syntax def-link)]
                       [frame (fw:handler:edit-file filename)])
                  (when (is-a? frame syncheck-frame<%>)
                    (let ([mod-stx (with-syntax ([in-id stx]) 
                                     (expand #'(module m mzscheme (define in-id 1))))])
                      (with-syntax ([(module m mzscheme (a b (define-values (id) x))) mod-stx])
                        (send frame syncheck:button-callback (syntax id)))))))
              
              (super-instantiate ())))))
      
      (define syncheck-bitmap
        (drscheme:unit:make-bitmap
         (string-constant check-syntax)
         (build-path (collection-path "icons") "syncheck.bmp")))
      
      (define syncheck-frame<%>
        (interface ()
          syncheck:clear-highlighting
          syncheck:button-callback
          syncheck:add-to-cleanup-texts))
      
      (define (make-new-unit-frame% super%)
        (class* super% (syncheck-frame<%>)
          (rename [super-clear-annotations clear-annotations]
		  [super-on-close on-close])
          (define/override (clear-annotations)
            (super-clear-annotations)
            (hide-error-report)
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
          
          (field [cleanup-texts '()])
          (define/public (syncheck:clear-highlighting)
            (let* ([definitions (get-definitions-text)]
                   [locked? (send definitions is-locked?)])
              (send definitions begin-edit-sequence #f)
              (send definitions lock #f)
              (send definitions syncheck:clear-arrows)
              (for-each (lambda (text)
                          (send text begin-edit-sequence)
                          (let* ([list (send text get-style-list)]
                                 [style (send list find-named-style "Standard")])
                            (when style
                              (send text change-style
                                    style 0 (send definitions last-position) #f))))
                        cleanup-texts)
              (for-each (lambda (text) (send text end-edit-sequence)) cleanup-texts)
              (set! cleanup-texts '())
              (send definitions lock locked?)
              (send definitions end-edit-sequence)))
          
          ;; syncheck:add-to-cleanup-texts : (is-a?/c text%) -> void
          (define/public (syncheck:add-to-cleanup-texts txt)
            (unless (memq txt cleanup-texts)
              (set! cleanup-texts (cons txt cleanup-texts))))
          
	  (define/override (on-close)
	    (send report-error-text on-close)
	    (super-on-close))

          (field
           [report-error-parent-panel 'uninitialized-report-error-parent-panel]
           [report-error-panel 'uninitialized-report-error-panel]
           [report-error-text (new fw:scheme:text%)])
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
              (erase))
            
            (drscheme:rep:insert-error-in-text report-error-text (get-interactions-text) message exn #f)
            
            (send* report-error-text
              (set-position 0 0)
              (lock #t)
              (end-edit-sequence))

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
          
          (inherit set-breakables get-breakables reset-offer-kill
                   open-status-line close-status-line update-status-line)
          ;; syncheck:button-callback : (case-> (-> void) ((union #f syntax) -> void)
          ;; this is the only function that has any code running on the user's thread
          (define/public syncheck:button-callback
            (case-lambda
              [() (syncheck:button-callback #f)]
              [(jump-to-id)
               (open-status-line 'drscheme:check-syntax)
               (update-status-line 'drscheme:check-syntax status-init)
               (let-values ([(expanded-expression expansion-completed) (make-traversal)]
                            [(old-break-thread old-custodian) (get-breakables)])
                 (let* ([definitions-text (get-definitions-text)]
                        [drs-eventspace (current-eventspace)]
                        [user-namespace #f]
                        [user-directory #f]
                        [user-custodian #f]
                        [normal-termination? #f]
                        [cleanup
                         (lambda () ; =drs=
                           (set-breakables old-break-thread old-custodian)
                           (enable-evaluation)
                           (send definitions-text end-edit-sequence)
                           (close-status-line 'drscheme:check-syntax))]
                        [kill-termination
                         (lambda ()
                           (unless normal-termination?
                             (parameterize ([current-eventspace drs-eventspace])
                               (queue-callback
                                (lambda ()
                                  (syncheck:clear-highlighting)
                                  (cleanup)
                                  (custodian-shutdown-all user-custodian))))))]
                        [error-display-semaphore (make-semaphore 0)]
                        [uncaught-exception-raised
                         (lambda () ;; =user=
                           (set! normal-termination? #t)
                           (parameterize ([current-eventspace drs-eventspace])
                             (queue-callback
                              (lambda () ;;  =drs=
                                (yield error-display-semaphore) ;; let error display go first
                                (syncheck:clear-highlighting)
                                (cleanup)
                                (custodian-shutdown-all user-custodian)))))]
                        [init-proc
                         (lambda () ; =user=
                           (set-breakables (current-thread) (current-custodian))
                           (set-directory definitions-text)
                           (error-display-handler (lambda (msg exn) ;; =user=
                                                    (parameterize ([current-eventspace drs-eventspace])
                                                      (queue-callback
                                                       (lambda () ;; =drs=
                                                         (report-error msg exn)
                                                         ;; tell uncaught-expception-raised to cleanup
                                                         (semaphore-post error-display-semaphore))))))
                           (error-print-source-location #f) ; need to build code to render error first
                           (current-exception-handler
                            (let ([oh (current-exception-handler)])
                              (lambda (exn)
                                (uncaught-exception-raised)
                                (oh exn))))
                           (update-status-line 'drscheme:check-syntax status-expanding-expression)
                           (set! user-custodian (current-custodian))
                           (set! user-directory (current-directory)) ;; set by set-directory above
                           (set! user-namespace (current-namespace)))])
                   (disable-evaluation) ;; this locks the editor, so must be outside.
                   (send definitions-text begin-edit-sequence #f)
                   (with-lock/edit-sequence
                    (lambda ()
                      (clear-annotations)
                      (reset-offer-kill)
                      (send definitions-text syncheck:init-arrows)
                      (color-range definitions-text
                                   0
                                   (send definitions-text last-position)
                                   base-style-str)
                      (drscheme:eval:expand-program
                       (drscheme:language:make-text/pos definitions-text
                                                        0
                                                        (send definitions-text last-position))
                       (send definitions-text get-next-settings)
                       #t
                       init-proc
                       kill-termination
                       (lambda (sexp loop) ; =user=
                         (cond
                           [(eof-object? sexp)
                            (set! normal-termination? #t)
                            (parameterize ([current-eventspace drs-eventspace])
                              (queue-callback
                               (lambda () ; =drs=
                                 (with-lock/edit-sequence
                                  (lambda ()
                                    (expansion-completed user-namespace user-directory)
                                    (send definitions-text syncheck:sort-bindings-table)))
                                 (cleanup)
                                 (custodian-shutdown-all user-custodian))))]
                           [else
                            (update-status-line 'drscheme:check-syntax status-eval-compile-time)
                            (eval-compile-time-part-of-top-level sexp)
                            (parameterize ([current-eventspace drs-eventspace])
                              (queue-callback
                               (lambda () ; =drs=
                                 (with-lock/edit-sequence
                                  (lambda ()
                                    (open-status-line 'drscheme:check-syntax)
                                    (update-status-line 'drscheme:check-syntax status-coloring-program)
                                    (expanded-expression user-namespace user-directory sexp jump-to-id)
                                    (close-status-line 'drscheme:check-syntax))))))
                            (update-status-line 'drscheme:check-syntax status-expanding-expression)
                            (loop)])))))))]))

          ;; set-directory : text -> void
          ;; sets the current-directory and current-load-relative-directory
          ;; based on the file saved in the definitions-text
          (define (set-directory definitions-text)
            (let* ([tmp-b (box #f)]
                   [fn (send definitions-text get-filename tmp-b)])
              (unless (unbox tmp-b)
                (when fn
                  (let-values ([(base name dir?) (split-path fn)])
                    (current-directory base)
                    (current-load-relative-directory base))))))
          
          ;; with-lock/edit-sequence : (-> void) -> void
          ;; sets and restores some state of the definitions text
          ;; so that edits to the definitions text work out.
          (define/private (with-lock/edit-sequence thnk)
            (let* ([definitions-text (get-definitions-text)]
                   [locked? (send definitions-text is-locked?)])
              (send definitions-text begin-edit-sequence)
              (send definitions-text lock #f)
              (thnk)
              (send definitions-text end-edit-sequence)
              (send definitions-text lock locked?)))
          
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
      
      (define (add-check-syntax-key-bindings keymap)
        (send keymap add-function
              "check syntax"
              (lambda (obj evt)
                (when (is-a? obj editor<%>)
                  (let ([canvas (send obj get-canvas)])
                    (when canvas
                      (let ([frame (send canvas get-top-level-window)])
                        (when (is-a? frame syncheck-frame<%>)
                          (send frame syncheck:button-callback))))))))
        
        (let ([jump-callback
               (lambda (send-msg)
                 (lambda (obj evt)
                   (when (is-a? obj text%)
                     (let ([canvas (send obj get-canvas)])
                       (when canvas
                         (let ([frame (send canvas get-top-level-window)])
                           (when (is-a? frame syncheck-frame<%>)
                             (let ([defs (send frame get-definitions-text)])
                               (when (is-a? defs syncheck-text<%>)
                                 (send-msg defs obj))))))))))])
          (send keymap add-function
                "jump to binding occurrence"
                (jump-callback (lambda (defs obj) (send defs syncheck:jump-to-binding-occurrence obj))))
          (send keymap add-function
                "jump to next bound occurrence"
                (jump-callback (lambda (defs obj) (send defs syncheck:jump-to-next-bound-occurrence obj))))
          (send keymap add-function
                "jump to definition (in other file)"
                (jump-callback (lambda (defs obj) (send defs syncheck:jump-to-definition obj)))))
        
        (send keymap map-function "f6" "check syntax")
        (send keymap map-function "c:c;c:c" "check syntax")
        (send keymap map-function "c:x;b" "jump to binding occurrence")
        (send keymap map-function "c:x;n" "jump to next bound occurrence")
        (send keymap map-function "c:x;d" "jump to definition (in other file)"))
        
      
                                          
                                          
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
                                                                      
                                                                      
                                                                      

      ;; make-traversal : -> (values (namespace syntax (union #f syntax) -> void)
      ;;                             (namespace string[directory] -> void))
      ;; returns a pair of functions that close over some state that
      ;; represents the top-level of a single program. The first value
      ;; is called once for each top-level expression and the second
      ;; value is called once, after all expansion is complete.
      (define (make-traversal)
        (let* ([tl-binders null]
               [tl-varrefs null]
               [tl-requires null]
               [tl-require-for-syntaxes null]
               [tl-tops null]
               [tl-referenced-macros null]
               [tl-bound-in-sources null]
               [expanded-expression
                (lambda (user-namespace user-directory sexp jump-to-id)
                  (let-values ([(new-binders
                                 new-varrefs
                                 new-tops
                                 new-requires
                                 new-require-for-syntaxes
                                 new-referenced-macros
                                 new-bound-in-sources
                                 has-module?)
                                (annotate-basic sexp user-namespace user-directory jump-to-id)])
                    (if has-module?
                        (annotate-complete user-namespace
                                           user-directory
                                           new-binders
                                           new-varrefs
                                           new-tops
                                           new-requires
                                           new-require-for-syntaxes
                                           new-referenced-macros
                                           new-bound-in-sources)
                        (begin
                          (set! tl-binders (append new-binders tl-binders))
                          (set! tl-varrefs (append new-varrefs tl-varrefs))
                          (set! tl-requires (append new-requires tl-requires))
                          (set! tl-require-for-syntaxes (append new-require-for-syntaxes tl-require-for-syntaxes))
                          (set! tl-referenced-macros (append new-referenced-macros tl-referenced-macros))
                          (set! tl-bound-in-sources (append new-bound-in-sources tl-bound-in-sources))
                          (set! tl-tops (append new-tops tl-tops))))))]
               [expansion-completed
                (lambda (user-namespace user-directory)
                  (annotate-complete user-namespace
                                     user-directory
                                     tl-binders
                                     tl-varrefs
                                     tl-tops
                                     tl-requires
                                     tl-require-for-syntaxes
                                     tl-referenced-macros
                                     tl-bound-in-sources))])
          (values expanded-expression expansion-completed)))
      
      
      ;; type req/tag = (make-req/tag syntax sexp boolean)
      (define-struct req/tag (req-stx req-sexp used?))
      
      ;; annotate-complete :    namespace
      ;;                        string[directory]
      ;;                        (listof syntax)
      ;;                        (listof (cons boolean syntax))
      ;;                        (listof syntax)
      ;;                        (listof syntax)
      ;;                        (listof (cons boolean syntax[original]))
      ;;                        (listof (cons syntax[original] syntax[original]))
      ;;                     -> void
      ;;
      ;; annotates the non-local portions of a complete program.
      ;; for the purposes of check syntax, a complete program is either
      ;; a module expression, or everything at the top level.
      ;;
      ;; the inputs match the outputs of annotate-basic, except this
      ;; accepts the user's namespace in addition and doesn't accept
      ;; the boolean from annotate-basic.
      (define (annotate-complete user-namespace
                                 user-directory
                                 binders
                                 varrefs
                                 tops
                                 requires
                                 require-for-syntaxes
                                 referenced-macros
                                 bound-in-sources)
        (let-values ([(non-require-varrefs non-require-referenced-macros)
                      (annotate-require-vars 
                       varrefs
                       requires
                       require-for-syntaxes
                       referenced-macros
                       user-namespace
                       user-directory)])
          (annotate-variables user-namespace
                              binders
                              non-require-varrefs
                              non-require-referenced-macros
                              tops))
        (annotate-bound-in-sources bound-in-sources))

      ;; annotate-require-vars :    (listof (cons boolean syntax[identifier]))
      ;;                            (listof syntax)
      ;;                            (listof syntax)
      ;;                            (listof (cons boolean syntax[original]))
      ;;                            namespace
      ;;                            string[directory]
      ;;                         -> (listof syntax) (listof syntax)
      ;; returns the sublist of `varrefs' that did not come from module imports and
      ;;     and the sublist of `referenced-macros' that didn't come from module imports
      ;; effect: colors all require-bound ids from `varrefs' and draws arrow for them. 
      (define (annotate-require-vars varrefs/levels requires require-for-syntaxes referenced-macros
                                      user-namespace user-directory)
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
                                 [from-module?
                                  (if high-level?
                                      (annotate-require-var req-syn/tags varref #t user-namespace user-directory)
                                      (annotate-require-var req/tags varref #f  user-namespace user-directory))])
                            (if from-module?
                                (loop (cdr varrefs/levels))
                                (cons varref (loop (cdr varrefs/levels)))))]))]

               [reduced-referenced-macros
                (filter (annotate-macro req/tags #f)
                        (map cdr (filter (lambda (x) (not (car x))) referenced-macros)))]

               ;; hopefully, this is the empty list 
               ;; (no idea if it ever can be non-empty or what it would mean...)
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
                          (add-mouse-over 
                           stx
                           (format mouse-over-syntax-import
                                   (syntax-object->datum stx)
                                   (syntax-object->datum (req/tag-req-stx req/tag))))
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
      (define (annotate-require-var req/tags varref high-level? user-namespace user-directory)
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
                            (connect-syntaxes (req/tag-req-stx req/tag) varref)
                            (add-jump-to-definition 
                             varref 
                             (get-require-filename
                              (req/tag-req-stx req/tag)
                              user-namespace
                              user-directory))
                            (add-mouse-over 
                             varref
                             (format mouse-over-variable-import
                                     (syntax-object->datum varref)
                                     (syntax-object->datum (req/tag-req-stx req/tag))))))
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
      (define (annotate-variables user-namespace binders varrefs referenced-macros tops)
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
          (for-each (annotate-varref (handle-no-binders/top user-namespace) vars-ht binders-ht #f)
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
      (define (handle-no-binders/top user-namespace)
        (lambda (varref)
          (let ([defined-in-user-namespace?
                 (parameterize ([current-namespace user-namespace])
                   (namespace-variable-value (syntax-e varref) #t (lambda () #f)))])
            (if defined-in-user-namespace?
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
	  (when (and (is-a? from-source text%)
                     (is-a? to-source text%))
            (let ([to-syncheck-text (find-syncheck-text to-source)]
                  [from-syncheck-text (find-syncheck-text from-source)])
              (when (and to-syncheck-text
                         from-syncheck-text
                         (eq? to-syncheck-text from-syncheck-text)
                         (syntax-position from)
                         (syntax-span from)
                         (syntax-position to)
                         (syntax-span to))
                (let* ([from-pos-left (- (syntax-position from) 1)]
                       [from-pos-right (+ from-pos-left (syntax-span from))]
                       [to-pos-left (- (syntax-position to) 1)]
                       [to-pos-right (+ to-pos-left (syntax-span to))])
                  (unless (= from-pos-left to-pos-left)
                    (send from-syncheck-text syncheck:add-arrow
                          from-source from-pos-left from-pos-right
                          to-source to-pos-left to-pos-right))))))))
      
      ;; add-mouse-over : syntax[original] string -> void
      ;; registers the range in the editor so that a mouse over
      ;; this area shows up in the status line.
      (define (add-mouse-over stx str)
        (let* ([source (syntax-source stx)])
	  (when (is-a? source text%)
            (let ([syncheck-text (find-syncheck-text source)])
              (when (and syncheck-text
                         (syntax-position stx)
                         (syntax-span stx))
                (let* ([pos-left (- (syntax-position stx) 1)]
                       [pos-right (+ pos-left (syntax-span stx))])
                  (send syncheck-text syncheck:add-mouse-over-status
                        source pos-left pos-right str)))))))
      
      ;; add-jump-to-definition : syntax[original] string[filename] -> void
      ;; registers the range in the editor so that a mouse over
      ;; this area shows up in the status line.
      (define (add-jump-to-definition stx filename)
        (let* ([source (syntax-source stx)])
	  (when (is-a? source text%)
            (let ([syncheck-text (find-syncheck-text source)])
              (when (and syncheck-text
                         (syntax-position stx)
                         (syntax-span stx))
                (let* ([pos-left (- (syntax-position stx) 1)]
                       [pos-right (+ pos-left (syntax-span stx))])
                  (send syncheck-text syncheck:add-jump-to-definition
                        source
                        pos-left
                        pos-right
                        stx
                        filename)))))))
      
      ;; find-syncheck-text : text% -> (union #f (is-a?/c syncheck-text<%>))
      (define (find-syncheck-text text)
        (let loop ([text text])
          (cond
            [(is-a? text syncheck-text<%>) text]
            [else 
             (let ([admin (send text get-admin)])
               (and (is-a? admin editor-snip-editor-admin<%>)
                    (let* ([enclosing-editor-snip (send admin get-snip)]
                           [editor-snip-admin (send enclosing-editor-snip get-admin)]
                           [enclosing-editor (send editor-snip-admin get-editor)])
                      (loop enclosing-editor))))])))
      
      ;; annotate-basic : syntax 
      ;;                  namespace
      ;;                  string
      ;;                  syntax[id]
      ;;                  -> (values (listof syntax)
      ;;                             (listof (cons boolean syntax))
      ;;                             (listof syntax)
      ;;                             (listof syntax)
      ;;                             (listof (cons boolean syntax[original]))
      ;;                             (listof (cons syntax[original] syntax[original]))
      ;;                             boolean)
      ;; annotates the lexical structure of the program `sexp', except
      ;; for the variables in the program. returns the variables in several
      ;; lists -- the first is the ones that occur in binding positions
      ;; and the second is those that occur in bound positions. The third
      ;; is those that occur in #%top's. 
      ;; The next value is all of the require expressions and then all of the
      ;; require-for-syntax expressions.
      ;; the next is the list of all original macro references.
      ;; the last to last is a boolean indicating if there was a `module' in the expanded expression.
      ;; the last is a hash-table that describes the relative tail positions in the expression
      
      ;; the booleans in the lists indicate if the variables or macro references
      ;; were on the rhs of a define-syntax (boolean is #t) or not (boolean is #f)
      (define (annotate-basic sexp user-namespace user-directory jump-to-id)
        (let ([binders null]
              [varrefs null]
              [tops null]
              [requires null]
              [require-for-syntaxes null]
              [referenced-macros null]
              [bound-in-sources null]
              [has-module? #f]
              [tail-ht (make-hash-table)])
          (let level-loop ([sexp sexp]
                           [high-level? #f])
            (annotate-original-keywords sexp)
            (set! bound-in-sources (combine-bound-in-source sexp bound-in-sources))
            (set! referenced-macros (get-referenced-macros high-level? sexp referenced-macros))
            (set! varrefs (flatten-bis-tree #t (syntax-property sexp 'bound-in-source) varrefs))
            (set! binders (flatten-cons-tree 'no-cons (syntax-property sexp 'binding-in-source) binders))
            (let ([loop (lambda (sexp) (level-loop sexp high-level?))])
              (syntax-case* sexp (lambda case-lambda if begin begin0 let-values letrec-values set!
                                   quote quote-syntax with-continuation-mark 
                                   #%app #%datum #%top #%plain-module-begin
                                   define-values define-syntaxes module
                                   require require-for-syntax provide)
                (if high-level? module-transformer-identifier=? module-identifier=?)
                [(lambda args bodies ...)
                 (begin
                   (annotate-raw-keyword sexp)
                   (annotate-tail-position/last sexp (syntax->list (syntax (bodies ...))) tail-ht)
                   (set! binders (combine/color-binders (syntax args) binders bound-variable-style-str))
                   (for-each loop (syntax->list (syntax (bodies ...)))))]
                [(case-lambda [argss bodiess ...]...)
                 (begin
                   (annotate-raw-keyword sexp)
                   (for-each (lambda (bodies/stx) (annotate-tail-position/last sexp 
                                                                               (syntax->list bodies/stx)
                                                                               tail-ht))
                             (syntax->list (syntax ((bodiess ...) ...))))
                   (for-each
                    (lambda (args bodies)
                      (set! binders (combine/color-binders args binders bound-variable-style-str))
                      (for-each loop (syntax->list bodies)))
                    (syntax->list (syntax (argss ...)))
                    (syntax->list (syntax ((bodiess ...) ...)))))]
                [(if test then else)
                 (begin
                   (annotate-raw-keyword sexp)
                   (annotate-tail-position sexp (syntax then) tail-ht)
                   (annotate-tail-position sexp (syntax else) tail-ht)
                   (loop (syntax test))
                   (loop (syntax else))
                   (loop (syntax then)))]
                [(if test then)
                 (begin
                   (annotate-raw-keyword sexp)
                   (annotate-tail-position sexp (syntax then) tail-ht)
                   (loop (syntax test))
                   (loop (syntax then)))]
                [(begin bodies ...)
                 (begin
                   (annotate-raw-keyword sexp)
                   (annotate-tail-position/last sexp (syntax->list (syntax (bodies ...))) tail-ht)
                   (for-each loop (syntax->list (syntax (bodies ...)))))]
                
                [(begin0 bodies ...)
                 (begin
                   (annotate-raw-keyword sexp)
                   (for-each loop (syntax->list (syntax (bodies ...)))))]
                
                [(let-values (((xss ...) es) ...) bs ...)
                 (begin
                   (annotate-raw-keyword sexp)
                   (annotate-tail-position/last sexp (syntax->list (syntax (bs ...))) tail-ht)
                   (for-each (lambda (x) (set! binders (combine/color-binders x binders bound-variable-style-str)))
                             (syntax->list (syntax ((xss ...) ...))))
                   (for-each loop (syntax->list (syntax (es ...))))
                   (for-each loop (syntax->list (syntax (bs ...)))))]
                [(letrec-values (((xss ...) es) ...) bs ...)
                 (begin
                   (annotate-raw-keyword sexp)
                   (annotate-tail-position/last sexp (syntax->list (syntax (bs ...))) tail-ht)
                   (for-each (lambda (x) (set! binders (combine/color-binders x binders bound-variable-style-str)))
                             (syntax->list (syntax ((xss ...) ...))))
                   (for-each loop (syntax->list (syntax (es ...))))
                   (for-each loop (syntax->list (syntax (bs ...)))))]
                [(set! var e)
                 (begin
                   (annotate-raw-keyword sexp)
                   (set! varrefs (cons (cons high-level? (syntax var)) varrefs))
                   (loop (syntax e)))]
                [(quote datum)
                 (begin 
                   (annotate-raw-keyword sexp)
                   (color-internal-structure (syntax datum) constant-style-str))]
                [(quote-syntax datum)
                 (begin 
                   (annotate-raw-keyword sexp)
                   (color-internal-structure (syntax datum) constant-style-str))]
                [(with-continuation-mark a b c)
                 (begin
                   (annotate-raw-keyword sexp)
                   (annotate-tail-position sexp (syntax c) tail-ht)
                   (loop (syntax a))
                   (loop (syntax b))
                   (loop (syntax c)))]
                [(#%app pieces ...)
                 (begin
                   (annotate-raw-keyword sexp)
                   (for-each loop (syntax->list (syntax (pieces ...)))))]
                [(#%datum . datum)
                 (color-internal-structure (syntax datum) constant-style-str)]
                [(#%top . var)
                 (begin
                   (set! tops (cons (syntax var) tops)))]
                
                [(define-values vars b)
                 (begin
                   (annotate-raw-keyword sexp)
                   (set! binders (combine/color-binders (syntax vars) binders bound-variable-style-str))
                   (when jump-to-id
                     (for-each (lambda (id)
                                 (when (module-identifier=? id jump-to-id)
                                   (jump-to id)))
                               (syntax->list (syntax vars))))
                   (loop (syntax b)))]
                [(define-syntaxes names exp)
                 (begin
                   (annotate-raw-keyword sexp)
                   (set! binders (combine/color-binders (syntax names) binders keyword-style-str))
                   (level-loop (syntax exp) #t))]
                [(module m-name lang (#%plain-module-begin bodies ...))
                 (begin
                   (set! has-module? #t)
                   ((annotate-require-open user-namespace user-directory) (syntax lang))
                   (set! requires (cons (syntax lang) requires))
                   (annotate-raw-keyword sexp)
                   (for-each loop (syntax->list (syntax (bodies ...)))))]
                
                ; top level or module top level only:
                [(require require-specs ...)
                 (let ([new-specs (map trim-require-prefix
                                       (syntax->list (syntax (require-specs ...))))])
                   (for-each (annotate-require-open user-namespace user-directory) new-specs)
                   (set! requires (append new-specs requires))
                   (annotate-raw-keyword sexp))]
                [(require-for-syntax require-specs ...)
                 (let ([new-specs (map trim-require-prefix (syntax->list (syntax (require-specs ...))))])
                   (for-each (annotate-require-open user-namespace user-directory) new-specs)
                   (set! require-for-syntaxes (append new-specs require-for-syntaxes))
                   (annotate-raw-keyword sexp))]
                
                ; module top level only:
                [(provide provide-specs ...)
                 (let ([provided-vars (apply 
                                       append
                                       (map extract-provided-vars
                                            (syntax->list (syntax (provide-specs ...)))))])
                   (set! varrefs (append (map (lambda (x) (cons #f x)) provided-vars) varrefs))
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
          (add-tail-ht-links tail-ht)
          (values binders 
                  varrefs
                  tops
                  requires
                  require-for-syntaxes
                  referenced-macros
                  bound-in-sources
                  has-module?)))

      ;; annotate-tail-position/last : (listof syntax) -> void
      (define (annotate-tail-position/last orig-stx stxs tail-ht)
        (unless (null? stxs)
          (annotate-tail-position orig-stx (car (last-pair stxs)) tail-ht)))
      
      ;; annotate-tail-position : syntax -> boid
      ;; colors the parens (if any) around the argument
      ;; to indicate this is a tail call.
      (define (annotate-tail-position orig-stx tail-stx tail-ht)
        (hash-table-put!
         tail-ht 
         orig-stx 
         (cons
          tail-stx
          (hash-table-get 
           tail-ht
           orig-stx
           (lambda () null)))))
      
      ;; paren? : character -> boolean
      (define (paren? char)
        (memq char '(#\( #\[ #\{ #\} #\] #\))))
      
      ;; annotate-bound-in-sources : (listof (cons syntax[orig] syntax[orig])) -> void
      ;; adds arrows and colors between pairs found in the 'bound-in-source syntax property.
      (define (annotate-bound-in-sources biss)
        (for-each
         (lambda (bis)
           (color (car bis) bound-variable-style-str)
           (color (cdr bis) bound-variable-style-str)
           (connect-syntaxes (car bis) (cdr bis)))
         biss))
      
      ;; combine-bound-in-sources : syntax (listof (cons syntax[orig] syntax[orig))
      ;;                         -> (listof (cons syntax[orig] syntax[orig))
      (define (combine-bound-in-source stx old-biss)
        (let loop ([bis (syntax-property stx 'bound-in-source)]
                   [acc old-biss])
          (cond
            [(and (cons? bis)
                  (identifier? (car bis))
                  (identifier? (cdr bis)))
             (if (and (syntax-original? (car bis))
                      (syntax-original? (cdr bis)))
                 (cons bis acc)
                 acc)]
            [(cons? bis)
             (loop (car bis)
                   (loop (cdr bis)
                         acc))]
            [else acc])))

      ;; annotate-require-open : namespace string -> (stx -> void)
      ;; relies on current-module-name-resolver, which in turn depends on
      ;; current-directory and current-namespace
      (define (annotate-require-open user-namespace user-directory)
	(lambda (require-spec)
	  (when (syntax-original? require-spec)
	    (let ([source (syntax-source require-spec)])
	      (when (and (is-a? source text%)
			 (syntax-position require-spec)
			 (syntax-span require-spec))
                (let ([syncheck-text (find-syncheck-text source)])
                  (when syncheck-text
                    (let* ([start (- (syntax-position require-spec) 1)]
                           [end (+ start (syntax-span require-spec))]
                           [file (get-require-filename require-spec user-namespace user-directory)])
                      (when file
                        (send syncheck-text syncheck:add-menu
                              source
                              start end 
                              #f
                              (make-require-open-menu file)))))))))))
      
      ;; get-require-filename : syntax namespace string[directory] -> filename
      ;; finds the filename corresponding to the require in stx
      (define (get-require-filename stx user-namespace user-directory)
        (let* ([datum (syntax-object->datum stx)]
               [sym 
                (and (not (symbol? datum))
                     (parameterize ([current-namespace user-namespace]
                                    [current-directory user-directory]
                                    [current-load-relative-directory user-directory])
                       ((current-module-name-resolver) datum #f #f)))])
          (and (symbol? sym)
               (module-name-sym->filename sym))))
      
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

      ;; flatten-bis-tree : (union 'no-cons boolean)
      ;;                    cons-tree
      ;;                    (union (listof syntax[original]) (listof (cons boolean syntax[original])))
      ;;                 -> (listof syntax[original])
      ;; similar to flatten-cons-tree, except it
      ;; flattesn the tree associated with the 'bound-in-source property
      (define (flatten-bis-tree level ct acc)
        (let loop ([ct ct]
                   [acc acc])
          (cond
            [(and (pair? ct)
                  (syntax? (car ct))
                  (syntax? (cdr ct)))
             (if (and (identifier? (car ct))
                      (syntax-original? (car ct))
                      (identifier? (cdr ct))
                      (syntax-original? (cdr ct)))
                 (if (boolean? level)
                     (list* (cons level (car ct))
                            (cons level (cdr ct))
                            acc)
                     (list* (car ct) (cdr ct) acc))
                 acc)]
            [(pair? ct) (loop (car ct) (loop (cdr ct) acc))]
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
        (let ([origin (syntax-property stx 'origin)])
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
      
      
      ;; color-internal-structure : syntax str -> void
      (define (color-internal-structure stx style-name)
        (let ([ht (make-hash-table)]) 
          ;; ht : stx -o> true
          ;; indicates if we've seen this syntax object before
          
          (let loop ([stx stx]
                     [datum (syntax-object->datum stx)])
	    (unless (hash-table-get ht datum (lambda () #f))
	      (hash-table-put! ht datum #t)
              (cond
	       [(pair? stx) 
		(loop (car stx) (car datum))
		(loop (cdr stx) (cdr datum))]
	       [(syntax? stx)
                (when (syntax-original? stx)
                  (color stx style-name))
                (let ([stx-e (syntax-e stx)]) 
                  (cond
		   [(cons? stx-e)
		    (loop (car stx-e) (car datum))
		    (loop (cdr stx-e) (cdr datum))]
		   [(null? stx-e)
		    (void)]
		   [(vector? stx-e)
		    (for-each loop
			      (vector->list stx-e)
			      (vector->list datum))]
		   [(box? stx-e)
		    (loop (unbox stx-e) (unbox datum))]
		   [else (void)]))])))))

      ;; jump-to : syntax -> void
      (define (jump-to stx)
        (let ([src (syntax-source stx)]
              [pos (syntax-position stx)]
              [span (syntax-span stx)])
          (when (and (is-a? src text%)
                     pos
                     span)
            (send src set-position (- pos 1) (+ pos span -1)))))
      
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
          (add-to-cleanup-texts source)
          (send source change-style style start finish #f)))

      ;; hash-table[syntax -o> (listof syntax)] -> void
      (define (add-tail-ht-links tail-ht)
        (hash-table-for-each
         tail-ht
         (lambda (stx-from stx-tos)
           (for-each (lambda (stx-to) (add-tail-ht-link stx-from stx-to))
                     stx-tos))))
      
      ;; add-tail-ht-link : syntax syntax -> void
      (define (add-tail-ht-link from-stx to-stx)
        (let* ([to-src (syntax-source to-stx)]
               [from-src (syntax-source from-stx)]
               [to-outermost-src (and (is-a? to-src editor<%>)
                                      (find-outermost-editor to-src))]
               [from-outermost-src (and (is-a? from-src editor<%>)
                                        (find-outermost-editor from-src))])
          (when (and (is-a? to-outermost-src syncheck-text<%>)
                     (eq? from-outermost-src to-outermost-src))
            (let ([from-pos (syntax-position from-stx)]
                  [to-pos (syntax-position to-stx)])
              (when (and from-pos to-pos)
                (send to-outermost-src syncheck:add-tail-arrow
                      from-src (- from-pos 1)
                      to-src (- to-pos 1)))))))
      
      ;; add-to-cleanup-texts : (is-a?/c editor<%>) -> void
      (define (add-to-cleanup-texts ed)
        (let ([canvas (send (find-outermost-editor ed) get-canvas)])
          (when canvas
            (let ([frame (send canvas get-top-level-window)])
              (when (is-a? frame syncheck-frame<%>)
                (send frame syncheck:add-to-cleanup-texts ed))))))
      
      (define (find-outermost-editor ed)
        (let loop ([ed ed])
          (let ([admin (send ed get-admin)])
            (if (is-a? admin editor-snip-editor-admin<%>)
                (let* ([enclosing-snip (send admin get-snip)]
                       [enclosing-snip-admin (send enclosing-snip get-admin)])
                  (loop (send enclosing-snip-admin get-editor)))
                ed))))
      
      ;; make-rename-menu : stx[original] (hash-table symbol (listof syntax)) -> void
      (define (make-rename-menu stx vars-ht)
        (let ([source (syntax-source stx)])
          (when (is-a? source text%)
            (let ([syncheck-text (find-syncheck-text source)])
              (when syncheck-text
                (let* ([name-to-offer (format "~a" (syntax-object->datum stx))]
                       [start (- (syntax-position stx) 1)]
                       [fin (+ start (syntax-span stx))])
                  (send syncheck-text syncheck:add-menu
                        source start fin (syntax-e stx)
                        (lambda (menu)
                          (instantiate menu-item% ()
                            (parent menu)
                            (label (format (string-constant cs-rename-var) name-to-offer))
                            (callback
                             (lambda (x y)
                               (let ([frame-parent (find-menu-parent menu)])
                                 (rename-callback name-to-offer stx vars-ht frame-parent)))))))))))))
      
      ;; find-parent : menu-item-container<%> -> (union #f (is-a?/c top-level-window<%>)
      (define (find-menu-parent menu)
        (let loop ([menu menu])
          (cond
            [(is-a? menu menu-bar%) (send menu get-frame)]
            [(is-a? menu popup-menu%)
             (let ([target (send menu get-popup-target)])
               (cond
                 [(is-a? target editor<%>) 
                  (let ([canvas (send target get-canvas)])
                    (and canvas
                         (send canvas get-top-level-window)))]
                 [(is-a? target window<%>) 
                  (send target get-top-level-window)]
                 [else #f]))]
            [(is-a? menu menu-item<%>) (loop (send menu get-parent))]
            [else #f])))

      ;; rename-callback : string syntax[original] (listof syntax) (union #f (is-a?/c top-level-window<%>)) -> void
      ;; callback for the rename popup menu item
      (define (rename-callback name-to-offer stx vars-ht parent)
        (let ([new-sym 
               (fw:keymap:call/text-keymap-initializer
                (lambda ()
                  (get-text-from-user
                   (string-constant cs-rename-id)
                   (format (string-constant cs-rename-var-to) name-to-offer)
                   parent
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
                                      new-sym)
                              parent
                              '(ok stop))]
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
                    [else (if (and (eq? (syntax-source fst)
                                        (syntax-source (car rst)))
                                   (= (syntax-position fst)
                                      (syntax-position (car rst))))
                              (loop fst (cdr rst))
                              (cons fst (loop (car rst) (cdr rst))))]))]))
      
      (add-check-syntax-key-bindings (drscheme:rep:get-drs-bindings-keymap))
      
      (drscheme:get/extend:extend-definitions-text make-graphics-text%)
      (drscheme:get/extend:extend-unit-frame make-new-unit-frame% #f))))
