
(module panel mzscheme
  (require (lib "unitsig.ss")
	   (lib "class.ss")
	   (lib "class100.ss")
	   "sig.ss"
	   "../macro.ss"
	   (lib "mred-sig.ss" "mred")
	   (lib "list.ss")
	   (lib "etc.ss"))
  
  (provide panel@)
  
  (define panel@
    (unit/sig framework:panel^
      (import mred^)
      
      (rename [-editor<%> editor<%>])
      
      (define (list-set! _list _i ele)
        (let loop ([lst _list]
                   [i _i])
          (cond
            [(null? lst) (error 'list-set! "index too large for list, args: ~e ~e ~e"
                                _list _i ele)]
            [(zero? i) (set-car! lst ele)]
            [else (loop (cdr lst) (- i 1))])))
      
      (define single<%> (interface (area-container<%>) active-child))
      (define single-mixin
        (mixin (area-container<%>) (single<%>)
          (inherit get-alignment)
          (rename [super-after-new-child after-new-child])
          (override after-new-child container-size place-children)
          [define after-new-child
            (lambda (c)
              (if current-active-child
                  (send c show #f)
                  (set! current-active-child c)))]
          [define container-size
            (lambda (l)
              (if (null? l)
                  (values 0 0)
                  (values (apply max (map car l)) (apply max (map cadr l)))))]
          [define place-children
            (lambda (l width height)
              (let-values ([(h-align-spec v-align-spec) (get-alignment)])
                (let ([align
                       (lambda (total-size spec item-size)
                         (floor
                          (case spec
                            [(center) (- (/ total-size 2) (/ item-size 2))]
                            [(left top) 0]
                            [(right bottom) (- total-size item-size)]
                            [else (error 'place-children
                                         "alignment spec is unknown ~a~n" spec)])))])
                  (map (lambda (l) 
                         (let*-values ([(min-width min-height v-stretch? h-stretch?)
                                        (apply values l)]
                                       [(x this-width)
                                        (if h-stretch?
                                            (values 0 width)
                                            (values (align width h-align-spec min-width)
                                                    min-width))]
                                       [(y this-height)
                                        (if v-stretch?
                                            (values 0 height)
                                            (values (align height v-align-spec min-height)
                                                    min-height))])
                           (list x y this-width this-height)))
                       l))))]
          
          (inherit get-children)
          [define current-active-child #f]
          (public active-child)
          [define active-child
            (case-lambda
             [() current-active-child]
             [(x) 
              (unless (memq x (get-children))
                (error 'active-child "got a panel that is not a child: ~e" x))
              (unless (eq? x current-active-child)
                (for-each (lambda (x) (send x show #f))
                          (get-children))
                (set! current-active-child x)
                (send current-active-child show #t))])]
          (super-instantiate ())))
      
      (define single-window<%> (interface (single<%> window<%>)))
      (define single-window-mixin
        (mixin (single<%> window<%>) (single-window<%>)
          (inherit get-client-size get-size)
          (rename [super-container-size container-size])
          (override container-size)
          [define container-size
            (lambda (l)
              (let-values ([(super-width super-height) (super-container-size l)]
                           [(client-width client-height) (get-client-size)]
                           [(window-width window-height) (get-size)]
                           [(calc-size)
                            (lambda (super client window)
                              (+ super (max 0 (- window client))))])
                
                (values
                 (calc-size super-width client-width window-width)
                 (calc-size super-height client-height window-height))))]
          (super-instantiate ())))
      
      (define multi-view<%>
        (interface (area-container<%>)
          split-vertically
          split-horizontally
          collapse))
      
      (define multi-view-mixin
        (mixin (area-container<%>) (multi-view<%>) 
          (init-field parent editor)
          (public get-editor-canvas% get-vertical% get-horizontal%)
          [define get-editor-canvas%
            (lambda ()
              editor-canvas%)]
          [define get-vertical%
            (lambda ()
              vertical-panel%)]
          [define get-horizontal%
            (lambda ()
              horizontal-panel%)]
          
          (public split-vertically split-horizontally)
          
          [define split
            (lambda (p%)
              (let ([canvas (send (send parent get-top-level-window) get-edit-target-window)]
                    [ec% (get-editor-canvas%)])
                (when (and canvas
                           (is-a? canvas ec%)
                           (eq? (send canvas get-editor) editor))
                  (let ([p (send canvas get-parent)])
                    (send p change-children (lambda (x) null))
                    (let ([pc (make-object p% p)])
                      (send (make-object ec% (make-object vertical-panel% pc) editor) focus)
                      (make-object ec% (make-object vertical-panel% pc) editor))))))]
          [define split-vertically
            (lambda ()
              (split (get-vertical%)))]
          [define split-horizontally
            (lambda ()
              (split (get-horizontal%)))]
          
          (public collapse)
          (define collapse
            (lambda ()
              (let ([canvas (send (send parent get-top-level-window) get-edit-target-window)]
                    [ec% (get-editor-canvas%)])
                (when (and canvas
                           (is-a? canvas ec%)
                           (eq? (send canvas get-editor) editor))
                  (let ([p (send canvas get-parent)])
                    (if (eq? p this)
                        (bell)
                        (let* ([sp (send p get-parent)]
                               [p-to-remain (send sp get-parent)])
                          (send p-to-remain change-children (lambda (x) null))
                          (send (make-object ec% p-to-remain editor) focus))))))))
          
          
          (super-instantiate () (parent parent))
          (make-object (get-editor-canvas%) this editor)))
      
      (define single% (single-window-mixin (single-mixin panel%)))
      (define single-pane% (single-mixin pane%))
      (define multi-view% (multi-view-mixin vertical-panel%))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (define up/down-cursor (make-object cursor% 'size-n/s))

      ;; type gap = (make-gap number area<%> percentage number area<%> percentage)
      (define-struct gap (before before-y before-percentage after after-y after-percentage))

      ;; type percentage : (make-percentage number)
      (define-struct percentage (%))
      
      (define dragable<%>
        (interface ((class->interface vertical-panel%))
          after-percentage-change
          set-percentages
          get-percentages))

      (define vertical-dragable-mixin
        (mixin ((class->interface vertical-panel%)) (dragable<%>)
          (init parent)
          (super-instantiate (parent))
          (inherit get-client-size container-flow-modified)
          
          (init-field [bar-thickness 5])
          
          ;; percentages : (listof percentage)
          (define percentages null)

          ;; get-percentages : -> (listof number)
          (define/public (get-percentages)
            (map percentage-% percentages))

          (define/public (set-percentages ps)
            (unless (and (list? ps)
                         (andmap number? ps)
                         (= 1 (apply + ps))
                         (andmap positive? ps))
              (error 'set-percentages 
                     "expected a list of numbers that are all positive and sum to 1, got: ~e"
                     ps))
            (unless (= (length ps) (length (get-children)))
              (error 'set-percentages 
                     "expected a list of numbers whose length is the number of children: ~a, got ~e"
                     (length (get-children))
                     ps))
            (let ([available-height (get-available-height)])
              (unless (andmap
                       (lambda (p child)
                         ((* p available-height) . >= . (send child min-height)))
                       ps
                       (get-children))
                (error 'set-percentages
                       "the percentages would violate minimum height requirements of the children: ~e"
                       ps)))
            (set! percentages (map make-percentage ps))
            (container-flow-modified))

          (define/public (after-percentage-change)
            (void))
          
          (define/private (get-available-height)
            (let-values ([(width height) (get-client-size)])
              (- height (* bar-thickness (- (length (get-children)) 1)))))

          (inherit get-children)
          
          (define/private (update-percentages)
            (let* ([len-children (length (get-children))])
              (unless (= len-children (length percentages))
                (let ([rat (/ 1 len-children)])
                  (set! percentages (build-list len-children (lambda (i) (make-percentage rat)))))
                (after-percentage-change))))
          
          (define/override (after-new-child child)
            (update-percentages))
          
          (define resizing-y #f)
          (define resizing-gap #f)
          
          (rename [super-on-subwindow-event on-subwindow-event])
          (inherit set-cursor)
          (define/override (on-subwindow-event receiver evt)
            (let ([gap
                   (ormap (lambda (gap) 
                            (and (<= (gap-before-y gap) (send evt get-y) (gap-after-y gap))
                                 gap))
                          cursor-gaps)])
              (set-cursor (and (or gap
                                   resizing-y)
                               up/down-cursor))
              (cond
                [(and gap (send evt button-down? 'left))
                 (set! resizing-y (send evt get-y))
                 (set! resizing-gap gap)]
                [(and resizing-y (send evt button-up?))
                 (set! resizing-y #f)
                 (set! resizing-gap #f)]
                [(and resizing-y (send evt moving?))
                 (let-values ([(width height) (get-client-size)])
                   (let* ([before (gap-before resizing-gap)]
                          [before-percentage (gap-before-percentage resizing-gap)]
                          [after (gap-after resizing-gap)]
                          [after-percentage (gap-after-percentage resizing-gap)]
                          [available-height (get-available-height)]
                          [change-in-percentage (/ (- resizing-y (send evt get-y)) available-height)]
                          [new-before (- (percentage-% before-percentage) change-in-percentage)]
                          [new-after (+ (percentage-% after-percentage) change-in-percentage)])
                     (when (and ((* new-before available-height) . > . (send before min-height))
                                ((* new-after available-height) . > . (send after min-height)))
                       (set-percentage-%! before-percentage new-before)
                       (set-percentage-%! after-percentage new-after)
                       (after-percentage-change)
                       (set! resizing-y (send evt get-y))
                       (container-flow-modified))))]
                [else (super-on-subwindow-event receiver evt)])))
          
          (define cursor-gaps null)
          
          (rename [super-place-children place-children])
          (define/override (place-children _infos width height)
            (set! cursor-gaps null)
            (update-percentages)
            (cond
              [(null? _infos) null]
              [(null? (cdr _infos)) (list (list 0 0 width height))]
              [else
               (let ([available-height (get-available-height)]
                     [show-error
                      (lambda (n)
                        (error 'panel.ss::dragable-panel "internal error.~a" n))])
                 (let loop ([percentages percentages]
                            [children (get-children)]
                            [infos _infos]
                            [y 0])
                   (cond
                     [(null? percentages)
                      (unless (null? infos) (show-error 1))
                      (unless (null? children) (show-error 2))
                      null]
                     [(null? (cdr percentages))
                      (when (null? infos) (show-error 3))
                      (when (null? children) (show-error 4))
                      (unless (null? (cdr infos)) (show-error 5))
                      (unless (null? (cdr children)) (show-error 6))
                      (list (list 0 y width (- height y)))]
                     [else
                      (when (null? infos) (show-error 7))
                      (when (null? children) (show-error 8))
                      (when (null? (cdr infos)) (show-error 9))
                      (when (null? (cdr children)) (show-error 10))
                      (let* ([info (car infos)]
                             [percentage (car percentages)]
                             [this-space (floor (* (percentage-% percentage) available-height))])
                        (set! cursor-gaps (cons (make-gap (car children)
                                                          (+ y this-space)
                                                          percentage
                                                          (cadr children)
                                                          (+ y this-space bar-thickness)
                                                          (cadr percentages))
                                                cursor-gaps))
                        (cons (list 0 y width this-space)
                              (loop (cdr percentages)
                                    (cdr children)
                                    (cdr infos)
                                    (+ y this-space bar-thickness))))])))]))))
      
      (define vertical-dragable% (vertical-dragable-mixin vertical-panel%)))))
