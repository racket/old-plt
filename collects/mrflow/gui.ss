
(module
 gui mzscheme
 
 (require
  (lib "tool.ss" "drscheme")
  (lib "unitsig.ss")
  (lib "class.ss")
  (lib "mred.ss" "mred")
  (prefix frame: (lib "framework.ss" "framework"))
  (lib "arrow.ss" "drscheme")
  (lib "list.ss")
  (prefix mrflow: (lib "constraints-gen-and-prop.ss" "mrflow"))
  )
 
 (provide tool@)
 
 (define tool@
   (begin
   (unit/sig ()
     (import drscheme:tool^)
     ; used for clickable locations in the program
     (define can-click-style (make-object style-delta% 'change-weight 'bold))
     (send can-click-style set-delta-foreground "purple")
     
     ; used for 'red primitives
     (define red-style (make-object style-delta% 'change-weight 'bold))
     (send red-style set-delta-foreground "red")
     (send red-style set-underlined-on #t)
     
     ; used for 'green primitives
     (define green-style (make-object style-delta% 'change-weight 'bold))
     (send green-style set-delta-foreground "forest green")
     
     ; normal style must cancel out can-click-style, red-style, green-style, and underline
     (define normal-style (make-object style-delta% 'change-weight 'normal))
     (send normal-style set-delta-foreground "black")
     (send normal-style set-underlined-off #t)
     
     ; used for the inserted type boxes
     (define box-style (make-object style-delta%))
     (send box-style set-delta-foreground "purple")
     
     (define flow-bitmap
       (drscheme:unit:make-bitmap
        "Analyze"
        (build-path (collection-path "icons") "mrflow-small.bmp")))
     
     (drscheme:get/extend:extend-definitions-text
      (lambda (super%)
        (class*/names
         (this super-initialize super-init) super% ()
         ;(init args1)
         (init-field
          [analyzed? #f]
          [get-prims (void)]
          [get-loc (void)]
          [get-label (void)]
          [get-type (void)]
          [pp-type (void)]
          [parents (void)]
          [children (void)]
          [has-member? (void)]
          ;[show-expanded (void)]
          )
         
         (inherit last-position begin-edit-sequence end-edit-sequence change-style)
         (public*
          [run-analysis
           (lambda ()
             (set! get-prims mrflow:get-prims)
             (set! get-loc mrflow:get-loc)
             (set! get-label mrflow:get-label)
             (set! get-type mrflow:get-type)
             (set! pp-type mrflow:pp-type)
             (set! parents mrflow:parents)
             (set! children mrflow:children)
             (set! has-member? mrflow:has-member?)
             ;(set! show-expanded mrflow:show-expanded)
             
             (begin-edit-sequence #f)
             
             ; color clickable positions
             (let loop ([i (last-position)])
               (unless (zero? i)
                 (when (get-label i)
                   (change-style can-click-style (- i 1) i))
                 (loop (- i 1))))
             
             ; color primitives
             ; this will overwrite the highlighting for
             ; the clickable spots in the primitives because
             ; each primitive should be clickable (only each
             ; first character of the primitives is clickable
             ; now)
             (for-each
              (lambda (prim)
                (let ([start (car prim)]
                      [end (+ (cadr prim) 1)]
                      [source (caddr prim)]
                      [color (cadddr prim)])
                  ; only color primitives in this 
                  (when (eq? this source)
                    (case color
                      [(red) (change-style red-style start end)]
                      [(green) (change-style green-style start end)]
                      [else (void)]))))
              (get-prims))

             (end-edit-sequence)
             
             (set! analyzed? #t)
             )]
          [remove-analysis
           (lambda ()
             (set! analyzed? #f)
             (begin-edit-sequence #f)
             (clear-arrows)
             (clear-inserted-snips)
             (change-style normal-style 0 (last-position))
             (end-edit-sequence))]
          )
         
         (inherit delete)
         (init-field
          [inserted-snip-poss null]
          [arrows null]
          [var-pos->edit-pos
           (lambda (pos)
             (let loop ([poss inserted-snip-poss]
                        [pos pos])
               (cond
                 [(null? poss) pos]
                 [(<= (car poss) pos) (loop (cdr poss) (+ pos 1))]
                 [else (loop (cdr poss) pos)])))]
          [edit-pos->var-pos
           (lambda (pos)
             (let loop ([poss inserted-snip-poss]
                        [pos pos])
               (cond
                 [(null? poss) pos]
                 [(<= (car poss) pos) (loop (cdr poss) (- pos 1))]
                 [else (loop (cdr poss) pos)])))]
          [move-poss
           (lambda (start len add)
             (let ([update-pos
                    (lambda (x)
                      (if (< x start)
                          x
                          (add x len)))])
               (set! inserted-snip-poss (map update-pos inserted-snip-poss))
               (set! arrows (map (lambda (x)
                                   (list (update-pos (car x))
                                         (update-pos (cadr x))
                                         (caddr x)))
                                 arrows))))]
          [add-inserted-snip
           (lambda (pos)
             (move-poss pos 1 +)
             (set! inserted-snip-poss (cons pos inserted-snip-poss)))]
          [clear-arrows
           (lambda ()
             (set! arrows null)
             (invalidate-bitmap-cache))]
          [clear-inserted-snips
           (lambda ()
             (for-each
              (lambda (pos)
                (delete pos (+ pos 1) #f))
              (quicksort inserted-snip-poss >=))
             (set! inserted-snip-poss null))])
         (inherit get-style-list is-locked?)
         (init-field
          [check-remove-analysis
           (lambda ()
             (eq? (message-box
                   "Clear Analysis?"
                   "Do you want to invalidate the analysis and remove any boxes and arrows?"
                   (send (get-canvas) get-top-level-window)
                   '(yes-no))
                  'yes))]
          [analysis-modifing? #f]
          )
         (rename [super-can-insert? can-insert?]
                 [super-after-insert after-insert]
                 [super-can-delete? can-delete?]
                 [super-after-delete after-delete])
         (override*
          [can-insert?
           (lambda (start len)
             (and (or (not analyzed?)
                      analysis-modifing?
                      (check-remove-analysis))
                  (super-can-insert? start len)))]
          [after-insert
           (lambda (start len)
             (when (and analyzed? (not analysis-modifing?))
               (move-poss start len +)
               (remove-analysis))
             (super-after-insert start len))]
          [can-delete?
           (lambda (start len)
             (and (or (not analyzed?)
                      analysis-modifing?
                      (check-remove-analysis))
                  (super-can-delete? start len)))]
          [after-delete
           (lambda (start len)
             (when (and analyzed? (not analysis-modifing?))
               (move-poss start len -)
               (remove-analysis))
             (super-after-delete start len))])
         
         (inherit dc-location-to-editor-location find-position)
         (init-field
          [get-pos
           (lambda (event)
             (let*-values ([(event-x event-y)
                            (values (send event get-x)
                                    (send event get-y))]
                           [(x y) (dc-location-to-editor-location
                                   event-x event-y)])
               (find-position x y)))])
         (rename [super-on-local-event on-local-event]
                 [super-on-paint on-paint])
         (inherit insert get-canvas invalidate-bitmap-cache position-location)
         (override*
          [on-paint
           (lambda (before? dc left top right bottom dx dy draw-caret)
             (super-on-paint before? dc left top right bottom dx dy draw-caret)
             (unless (null? arrows)
               (let ([pen (send dc get-pen)]
                     [brush (send dc get-brush)])
                 (send dc set-pen (send the-pen-list find-or-create-pen "forest green" 1 'solid))
                 (send dc set-brush (send the-brush-list find-or-create-brush "purple" 'solid))
                 (for-each (lambda (arrow)
                             (let ([start (car arrow)]
                                   [end (cadr arrow)]
                                   [start-x-left (box 0)]
                                   [start-y-top (box 0)]
                                   [end-x-left (box 0)]
                                   [end-y-top (box 0)]
                                   [start-x-right (box 0)]
                                   [start-y-bot (box 0)]
                                   [end-x-right (box 0)]
                                   [end-y-bot (box 0)]
                                   [avg (lambda (x y)
                                          (/ (+ (unbox x) (unbox y)) 2))])
                               ;(if (eq? (caddr arrow) 'child)
                               ;  (begin
                               ;    (position-location start start-x-left start-y-top #t)
                               ;    (position-location (add1 start) start-x-right start-y-bot #f)
                               ;    (position-location (sub1 end) end-x-left end-y-top #t)
                               ;    (position-location end end-x-right end-y-bot #f))
                               ;  (begin
                               ;    (position-location (sub1 start) start-x-left start-y-top #t)
                               ;    (position-location start start-x-right start-y-bot #f)
                               ;    (position-location end end-x-left end-y-top #t)
                               ;    (position-location (add1 end) end-x-right end-y-bot #f)))
                               (position-location start start-x-left start-y-top #t)
                               (position-location start start-x-right start-y-bot #f)
                               (position-location end end-x-left end-y-top #t)
                               (position-location end end-x-right end-y-bot #f)
                               (draw-arrow dc
                                           (avg start-x-left start-x-right)
                                           (avg start-y-top start-y-bot)
                                           (avg end-x-left end-x-right)
                                           (avg end-y-top end-y-bot)
                                           dx dy)))
                           arrows)
                 (send dc set-pen pen)
                 (send dc set-brush brush))))]
          [on-local-event
           (lambda (event)
             (cond
               [(not analyzed?)
                (super-on-local-event event)]
               [(and (send event button-down? 'right)
                     ; mzscheme numbers starting from 1, DrScheme numbers starting from 0
                     (get-label (add1 (edit-pos->var-pos (get-pos event)))))
                =>
                (lambda (label)
                  (let ([menu (make-object popup-menu%)]
                        [pos (get-pos event)])
                    (make-object menu-item%
                      "Show value set"
                      menu
                      (lambda (item evt)
                        (let ([t (make-object text%)])
                          (send t insert (pp-type (get-type label) 'gui))
                          (set! analysis-modifing? #t)
                          (begin-edit-sequence #f) ; so it is not undoable...
                          (insert (make-object editor-snip% t) pos pos)
                          (change-style box-style pos (+ pos 1))
                          (add-inserted-snip pos)
                          (invalidate-bitmap-cache)
                          (end-edit-sequence)
                          (set! analysis-modifing? #f)
                          )))
                    
;                    (make-object menu-item%
;                      "show expanded"
;                      menu
;                      (lambda (item evt)
;                        (let ([t (make-object text%)])
;                          (send t insert (show-expanded label))
;                          (set! analysis-modifing? #t)
;                          (begin-edit-sequence #f) ; so it is not undoable...
;                          (insert (make-object editor-snip% t) pos pos)
;                          (change-style box-style pos (+ pos 1))
;                          (add-inserted-snip pos)
;                          (invalidate-bitmap-cache)
;                          (end-edit-sequence)
;                          (set! analysis-modifing? #f)
;                          )))
 
                    (make-object menu-item%
                      "show errors"
                      menu
                      (lambda (item evt)
                        (begin-edit-sequence #f) ; so it is not undoable...
                        (for-each (lambda (error-msg)
                                    (let ([t (make-object text%)])
                                      (send t insert error-msg)
                                      (set! analysis-modifing? #t)
                                      (insert (make-object editor-snip% t) pos pos)
                                      (change-style box-style pos (+ pos 1))
                                      (add-inserted-snip pos)
                                      (invalidate-bitmap-cache)
                                      (set! analysis-modifing? #f)
                                      ))
                                  (mrflow:get-errors label))
                        (end-edit-sequence)
                        ))
 
                    (let ([make-children/parents-item
                           (lambda (menu-label children/parents f)
                             (make-object menu-item%
                               menu-label
                               menu
                               (lambda (item evt)
                                 (for-each
                                  (lambda (var)
                                    (let ([loc (get-loc var)])
                                      (when loc
                                        (let ([parent/child (var-pos->edit-pos loc)])
                                          (set! arrows (cons (f pos parent/child) arrows))
                                          ))))
                                  (children/parents label))
                                 (invalidate-bitmap-cache))))])
                      (make-children/parents-item "Parents" parents (lambda (x y) (list y x 'parent)))
                      (make-children/parents-item "Children" children (lambda (x y) (list x y 'child))))
                    
                    (send (get-canvas) popup-menu menu
                          (+ 1 (inexact->exact (floor (send event get-x))))
                          (+ 1 (inexact->exact (floor (send event get-y)))))))]
               [else
                (super-on-local-event event)]))])
         
         ;(sequence
         ;(apply super-init args1)
         (super-init)
         ;)
         )))
     
     (drscheme:get/extend:extend-unit-frame
      (lambda (super%)
        (class*/names
         (this super-initialize super-init) super% ()
         ;(init args2)
         (inherit get-button-panel)
         ;(sequence
         ;(apply super-init args2)
         (super-init)
         ;)
         (rename [super-disable-evaluation disable-evaluation]
                 [super-enable-evaluation enable-evaluation])
         
         (rename [super-clear-annotations clear-annotations])
         (override*
          [clear-annotations
           (lambda ()
             (super-clear-annotations)
             (send (get-definitions-text) remove-analysis))])
         
         (inherit get-definitions-text get-interactions-text)
         (override*
          [enable-evaluation
           (lambda ()
             (send flow:analyze-button enable #t)
             (super-enable-evaluation))]
          [disable-evaluation
           (lambda ()
             (send flow:analyze-button enable #f)
             (super-disable-evaluation))])
         (init-field
          [flow:analyze-button
           (instantiate
            button% ()
            (label (flow-bitmap this))
            (parent (get-button-panel))
            (callback
             (lambda (button evt)
               (clear-annotations)
               (let ([start (current-milliseconds)])
                 (mrflow:reset-all)
                 (send (get-interactions-text)
                       expand-program
                       (drscheme:language:make-text/pos (get-definitions-text) 
                                                        0
                                                        (send (get-definitions-text)
                                                              last-position))
                       (frame:preferences:get
                        (drscheme:language-configuration:get-settings-preferences-symbol))
                       (lambda (exception? syntax-object/exception run-in-expansion-thread loop)
                         (if exception?
                             (let ([message (car syntax-object/exception)]
                                   [exn (cdr syntax-object/exception)])
                               (cond
                                 [(exn:read? exn)
                                  (let ([end (exn:read-position exn)])
                                    (send (get-definitions-text)
                                          change-style red-style (- end 1) end)
                                    (message-box "read exception"
                                                 (string-append "read exception: " message)
                                                 #f '(ok)))]
                                 [(exn:syntax? exn)
                                  (let ([syntax-object (exn:syntax-expr exn)])
                                    (when syntax-object
                                      (let ([start (- (syntax-position syntax-object) 1)])
                                        (send (get-definitions-text) change-style red-style
                                              start (+ start (syntax-span syntax-object)))
                                        (message-box "syntax exception"
                                                     (format "syntax exception: ~a"
                                                             message)
                                                     #f '(ok)))))]
                                 [else
                                  (message-box "unknown exception"
                                               (format "unknown exception: ~a" exn))]))
                           (unless (eof-object? syntax-object/exception)
                             (mrflow:create-label-from-term syntax-object/exception '() #f '())))
                         (loop)))
                 (mrflow:check-primitive-types)
                 ;(printf "time: ~a ms~n" (- (current-milliseconds) start))
                 )
               
               ; XXX perf analysis
               ;(printf "ast-nodes: ~a  graph-nodes: ~a  graph-edges: ~a~n"
               ;        mrflow:ast-nodes mrflow:graph-nodes mrflow:graph-edges)
               
               (send (get-definitions-text) run-analysis))))])
         ;(sequence
         ; XXX state is shared between instances of the tool, so we could do
         ; this call only once, outside the mixin...
         (mrflow:initialize-primitive-type-schemes)
         (send (get-button-panel) change-children
               (lambda (l)
                 (cons flow:analyze-button (remq flow:analyze-button l))))
         ;)
         )))))
 )
)