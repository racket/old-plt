
(module gui mzscheme
  
  (require
   (lib "tool.ss" "drscheme")
   (lib "unitsig.ss")
   (lib "mred.ss" "mred")
   (lib "class.ss")
   (lib "list.ss")
   (prefix frame: (lib "framework.ss" "framework"))
   (prefix strcst: (lib "string-constant.ss" "string-constants"))
   
   (prefix cst: "constants.ss")
   (prefix sba: "constraints-gen-and-prop.ss")
   (prefix err: "sba-errors.ss")
   (prefix saav: "snips-and-arrows-view.ss")
   )

  (provide tool@)
  
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      
      ; INTERFACE WITH LANGUAGES
      (define mrflow-language-extension-interface<%>
        (interface ()
          render-value-set
          get-mrflow-primitives-filename))
      
      (define (mrflow-default-implementation-mixin super%)
        (class* super% (mrflow-language-extension-interface<%>)
          
          ; type -> string
          ; Language implementors are responsible for providing a type pretty-printer.
          ; XXX NOT CURRENTLY USED
          (define/public (render-value-set val) "render-value-set-mixin not implemented")
          
          ; -> string
          ; Language implementors are responsible for providing the name of the file
          ; that contains the types of the primitives for their language. If they don't,
          ; we give a warning, use R5RS, and hope for the best.
          (define/public (get-mrflow-primitives-filename)
            (message-box (strcst:string-constant mrflow-using-default-language-title)
                         (strcst:string-constant mrflow-using-default-language)
                         #f '(ok))
            (build-path (collection-path "mrflow")
                        "primitives"
                        "r5rs.ss"))
          
          (super-instantiate ())))
      
      (define (phase1) 
        (drscheme:language:extend-language-interface
         mrflow-language-extension-interface<%>
         mrflow-default-implementation-mixin))
      
      (define (phase2) cst:void)
      
      
      ; GUI STYLES
      (define can-click-style-delta (make-object style-delta% 'change-weight 'bold))
      (send can-click-style-delta set-delta-foreground "purple")
      
      (define green-style-delta (make-object style-delta% 'change-weight 'bold))
      (send green-style-delta set-delta-foreground "green")
      (send green-style-delta set-underlined-on #t)
      
      (define orange-style-delta (make-object style-delta% 'change-weight 'bold))
      (send orange-style-delta set-delta-foreground "orange")
      (send orange-style-delta set-underlined-on #t)
      
      (define red-style-delta (make-object style-delta% 'change-weight 'bold))
      (send red-style-delta set-delta-foreground "red")
      (send red-style-delta set-underlined-on #t)
      
      ; style before analysis
      (define original-style (send (frame:scheme:get-style-list) find-named-style "standard"))
      
      ; symbol style-delta% -> style-delta%
      ; compares two style-deltas (one represented as a color/severity name, the other one as
      ; an actual style-delta) and returns the most "urgent" one.
      ; red > orange > green
      (define (max-style-delta-by-name style-delta-name style-delta)
        (case style-delta-name
          [(red) red-style-delta]
          [(orange) (if (eq? style-delta red-style-delta) style-delta orange-style-delta)]
          [(green) style-delta]
          [else (error 'max-style-delta-by-name
                       "MrFlow internal error; unknown style-delta ~a"
                       style-delta-name)]))
      
      ; box styles
      (define error-box-style-delta (make-object style-delta%))
      (send error-box-style-delta set-delta-foreground "red")
      
      (define type-box-style-delta (make-object style-delta%))
      (send type-box-style-delta set-delta-foreground "blue")
      
      ; arrow styles
      (define tacked-arrow-brush (send the-brush-list find-or-create-brush "BLUE" 'solid))
      (define untacked-arrow-brush (send the-brush-list find-or-create-brush "WHITE" 'solid))
      (define arrow-pen (send the-pen-list find-or-create-pen "BLUE" 1 'solid))
      
      (define mrflow-bitmap
        (drscheme:unit:make-bitmap
         (strcst:string-constant mrflow-button-title)
         (build-path (collection-path "icons") "mrflow-small.bmp")))
      
      
      ; INTERFACE FOR COLORING TEXT AND SNIPS
      ; label -> exact-non-negative-integer
      ; span conversation: for all graphical purposes, the span of a compound expression is 1,
      ; to highlight only the opening parenthesis. Otherwise we might highlight subexpressions
      ; with the wrong color.
      (define (get-span-from-label label)
        (if (or (sba:is-label-atom? label)
                (not (null? (sba:get-errors-from-label label))))
            (sba:get-span-from-label label)
            1))
      
      ; label -> style-delta%
      ; If the label has errors associated with it, we color the term with the color
      ; of the worst error, otherwise we color it with the normal clickable style-delta.
      (define (get-style-delta-from-label label)
        (let ([errors (sba:get-errors-from-label label)])
          (if (null? errors)
              can-click-style-delta
              (foldl (lambda (sba-error current-max-style-delta)
                       (max-style-delta-by-name (err:sba-error-gravity sba-error) current-max-style-delta))
                     green-style-delta
                     errors))))
      
      ; symbol -> style-delta%
      (define (get-box-style-delta-from-snip-type type)
        (case type
          [(type) type-box-style-delta]
          [(error) error-box-style-delta]
          [else (error 'get-box-style-delta-from-snip-type
                       "MrFlow internal error; unknown snip type: ~a"
                       type)]))
      
      ; (listof symbol)
      ; this describes the order in which snips of different types will appear after
      ; insertion in the editor, from left to right, for a given label.
      (define type-list '(type error))
      
      
      ; INTERFACE FOR MENUS
      ; symbol symbol -> string
      ; given a snip type and a menu action for snips (show/hide), return the corresponding
      ; menu text
      (define (get-menu-text-from-type type action)
        (case type
          [(type)
           (case action
             [(show) (strcst:string-constant mrflow-popup-menu-show-type)]
             [(hide) (strcst:string-constant mrflow-popup-menu-hide-type)]
             [else (error 'get-menu-text-from-type "MrFlow internal error; unknown type action: ~a" action)])]
          [(error)
           (case action
             [(show) (strcst:string-constant mrflow-popup-menu-show-errors)]
             [(hide) (strcst:string-constant mrflow-popup-menu-hide-errors)]
             [else (error 'get-menu-text-from-type "MrFlow internal error; unknown error action: ~a" action)])]
          [else (error 'get-menu-text-from-type "MrFlow internal error; unknown type: ~a" type)]))
      
      ; gui-state menu% label symbol (-> (listof string)) -> menu-item%
      ; create menu entries for snips
      (define (create-snips-menu-item-by-type snips-and-arrows-gui-state menu label type get-snip-strings)
        (if (saav:label-has-snips-of-this-type? snips-and-arrows-gui-state label type)
            ; delete menu entry
            (make-object menu-item%
              (get-menu-text-from-type type 'hide)
              menu
              (lambda (item event)
                (saav:remove-inserted-snips snips-and-arrows-gui-state label type)))
            ; show menu entry
            (let ([snip-strings (get-snip-strings)])
              (unless (null? snip-strings)
                (make-object menu-item%
                  (get-menu-text-from-type type 'show)
                  menu
                  (lambda (item event)
                    (saav:add-snips snips-and-arrows-gui-state label type snip-strings)))))))
      
      ; text% gui-state menu% label -> menu-item%
      ; create menu entries for arrows
      (define (create-arrow-menu-items this snips-and-arrows-gui-state menu label)
        (let* ([parents (sba:parents label)]
               [parents-max-arrows (length parents)]
               [parents-tacked-arrows (saav:get-parents-tacked-arrows snips-and-arrows-gui-state label)]
               [children (sba:children label)]
               [children-max-arrows (length children)]
               [children-tacked-arrows (saav:get-children-tacked-arrows snips-and-arrows-gui-state label)]
               [max-arrows (+ parents-max-arrows children-max-arrows)]
               [tacked-arrows (+ parents-tacked-arrows children-tacked-arrows)])
          (when (< tacked-arrows max-arrows)
            (make-object menu-item%
              (strcst:string-constant mrflow-popup-menu-tack-all-arrows)
              menu
              (lambda (item event)
                ; remove all (possibly untacked) arrows and add all arrows, tacked.
                ; we could just add the untacked ones, but what we do here is simple
                ; and efficient enough
                (saav:remove-arrows snips-and-arrows-gui-state label 'all #t)
                (for-each (lambda (parent-label)
                            (saav:add-arrow snips-and-arrows-gui-state parent-label label #t))
                          parents)
                (for-each (lambda (child-label)
                            (saav:add-arrow snips-and-arrows-gui-state label child-label #t))
                          children)
                (send this invalidate-bitmap-cache))))
          (when (> tacked-arrows 0)
            (make-object menu-item%
              (strcst:string-constant mrflow-popup-menu-untack-all-arrows)
              menu
              (lambda (item event)
                (saav:remove-arrows snips-and-arrows-gui-state label 'all #t)
                (send this invalidate-bitmap-cache))))))
      
      
      ; DEFINITION WINDOW MIXIN
      (drscheme:get/extend:extend-definitions-text
       (lambda (super%)
         (class super%
           
           ; STATE
           (define snips-and-arrows-gui-state
             (saav:make-gui-state
              this
              sba:get-label-from-mzscheme-position
              sba:get-mzscheme-position-from-label
              get-span-from-label
              get-style-delta-from-label
              get-box-style-delta-from-snip-type
              type-list
              ))
           ; (union #f label)
           (define previous-label #f)
           
           
           ; PUBLIC
           ; -> void
           (define/public (color-text)
             (saav:color-text snips-and-arrows-gui-state))
           
           ; -> void
           ; remove all snips and arrows, and reset text style
           (define/public (clear-text)
             (saav:clear-all-snips-and-arrows snips-and-arrows-gui-state)
             ; don't do this if you want to keep the colors after analysis
             (send this begin-edit-sequence #f)
             (send this change-style original-style 0 (send this last-position))
             (send this end-edit-sequence))
           
           
           ; OVERRIDE
           (rename [super-after-insert after-insert])
           ; exact-non-negative-integer exact-non-negative-integer -> void
           (define/override (after-insert start len)
             (super-after-insert start len)
             (saav:after-insert snips-and-arrows-gui-state start len))
           
           (rename [super-after-delete after-delete])
           ; exact-non-negative-integer exact-non-negative-integer -> void
           (define/override (after-delete start len)
             (super-after-delete start len)
             (saav:after-delete snips-and-arrows-gui-state start len))
           
           (inherit dc-location-to-editor-location find-position)
           ; mouse-event% -> exact-non-negative-integer
           (define (get-drscheme-pos event)
             (let*-values ([(event-x) (send event get-x)]
                           [(event-y) (send event get-y)]
                           [(x y) (dc-location-to-editor-location
                                   event-x event-y)])
               (find-position x y)))
           
           (rename [super-on-paint on-paint])
           ; boolean dc% real real real real real real symbol -> void
           (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
             (super-on-paint before? dc left top right bottom dx dy draw-caret)
             (saav:on-paint snips-and-arrows-gui-state dc dx dy arrow-pen tacked-arrow-brush untacked-arrow-brush))
           
           (inherit get-canvas)
           (rename [super-on-local-event on-local-event])
           ; mouse-event% -> void
           (define/override (on-local-event event)
             (cond
               [(not (saav:text-modified? snips-and-arrows-gui-state)) (super-on-local-event event)]
               [(and (send event button-down? 'right)
                     (saav:get-related-label-from-drscheme-pos snips-and-arrows-gui-state (get-drscheme-pos event)))
                =>
                (lambda (label)
                  (let ([menu (make-object popup-menu%)])
                    ; SNIPS
                    (create-snips-menu-item-by-type
                     snips-and-arrows-gui-state
                     menu label 'type
                     (lambda ()
                       (list (sba:pp-type (sba:get-type-from-label label) 'gui))))
                    (create-snips-menu-item-by-type
                     snips-and-arrows-gui-state
                     menu label 'error
                     (lambda ()
                       (map err:sba-error-message (sba:get-errors-from-label label))))
                    ; ARROWS
                    (create-arrow-menu-items this snips-and-arrows-gui-state menu label)
                    
                    (send (get-canvas) popup-menu menu
                          (add1 (inexact->exact (floor (send event get-x))))
                          (add1 (inexact->exact (floor (send event get-y)))))
                    ))]
               [(send event leaving?)
                (when previous-label
                  (saav:remove-arrows snips-and-arrows-gui-state previous-label #f #f)
                  (set! previous-label #f)
                  (send this invalidate-bitmap-cache))]
               [(or (send event moving?)
                    (send event entering?))
                (let ([label (saav:get-related-label-from-drscheme-pos snips-and-arrows-gui-state (get-drscheme-pos event))])
                  (if previous-label
                      (if (eq? label previous-label)
                          ; nothing to do, still pointing at same stuff
                          cst:void
                          (if label
                              ; pointing at something new, remove old untacked arrows, add new ones
                              (let ([parents (sba:parents label)]                                
                                    [children (sba:children label)])
                                (saav:remove-arrows snips-and-arrows-gui-state previous-label #f #f)
                                (set! previous-label label)
                                (for-each (lambda (parent-label)
                                            (saav:add-arrow snips-and-arrows-gui-state parent-label label #f))
                                          parents)
                                (for-each (lambda (child-label)
                                            (saav:add-arrow snips-and-arrows-gui-state label child-label #f))
                                          children)
                                (send this invalidate-bitmap-cache))
                              ; not pointing at anything new, just remove old arrows
                              (begin
                                (saav:remove-arrows snips-and-arrows-gui-state previous-label #f #f)
                                (set! previous-label #f)
                                (send this invalidate-bitmap-cache))))
                      (if label
                          ; pointing at something, coming from nowhere, add arrows
                          (let ([parents (sba:parents label)]                                
                                [children (sba:children label)])
                            (set! previous-label label)
                            (for-each (lambda (parent-label)
                                        (saav:add-arrow snips-and-arrows-gui-state parent-label label #f))
                                      parents)
                            (for-each (lambda (child-label)
                                        (saav:add-arrow snips-and-arrows-gui-state label child-label #f))
                                      children)
                            (send this invalidate-bitmap-cache))
                          ; pointing at nothing, coming from nowhere, do nothing
                          cst:void)))]
               [else (super-on-local-event event)]))
           
           (super-instantiate ())
           ) ; class
         )) ; drscheme:get/extend:extend-definitions-text
      
      
      ; UNIT FRAME MIXIN
      (drscheme:get/extend:extend-unit-frame
       (lambda (super%)
         (class super%
           
           (inherit get-definitions-text)
           (rename [super-clear-annotations clear-annotations])
           ; -> void
           (define/override (clear-annotations)
             (super-clear-annotations)
             (send (get-definitions-text) clear-text))
           
           (rename [super-enable-evaluation enable-evaluation])
           ; -> void
           (define/override (enable-evaluation)
             (super-enable-evaluation)
             (send analyze-button enable #t))
           
           (rename [super-disable-evaluation disable-evaluation])
           ; -> void
           (define/override (disable-evaluation)
             (super-disable-evaluation)
             (send analyze-button enable #f))
          
           (super-instantiate ())
           
           (inherit get-button-panel get-interactions-text)
           (define analyze-button
             (instantiate
                 button% ()
               (label (mrflow-bitmap this))
               (parent (get-button-panel))
               (callback
                (lambda (button event)
                  (let ([start-time (current-milliseconds)]
                        [definitions-text (get-definitions-text)]
                        [interactions-text (get-interactions-text)]
                        [language-settings
                         (frame:preferences:get
                          (drscheme:language-configuration:get-settings-preferences-symbol))])
                    (clear-annotations)
                    (sba:reset-all)
                    
                    ; note: we have to do this each time, because the user might have changed
                    ; the language between analyses.
                    (let* ([language-object (drscheme:language-configuration:language-settings-language
                                             language-settings)]
                           [primitive-table-file (send language-object get-mrflow-primitives-filename)])
                      (if (file-exists? primitive-table-file)
                          (begin
                            (sba:initialize-primitive-type-schemes primitive-table-file)
                            (drscheme:eval:expand-program
                             (drscheme:language:make-text/pos definitions-text
                                                              0
                                                              (send definitions-text last-position))
                             language-settings
                             #t
                             void
                             void
                             (lambda (syntax-object-or-eof iter)
                               (if (eof-object? syntax-object-or-eof)
                                   (begin
                                     (let ([sba-end-time (current-milliseconds)])
                                       ;(printf "sba time: ~a ms~n" (- (current-milliseconds) start-time))
                                       (sba:check-primitive-types)
                                       ;(printf "check time: ~a ms~n" (- (current-milliseconds) sba-end-time))
                                       )
                                     (send definitions-text begin-edit-sequence #f)
                                     (send definitions-text color-text)
                                     (send definitions-text end-edit-sequence))
                                   (begin
                                     ;(printf "~a~n" (syntax-object->datum syntax-object-or-eof))
                                     (sba:create-label-from-term syntax-object-or-eof '() #f)
                                     (iter))))))
                          ; get-mrflow-primitives-filename defaults to R5RS
                          ; (see mrflow-default-implementation-mixin above), so if we arrive here,
                          ; we know we are in trouble, because it means no primitive table is
                          ; defined for the current language and we couldn't even find the table
                          ; for the R5RS primitives.
                          (error 'analyze-button-callback
                                 "MrFlow internal error; R5RS primitive types file ~a doesn't exist."
                                 primitive-table-file))))))))
           
           (send (get-button-panel) change-children
                 (lambda (button-list)
                   (cons analyze-button (remq analyze-button button-list))))
           ) ; class
         )) ; drscheme:get/extend:extend-unit-frame
      
      )) ; tool@ unit/sig
  ); module
