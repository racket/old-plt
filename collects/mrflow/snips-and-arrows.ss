
(module snips-and-arrows mzscheme
  
  (require
   (lib "etc.ss")
   (lib "class.ss")
   (lib "mred.ss" "mred")
   (prefix fw: (lib "framework.ss" "framework"))
   (prefix strcst: (lib "string-constant.ss" "string-constants"))
   
   (prefix saav: "snips-and-arrows-view.ss")
   "labels.ss"
   )
  
  (provide
   drscheme:unit:add-to-program-editor-mixin-mixin
   drscheme:get/extend:extend-definitions-text-mixin
   make-register-label-with-gui
   make-register-label-with-gui-for-syntax-objects
   )
  
  (define-struct gui-state (; gui-view-state
                            gui-view-state
                            ; (label -> (listof label))
                            get-parents-from-label
                            ; (label -> (listof label))
                            get-children-from-label
                            ; (label -> boolean)
                            label-resizable?
                            ; (label -> string)
                            get-name-from-label
                            ; ((listof label) -> (listof label))
                            get-labels-to-rename-from-labels
                            ; (symbol -> string)
                            get-menu-text-from-snip-type
                            ; symbol label -> (listof string)
                            get-snip-text-from-snip-type-and-label
                            ; (union #f (listof label))
                            previous-labels
                            ; boolean
                            ; we need this one to prevent arrows and menus to show up
                            ; before the real analysis part is over
                            term-analysis-done?
                            ))
  
  ; MENUS
  ; gui-state menu% (listof labels) symbol top -> menu-item%
  ; creates a menu entry for a given snip type
  (define (create-snips-menu-item-by-type gui-state menu labels type source)
    (let ([gui-view-state (gui-state-gui-view-state gui-state)]
          [get-menu-text-from-snip-type (gui-state-get-menu-text-from-snip-type gui-state)]
          [get-snip-text-from-snip-type-and-label
           (gui-state-get-snip-text-from-snip-type-and-label gui-state)])
      (if (ormap (lambda (label)
                   (saav:label-has-snips-of-this-type? gui-view-state label type))
                 labels)
          ; at least one label has snips displayed => delete menu entry
          (make-object menu-item%
            (get-menu-text-from-snip-type type 'hide)
            menu
            (lambda (item event)
              (for-each (lambda (label)
                          (when (saav:label-has-snips-of-this-type? gui-view-state label type)
                            (saav:remove-inserted-snips gui-view-state label type source)))
                        labels)))
          ; no label has snips displayed => show menu entry if one of them has snips associated
          ; with it
          (unless (andmap (lambda (label)
                            (null? (get-snip-text-from-snip-type-and-label type label)))
                          labels)
            (make-object menu-item%
              (get-menu-text-from-snip-type type 'show)
              menu
              (lambda (item event)
                (for-each (lambda (label)
                            (let ([snip-strings (get-snip-text-from-snip-type-and-label type label)])
                              (unless (null? snip-strings)
                                (saav:add-snips gui-view-state label type source snip-strings))))
                          labels)))))))
  
  ; gui-state menu% (listof label) -> menu-item%
  ; create menu entries for arrows
  (define (create-arrow-menu-items gui-state menu labels)
    (let* ([gui-view-state (gui-state-gui-view-state gui-state)]
           [get-parents-from-label (gui-state-get-parents-from-label gui-state)]
           [get-children-from-label (gui-state-get-children-from-label gui-state)]
           [parentss (map get-parents-from-label labels)]
           [parents-max-arrows (apply + (map length parentss))]
           [parents-tacked-arrows (apply + (map (lambda (label)
                                                  (saav:get-parents-tacked-arrows gui-view-state label))
                                                labels))]
           [childrens (map get-children-from-label labels)]
           [children-max-arrows (apply + (map length childrens))]
           [children-tacked-arrows (apply + (map (lambda (label)
                                                   (saav:get-children-tacked-arrows gui-view-state label))
                                                 labels))]
           [max-arrows (+ parents-max-arrows children-max-arrows)]
           [tacked-arrows (+ parents-tacked-arrows children-tacked-arrows)])
      (when (< tacked-arrows max-arrows)
        (make-object menu-item%
          (strcst:string-constant snips-and-arrows-popup-menu-tack-all-arrows)
          menu
          (lambda (item event)
            ; remove all (possibly untacked) arrows and add all arrows, tacked.
            ; we could just add the untacked ones, but what we do here is simple
            ; and efficient enough
            (for-each (lambda (label parents children)
                        (saav:remove-arrows gui-view-state label 'all #t)
                        (for-each (lambda (parent-label)
                                    (saav:add-arrow gui-view-state parent-label label #t))
                                  parents)
                        (for-each (lambda (child-label)
                                    (saav:add-arrow gui-view-state label child-label #t))
                                  children))
                      labels parentss childrens)
            (saav:invalidate-all-bitmap-caches gui-view-state))))
      (when (> tacked-arrows 0)
        (make-object menu-item%
          (strcst:string-constant snips-and-arrows-popup-menu-untack-all-arrows)
          menu
          (lambda (item event)
            (for-each (lambda (label)
                        (saav:remove-arrows gui-view-state label 'all #t))
                      labels)
            (saav:invalidate-all-bitmap-caches gui-view-state))))))
  
  
  ; gui-view-state -> boolean
  ; User deletions don't mesh well with tool-inserted snips, especially when the
  ; user tries to delete snips inserted by the tool.  We could check to see if the
  ; user deletes any of our snips, but then keeping track of that rapidely becomes
  ; a mess.  And then it wouln't interact well with the undo feature: undoing the
  ; user-initiated deletion of tool-inserted snips would make tool-inserted snips
  ; reappear even after the analysis was finished.
  ; Even insertions cause problems: a user might insert something while our snips
  ; are present, then remove all the snips, then undo the insertion: this would remove
  ; stuff at the position where the inserted something initially was, not at the
  ; position where the inserted something currently is, thereby changing "undo" into
  ; "delete the wrong stuff".
  ; Conclusion: we disallow user insertion and deletion while our snips are displayed
  ; (in that editor).
  (define (is-action-allowed? gui-view-state source)
    (or (saav:analysis-currently-modifying? gui-view-state)
        (if (saav:snips-currently-displayed-in-source? gui-view-state source)
            (begin
              (message-box (strcst:string-constant snips-and-arrows-user-action-disallowed-title)
                           (strcst:string-constant snips-and-arrows-user-action-disallowed)
                           #f '(ok))
              #f)
            #t)))
  
  
  ; MIXINS
  ; drscheme:unit:add-to-program-editor-mixin mixin
  (define drscheme:unit:add-to-program-editor-mixin-mixin
    (lambda (super%)
      (class super%

        ; State initialization and resetting
        ; The state is created by the call to make-register-label-with-gui in the callback
        ; of the tool's button.  The state is hidden inside the register-label-with-gui function
        ; returned by the call.  That means a new instance of the state is created each time
        ; the user uses the tool.  Then, each time the user uses register-label-with-gui,
        ; the function checks whether the source has been seen before or not, and if it hasn't
        ; it calls the source's initialize-snips-and-arrows-gui-state method to initialize the
        ; source's state.  That ensures that all sources where coloring has to happen share the
        ; same state.  Note that the editor for the definition window has both
        ; add-to-program-editor-mixin-mixin and extend-definitions-text-mixin applied to it,
        ; so the initialize-snips-and-arrows-gui-state method is define/public in one case and
        ; define/override in the other case.
        ;
        ; The state is reset in two cases:
        ; - the user inserts or deletes something in an editor (see the comment for
        ;   is-action-allowed? for details about when this is allowed), and
        ;   clear-colors-after-user-action? is true
        ; - the gui makes a direct call to remove-all-snips-and-arrows-and-colors (probably inside
        ;   the clear-annotations method for the unit frame)
        ; The state is reseted by calling the reset-snips-and-arrows-state method of each source
        ; for which a label has been registred.  Since the unit frame has no direct reference to
        ; the state but only through the register-label-with-gui function, and since the sources
        ; don't have any reference to the state after their reset-snips-and-arrows-state method
        ; is called, the state can be garbage collected as sson as the register-label-with-gui
        ; function is not referenced by the unit frame anymore.
        ; Note that it would be possible for the unit frame to re-use the state (and indeed that's
        ; how it was working for a while) but it makes testing whether the analysis is currently
        ; running a bit more difficult and doesn't make anything else any simpler.  Besides, it
        ; might also be a source of subtle errors if everything is not correctly reseted from one
        ; run of the analysis to the next one.
        
        ; gui-state
        (define gui-state 'uninitialized-gui-state-in-program-editor-mixin)
        
        ; gui-view-state
        (define gui-view-state 'uninitialized-gui-view-state-in-program-editor-mixin)
        
        ; gui-state -> void
        ; see the same method below for explanation
        (define/public (initialize-snips-and-arrows-gui-state new-state)
          (if (symbol? gui-state)
              (begin
                (set! gui-state new-state)
                (set! gui-view-state (gui-state-gui-view-state gui-state)))
              (error 'initialize-snips-and-arrows-gui-state "state already initialized in program editor")))
        
        ; -> void
        (define/public (reset-snips-and-arrows-state)
          (set! gui-state 'reinitialized-gui-state-in-program-editor-mixin)
          (set! gui-view-state 'reinitialized-gui-view-state-in-program-editor-mixin))
        
        (rename [super-can-insert? can-insert?])
        ; exact-non-negative-integer exact-non-negative-integer -> boolean
        (define/override (can-insert? start len)
          (and (or (symbol? gui-state)
                   (and (gui-state-term-analysis-done? gui-state)
                        (is-action-allowed? gui-view-state this)))
               (super-can-insert? start len)))
        
        (rename [super-can-delete? can-delete?])
        ; exact-non-negative-integer exact-non-negative-integer -> boolean
        (define/override (can-delete? start len)
          (and (or (symbol? gui-state)
                   (and (gui-state-term-analysis-done? gui-state)
                        (is-action-allowed? gui-view-state this)))
               (super-can-delete? start len)))
        
        (rename [super-can-save-file? can-save-file?])
        ; exact-non-negative-integer exact-non-negative-integer -> boolean
        (define/override (can-save-file? filename format)
          (if (symbol? gui-state)
              (super-can-save-file? filename format)
              (if (and (gui-state-term-analysis-done? gui-state)
                       (not (saav:analysis-currently-modifying? gui-view-state)))
                  (begin
                    (saav:after-user-action gui-view-state)
                    (super-can-save-file? filename format))
                  #f)))
        
        (rename [super-after-insert after-insert])
        ; exact-non-negative-integer exact-non-negative-integer -> void
        (define/override (after-insert start len)
          (super-after-insert start len)
          (unless (or (symbol? gui-state)
                      (saav:analysis-currently-modifying? gui-view-state))
            (saav:after-user-action gui-view-state)))
        
        (rename [super-after-delete after-delete])
        ; exact-non-negative-integer exact-non-negative-integer -> void
        (define/override (after-delete start len)
          (super-after-delete start len)
          (unless (or (symbol? gui-state)
                      (saav:analysis-currently-modifying? gui-view-state))
            (saav:after-user-action gui-view-state)))
        
        (super-instantiate ()))))
  
  ; drscheme:get/extend:extend-definitions-text mixin
  (define drscheme:get/extend:extend-definitions-text-mixin
    (lambda (super%)
      (class super%
        ; gui-state
        (define gui-state 'uninitialized-gui-state-in-definitions-text-mixin)
        
        ; gui-view-state
        (define gui-view-state 'uninitialized-gui-view-state-in-definitions-text-mixin)
        
        ; gui-state -> void
        ; make-register-label-with-gui creates register-label-with-gui, which will call
        ; saav:register-label-with-gui, which will in turn find the source for the label
        ; and call this method (if necessary) to initialize the editor's state, thereby
        ; allowing all the editors for a single analysis to share the same state (see
        ; the same method above too).
        (rename [super-initialize-snips-and-arrows-gui-state initialize-snips-and-arrows-gui-state])
        (define/override (initialize-snips-and-arrows-gui-state new-state)
          (super-initialize-snips-and-arrows-gui-state new-state)
          (if (symbol? gui-state)
              (begin
                (set! gui-state new-state)
                (set! gui-view-state (gui-state-gui-view-state gui-state)))
              (error 'initialize-snips-and-arrows-gui-state "state already initialized in definitions text")))
        
        ; -> void
        (rename [super-reset-snips-and-arrows-state reset-snips-and-arrows-state])
        (define/override (reset-snips-and-arrows-state)
          (super-reset-snips-and-arrows-state)
          (set! gui-state 'reinitialized-gui-state-in-program-editor-mixin)
          (set! gui-view-state 'reinitialized-gui-view-state-in-program-editor-mixin))
        
        ; -> void
        ; colors all registered labels
        ; The analysis is only officially done after we've colored everything, otherwise user
        ; insertions might occur before we have time to finish coloring and we will color the
        ; wrong stuff...
        (define/public (color-all-labels)
          (unless (symbol? gui-view-state)
            (saav:color-all-labels gui-view-state)
            (set-gui-state-term-analysis-done?! gui-state #t)))
        
        ; -> void
        ; remove all snips and arrows, and resets text style in all editors
        (define/public (remove-all-snips-and-arrows-and-colors)
          (unless (symbol? gui-view-state)
            (saav:remove-all-snips-and-arrows-and-colors gui-view-state)))
        
        (rename [super-on-paint on-paint])
        ; boolean dc% real real real real real real symbol -> void
        (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
          (super-on-paint before? dc left top right bottom dx dy draw-caret)
          (when (and (not (symbol? gui-state))
                     (gui-state-term-analysis-done? gui-state)
                     (not before?))
            (saav:redraw-arrows gui-view-state this dc dx dy)))
        
        (inherit find-position dc-location-to-editor-location)
        ; mouse-event% text% -> (values (union #f exact-non-negative-integer) (union #f text%))
        ; finds the editor in which a mouse-event% has occured, going down recursively
        ; if there are embedded editors
        (define (get-drscheme-pos-and-editor event editor)
          (let ([dc-x (send event get-x)]
                [dc-y (send event get-y)]
                [on-it? (box #f)])
            (let loop ([editor editor])
              (let-values ([(ed-x ed-y) (send editor dc-location-to-editor-location dc-x dc-y)])
                (let ([pos (send editor find-position ed-x ed-y #f on-it?)])
                  (if (not (unbox on-it?))
                      (values #f #f)
                      (let ([snip (send editor find-snip pos 'after-or-none)])
                        (if (and snip (is-a? snip editor-snip%))
                            (loop (send snip get-editor))
                            (values pos editor)))))))))
        
        (inherit get-admin)
        (rename [super-on-event on-event])
        ; mouse-event% -> void
        (define/override (on-event event)
          (cond
            [(or (symbol? gui-state)
                 (not (gui-state-term-analysis-done? gui-state)))
             (super-on-event event)]
            [(and (send event button-down? 'right)
                  (let-values ([(pos editor) (get-drscheme-pos-and-editor event this)])
                    (if pos
                        (let ([labels (saav:get-related-label-from-drscheme-pos-and-source
                                       gui-view-state pos editor)])
                          (if (null? labels)
                              #f
                              (cons labels editor))) ; no =>-values so use cons...
                        #f)))
             =>
             (lambda (labels&editor)
               (let ([menu (make-object popup-menu%)]
                     [labels (car labels&editor)]
                     [editor (cdr labels&editor)])
                 ; SNIPS
                 (let ([create-snips-menu-item
                        (lambda (snip-type)
                          (create-snips-menu-item-by-type gui-state menu labels snip-type editor))])
                   (saav:for-each-snip-type gui-view-state create-snips-menu-item))
                 ; ARROWS
                 (create-arrow-menu-items gui-state menu labels)
                 ; RESIZE
                 (let ([label (car labels)])
                   (when ((gui-state-label-resizable? gui-state) label)
                     (let* ([old-name ((gui-state-get-name-from-label gui-state) label)]
                            [new-name-callback
                             (lambda (item event)
                               (let ([new-name
                                      (fw:keymap:call/text-keymap-initializer
                                       (lambda ()
                                         (get-text-from-user
                                          (strcst:string-constant cs-rename-id)
                                          (format (strcst:string-constant cs-rename-var-to) old-name)
                                          #f
                                          old-name)))])
                                 (for-each (lambda (label)
                                             ; the label might not be in the same editor, so passing
                                             ; editor as an argument to user-resize-label is useless
                                             (saav:user-resize-label gui-view-state label new-name))
                                           ((gui-state-get-labels-to-rename-from-labels gui-state) labels))))])
                       (make-object menu-item%
                         (strcst:string-constant cs-rename-id)
                         menu
                         new-name-callback))))
                 ; HIDE ALL SNIPS
                 (when (saav:snips-currently-displayed-in-source? gui-view-state editor)
                   (make-object menu-item%
                     (strcst:string-constant snips-and-arrows-hide-all-snips-in-editor)
                     menu
                     (lambda (item event)
                       (saav:remove-all-snips-in-source gui-view-state editor))))
                 
                 (let-values ([(x y) (dc-location-to-editor-location (send event get-x) (send event get-y))])
                   (send (get-admin) popup-menu menu x y))
                 ))]
            [(send event leaving?)
             (let ([previous-labels (gui-state-previous-labels gui-state)])
               (when previous-labels
                 (for-each (lambda (previous-label)
                             (saav:remove-arrows gui-view-state previous-label #f #f))
                           previous-labels)
                 (set-gui-state-previous-labels! gui-state #f)
                 (saav:invalidate-all-bitmap-caches gui-view-state)))]
            [(or (send event moving?)
                 (send event entering?))
             (let*-values ([(pos editor) (get-drscheme-pos-and-editor event this)]
                           [(labels)
                            (if pos
                                (saav:get-related-label-from-drscheme-pos-and-source
                                 gui-view-state pos editor)
                                #f)]
                           [(previous-labels) (gui-state-previous-labels gui-state)]
                           [(not-same-labels) (not (eq? labels previous-labels))])
               (when (and previous-labels not-same-labels)
                 (for-each (lambda (previous-label)
                             (saav:remove-arrows gui-view-state previous-label #f #f))
                           previous-labels))
               (when (and labels not-same-labels)
                 (let ([get-parents-from-label (gui-state-get-parents-from-label gui-state)]
                       [get-children-from-label (gui-state-get-children-from-label gui-state)])
                   (for-each (lambda (label)
                               (for-each (lambda (parent-label)
                                           (saav:add-arrow gui-view-state parent-label label #f))
                                         (get-parents-from-label label))
                               (for-each (lambda (child-label)
                                           (saav:add-arrow gui-view-state label child-label #f))
                                         (get-children-from-label label)))
                             labels)))
               (when not-same-labels
                 (set-gui-state-previous-labels! gui-state labels)
                 (saav:invalidate-all-bitmap-caches gui-view-state)))]
            [else (super-on-event event)]))
        
        (super-instantiate ()))))
  
  
  ; ... see below ... -> (label -> void)
  ; Ouch...  The returned function can be used to register labels with this gui
  (define (make-register-label-with-gui
           ; (label -> top)
           get-source-from-label
           ; (label -> non-negative-exact-integer)           
           get-mzscheme-position-from-label
           ; (label -> non-negative-exact-integer)
           get-span-from-label
           ; (label -> (listof label))
           get-parents-from-label
           ; (label -> (listof label))
           get-children-from-label
           ; (label -> boolean)
           label-resizable?
           ; (label -> string)
           get-name-from-label
           ; ((listof label) -> (listof label))
           get-labels-to-rename-from-labels
           ; (label -> style-delta%)
           get-style-delta-from-label
           ; (symbol -> style-delta%)
           get-box-style-delta-from-snip-type
           ; (symbol -> string)
           get-menu-text-from-snip-type
           ; (symbol label -> (listof string))
           get-snip-text-from-snip-type-and-label
           ; (listof symbol)
           snip-type-list
           ; boolean
           clear-colors-after-user-action?
           ; brush%
           tacked-arrow-brush
           ; brush%
           untacked-arrow-brush
           ; pen%
           arrow-pen)
    (let* ([gui-view-state (saav:make-gui-view-state
                            get-source-from-label
                            get-mzscheme-position-from-label
                            get-span-from-label
                            get-style-delta-from-label
                            get-box-style-delta-from-snip-type
                            snip-type-list
                            clear-colors-after-user-action?
                            tacked-arrow-brush
                            untacked-arrow-brush
                            arrow-pen)]
           [gui-state (make-gui-state
                       gui-view-state
                       get-parents-from-label
                       get-children-from-label
                       label-resizable?
                       get-name-from-label
                       get-labels-to-rename-from-labels
                       get-menu-text-from-snip-type
                       get-snip-text-from-snip-type-and-label
                       #f
                       #f)])
      (lambda (label)
        (saav:register-label-with-gui gui-view-state label gui-state))))
  
  ; SIMPLIFIED INTERFACE
  ; symbol -> void
  ; default function for snip handling
  (define error-no-snips
    (case-lambda
      [(_) (error-no-snips 'dummy 'dummy)]
      [(_1 _2) (error 'snips-and-arrows "no snip info was provided when mixins were created")]))
  
  ; ... see below ... -> (values gui-state (label -> void))
  ; simplified version of make-snips-and-arrows-state, specialized for syntax objects,
  ; and with default handling of snips
  (define make-register-label-with-gui-for-syntax-objects
    (opt-lambda (; (syntax-object -> (listof syntax-object))
                 get-parents-from-syntax-object
                 ; (syntax-object -> (listof syntax-object))
                 get-children-from-syntax-object
                 ; (syntax-object -> boolean)
                 syntax-object-resizable?
                 ; (syntax-object -> (listof syntax-object))
                 get-syntax-objects-to-rename-from-syntax-object
                 ; (syntax-object -> style-delta%)
                 get-style-delta-from-syntax-object
                 
                 ; OPTIONAL snip stuff
                 ; (symbol -> style-delta%)
                 (get-box-style-delta-from-snip-type error-no-snips)
                 ; (symbol -> string)
                 (get-menu-text-from-snip-type error-no-snips)
                 ; (symbol syntax-object -> (listof string))
                 (get-snip-text-from-snip-type-and-syntax-object error-no-snips)
                 ; (listof symbol)
                 (snip-type-list '())
                 
                 ; boolean
                 (clear-colors-after-user-action? #f)
                 
                 ; OPTIONAL ARROW STUFF
                 ; brush%
                 (tacked-arrow-brush (send the-brush-list find-or-create-brush "BLUE" 'solid))
                 ; brush%
                 (untacked-arrow-brush (send the-brush-list find-or-create-brush "WHITE" 'solid))
                 ; pen%
                 (arrow-pen (send the-pen-list find-or-create-pen "BLUE" 1 'solid)))
      (make-register-label-with-gui
       syntax-source
       (lambda (x) x)
       syntax-position
       syntax-span
       get-parents-from-syntax-object
       get-children-from-syntax-object
       syntax-object-resizable?
       syntax-object->datum
       ; this function will get a list of labels, but in the present case the labels are
       ; directly syntax objects, and register-label-with-gui in the model garantees that
       ; all the labels correspond to the same syntax object, therefore we know that the
       ; list contains at least one syntax-object (otherwise this function wouldn't be called)
       ; and that, if there are more than one syntax object in the list, they are all the
       ; same.  So we can just apply the function to the first one.
       (lambda (syntax-objects)
         (get-syntax-objects-to-rename-from-syntax-object (car syntax-objects)))
       get-style-delta-from-syntax-object
       get-box-style-delta-from-snip-type
       get-menu-text-from-snip-type
       get-snip-text-from-snip-type-and-syntax-object
       snip-type-list
       clear-colors-after-user-action?
       tacked-arrow-brush
       untacked-arrow-brush
       arrow-pen)))
  
  )
