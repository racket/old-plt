
(module snips-and-arrows mzscheme
  
  (require
   (lib "etc.ss")
   (lib "class.ss")
   (lib "mred.ss" "mred")
   (prefix fw: (lib "framework.ss" "framework"))
   (prefix strcst: (lib "string-constant.ss" "string-constants"))
   
   (prefix saav: "snips-and-arrows-view.ss")
   )
  
  (provide
   make-snips-and-arrows-mixins
   make-snips-and-arrows-mixins-for-syntax-objects
   )
  
  ; gui-state menu% label symbol (symbol -> string) (-> (listof string)) -> menu-item%
  ; creates a menu entry for a given snip type
  (define (create-snips-menu-item-by-type snips-and-arrows-gui-state menu label type get-menu-text-from-snip-type get-snip-strings)
    (if (saav:label-has-snips-of-this-type? snips-and-arrows-gui-state label type)
        ; delete menu entry
        (make-object menu-item%
          (get-menu-text-from-snip-type type 'hide)
          menu
          (lambda (item event)
            (saav:remove-inserted-snips snips-and-arrows-gui-state label type)))
        ; show menu entry
        (let ([snip-strings (get-snip-strings)])
          (unless (null? snip-strings)
            (make-object menu-item%
              (get-menu-text-from-snip-type type 'show)
              menu
              (lambda (item event)
                (saav:add-snips snips-and-arrows-gui-state label type snip-strings)))))))
  
  ; gui-state menu% label -> menu-item%
  ; create menu entries for arrows
  (define (create-arrow-menu-items snips-and-arrows-gui-state menu label get-parents-from-label get-children-from-label)
    (let* ([parents (get-parents-from-label label)]
           [parents-max-arrows (length parents)]
           [parents-tacked-arrows (saav:get-parents-tacked-arrows snips-and-arrows-gui-state label)]
           [children (get-children-from-label label)]
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
            (saav:invalidate-bitmap-caches snips-and-arrows-gui-state))))
      (when (> tacked-arrows 0)
        (make-object menu-item%
          (strcst:string-constant mrflow-popup-menu-untack-all-arrows)
          menu
          (lambda (item event)
            (saav:remove-arrows snips-and-arrows-gui-state label 'all #t)
            (saav:invalidate-bitmap-caches snips-and-arrows-gui-state))))))
  
  ; ... see below ... -> (values (label -> void) mixin mixin)
  ; Ouch...
  (define (make-snips-and-arrows-mixins
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
           ; (label -> (listof label))
           get-labels-to-rename-from-label
           ; (label -> style-delta%)
           get-style-delta-from-label
           ; (symbol -> style-delta%)
           get-box-style-delta-from-snip-type
           ; (symbol -> string)
           get-menu-text-from-snip-type
           ; (symbol label -> (-> (listof string)))
           get-snip-text-thunk-from-snip-type-and-label
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
    (let ([snips-and-arrows-gui-state
           (saav:make-gui-state
            get-source-from-label
            get-mzscheme-position-from-label
            get-span-from-label
            get-style-delta-from-label
            get-box-style-delta-from-snip-type
            snip-type-list
            clear-colors-after-user-action?)]
          [previous-label #f])
      (values
       ; label -> void
       ; label registering function
       (lambda (label)
         (saav:register-label-with-gui snips-and-arrows-gui-state label))
       
       ; drscheme:unit:add-to-program-editor-mixin mixin
       (lambda (super%)
         (class super%
           (rename [super-after-insert after-insert])
           ; exact-non-negative-integer exact-non-negative-integer -> void
           (define/override (after-insert start len)
             (super-after-insert start len)
             (saav:after-user-action snips-and-arrows-gui-state this start len))
           
           (rename [super-after-delete after-delete])
           ; exact-non-negative-integer exact-non-negative-integer -> void
           (define/override (after-delete start len)
             (super-after-delete start len)
             (saav:after-user-action snips-and-arrows-gui-state this start (- len)))
           
           (super-instantiate ())
           ))
       
       ; drscheme:get/extend:extend-definitions-text mixin
       (lambda (super%)
         (class super%
           
           ; -> void
           ; colors all registered labels
           (define/public (color-all-labels)
             (saav:color-all-labels snips-and-arrows-gui-state))
           
           ; -> void
           ; remove all snips and arrows, and resets text style in all editors
           (define/public (remove-all-snips-and-arrows-and-colors)
             (saav:remove-all-snips-and-arrows-and-colors snips-and-arrows-gui-state))
           
           (rename [super-on-paint on-paint])
           ; boolean dc% real real real real real real symbol -> void
           (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
             (super-on-paint before? dc left top right bottom dx dy draw-caret)
             (when (not before?)
               (saav:redraw-arrows snips-and-arrows-gui-state this dc dx dy arrow-pen tacked-arrow-brush untacked-arrow-brush)))
           
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
               [(not (saav:text-modified? snips-and-arrows-gui-state)) (super-on-event event)]
               [(and (send event button-down? 'right)
                     (let-values ([(pos editor) (get-drscheme-pos-and-editor event this)])
                       (if pos
                           (saav:get-related-label-from-drscheme-pos-and-source
                            snips-and-arrows-gui-state pos editor)
                           #f)))
                =>
                (lambda (label)
                  (let ([menu (make-object popup-menu%)])
                    ; SNIPS
                    (let ([create-snips-menu-item
                           (lambda (snip-type)
                             (create-snips-menu-item-by-type
                              snips-and-arrows-gui-state
                              menu label snip-type
                              get-menu-text-from-snip-type
                              (get-snip-text-thunk-from-snip-type-and-label snip-type label)))])
                      (for-each create-snips-menu-item snip-type-list))
                    ; ARROWS
                    (create-arrow-menu-items
                     snips-and-arrows-gui-state
                     menu
                     label
                     get-parents-from-label
                     get-children-from-label)
                    ; RESIZE
                    (when (label-resizable? label)
                      (let* ([old-name (get-name-from-label label)]
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
                                              (saav:user-resize-label snips-and-arrows-gui-state label new-name))
                                            (get-labels-to-rename-from-label label))))])
                        (make-object menu-item%
                          (strcst:string-constant cs-rename-id)
                          menu
                          new-name-callback)))
                    
                    (let-values ([(x y) (dc-location-to-editor-location (send event get-x) (send event get-y))])
                      (send (get-admin) popup-menu menu x y))
                    ))]
               [(send event leaving?)
                (when previous-label
                  (saav:remove-arrows snips-and-arrows-gui-state previous-label #f #f)
                  (set! previous-label #f)
                  (saav:invalidate-bitmap-caches snips-and-arrows-gui-state))]
               [(or (send event moving?)
                    (send event entering?))
                (let*-values ([(pos editor) (get-drscheme-pos-and-editor event this)]
                              [(label)
                               (if pos
                                   (saav:get-related-label-from-drscheme-pos-and-source
                                    snips-and-arrows-gui-state pos editor)
                                   #f)]
                              [(not-same-label) (not (eq? label previous-label))])
                  (when (and previous-label not-same-label)
                    (saav:remove-arrows snips-and-arrows-gui-state previous-label #f #f))
                  (when (and label not-same-label)
                    (for-each (lambda (parent-label)
                                (saav:add-arrow snips-and-arrows-gui-state parent-label label #f))
                              (get-parents-from-label label))
                    (for-each (lambda (child-label)
                                (saav:add-arrow snips-and-arrows-gui-state label child-label #f))
                              (get-children-from-label label)))
                  (when not-same-label
                    (set! previous-label label)
                    (saav:invalidate-bitmap-caches snips-and-arrows-gui-state)))]
               [else (super-on-event event)]))
           
           (super-instantiate ())
           )) ; mixin
       ))) ; make-snips-and-arrows-mixins
  
  ; symbol -> void
  ; default function for snip handling
  (define error-no-snips
    (case-lambda
      [(_) (error-no-snips 'dummy 'dummy)]
      [(_1 _2) (error 'snips-and-arrows "no snip info was provided when mixins were created")]))
  
  ; ... see below ... -> (values (label -> void) mixin mixin)
  ; simplified version of make-snips-and-arrows-mixins, specialized for syntax objects,
  ; and with default handling of snips
  (define make-snips-and-arrows-mixins-for-syntax-objects
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
                 ; (symbol syntax-object -> (-> (listof string)))
                 (get-snip-text-thunk-from-snip-type-and-syntax-object error-no-snips)
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
      (make-snips-and-arrows-mixins
       syntax-source
       syntax-position
       syntax-span
       get-parents-from-syntax-object
       get-children-from-syntax-object
       syntax-object-resizable?
       syntax-object->datum
       get-syntax-objects-to-rename-from-syntax-object
       get-style-delta-from-syntax-object
       get-box-style-delta-from-snip-type
       get-menu-text-from-snip-type
       get-snip-text-thunk-from-snip-type-and-syntax-object
       snip-type-list
       clear-colors-after-user-action?
       tacked-arrow-brush
       untacked-arrow-brush
       arrow-pen)))
  
  )
