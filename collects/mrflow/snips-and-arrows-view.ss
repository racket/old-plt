
(module snips-and-arrows-view mzscheme
  (require
   (lib "class.ss")
   (lib "mred.ss" "mred")
   (prefix fw: (lib "framework.ss" "framework"))
   (prefix arrow: (lib "arrow.ss" "drscheme"))
   
   (prefix cst: "constants.ss")
   (prefix saam: "snips-and-arrows-model.ss")
   )
  
  (provide
   make-gui-view-state ; text% (label -> top) (label -> non-negative-exact-integer) (label -> non-negative-exact-integer) (label -> style-delta%) (symbol -> style-delta%) (listof symbol) boolean (label label -> string) -> gui-view-state
   
   (rename gui-view-state-analysis-currently-modifying? analysis-currently-modifying?) ; gui-view-state -> boolean
   color-all-labels ; gui-view-state -> void
   after-user-action ; gui-view-state -> void
   
   register-label-with-gui ; gui-view-state label gui-state -> void
   register-source-with-gui ; gui-view-state top gui-state -> void
   is-source-registered? ; gui-view-state top -> boolean
   get-related-labels-from-drscheme-pos-and-source ; gui-view-state non-negative-exact-integer top -> (listof label)
   user-resize-label ; gui-view-state label string -> void
   
   add-arrow ; gui-view-state label label boolean -> void
   get-parents-tacked-arrows ; gui-view-state label -> non-negative-exact-integer
   get-children-tacked-arrows ; gui-view-state label -> non-negative-exact-integer
   remove-arrows ; gui-view-state label (union symbol boolean) boolean -> void
   redraw-arrows ; gui-view-state dc% real real -> void
   
   invalidate-bitmap-cache ; gui-view-state -> void
   
   label-has-snips-of-this-type? ; gui-view-state label symbol -> boolean
   snips-currently-displayed-in-source? ; gui-view-state top -> boolean
   for-each-snip-type ; gui-view-state (symbol -> void) -> void
   add-snips ; gui-view-state label symbol text% (listof string) -> void
   remove-inserted-snips ; gui-view-state label symbol text% -> void
   remove-all-snips-in-source ; gui-view-state text% -> void
   remove-all-snips-and-arrows-and-colors ; gui-view-state -> void
   )
  
  (define-struct gui-view-state (; gui-model-state
                                 gui-model-state
                                 ; test%
                                 definitions-text
                                 ; (label -> top)
                                 get-source-from-label
                                 ; boolean
                                 ; so we can differenciate between actions done by the analysis and actions
                                 ; done by the user. Also prevents an infinite loop when deleting: if the user
                                 ; deletes something, it triggers a call to after-delete, which deletes all the
                                 ; snips, which triggers calls to after-delete, etc... so after-delete needs to
                                 ; be wrapped to prevent an infinite loop.
                                 analysis-currently-modifying?
                                 ; (label -> style-delta%)
                                 get-style-delta-from-label
                                 ; (listof (cons symbol style-delta%))
                                 snip-types-and-styles
                                 ; style-delta%
                                 original-style
                                 ; boolean
                                 clear-colors-after-user-action?
                                 ))
  
  ; text%
  ; (label -> top)
  ; (label -> non-negative-exact-integer)
  ; (label -> non-negative-exact-integer)
  ; (label -> style-delta%)
  ; (symbol -> style-delta%)
  ; (listof symbol)
  ; (label label -> string)
  ; boolean
  ; -> gui-view-state
  (set! make-gui-view-state
        (let ([real-make-gui-view-state make-gui-view-state])
          (lambda (definitions-text
                   get-source-from-label
                   get-mzscheme-position-from-label
                   get-span-from-label
                   get-style-delta-from-label
                   snip-types-and-styles
                   get-arrow-color-from-labels
                   clear-colors-after-user-action?)
            (real-make-gui-view-state (saam:make-gui-model-state get-source-from-label
                                                                 get-mzscheme-position-from-label
                                                                 get-span-from-label
                                                                 (map car snip-types-and-styles)
                                                                 get-arrow-color-from-labels)
                                      definitions-text
                                      get-source-from-label
                                      #f
                                      get-style-delta-from-label
                                      snip-types-and-styles
                                      (send (fw:scheme:get-style-list) find-named-style "standard")
                                      clear-colors-after-user-action?))))
  
  ; INTERFACE BETWEEN MODEL AND TOP MODULE
  ; gui-view-state non-negative-exact-integer top -> (listof label)
  (define (get-related-labels-from-drscheme-pos-and-source gui-view-state pos source)
    (saam:get-related-labels-from-drscheme-pos-and-source
     (gui-view-state-gui-model-state gui-view-state) pos source))
  
  ; gui-view-state label gui-state -> void
  ; registers a label with the gui. We need to pass the whole gui-state as an argument because
  ; we'll need to initialize the source's state the first time we see that source, to make
  ; sure all source are sharing the same state.  Note that we can't give just gui-state as
  ; an argument and extract gui-view-state from it, because we need the gui-state-gui-view-state
  ; function to do that which means this module would depend on the snips-and-arrows one, thereby
  ; creating a loop in the module dependencies...
  ; Note that we could color the label as we go, thereby having incremental coloring as we
  ; analyze terms, but that turns out to be *very* slow, because the editor has to be unlocked
  ; (because of disable-evalution), the style changed, the editor re-lock and the bitmap cache
  ; invalidated for each label in turn.  It would also possibly not show all the arrows for a
  ; given label while the analysis is still going on.
  (define (register-label-with-gui gui-view-state label gui-state)
    (let ([source (saam:register-label-with-gui (gui-view-state-gui-model-state gui-view-state) label)])
      (when source
        (send source initialize-snips-and-arrows-gui-state gui-state)))
    cst:void)
  
  ; gui-view-state top gui-state -> void
  ; Same as above, except that we register a source instead of a label.  We use this to always
  ; register the definitions window (see comment in make-register-label-with-gui in
  ; snips-and-arrows.ss).
  (define (register-source-with-gui gui-view-state source gui-state)
    (let ([source (saam:register-source-with-gui (gui-view-state-gui-model-state gui-view-state) source)])
      (when source
        (send source initialize-snips-and-arrows-gui-state gui-state)))
    cst:void)
  
  ; gui-view-state top -> boolean
  (define (is-source-registered? gui-view-state source)
    (saam:is-source-registered? (gui-view-state-gui-model-state gui-view-state) source))
  
  ; gui-view-state (symbol -> void) -> void
  (define (for-each-snip-type gui-view-state f)
    (saam:for-each-snip-type (gui-view-state-gui-model-state gui-view-state) f))
  
  ; gui-view-state label symbol -> boolean
  (define (label-has-snips-of-this-type? gui-view-state label type)
    (saam:label-has-snips-of-this-type? (gui-view-state-gui-model-state gui-view-state) label type))
  
  ; gui-view-state top -> boolean
  (define (snips-currently-displayed-in-source? gui-view-state source)
    (saam:snips-currently-displayed-in-source? (gui-view-state-gui-model-state gui-view-state) source))
  
  ; gui-view-state label -> non-negative-exact-integer
  (define (get-parents-tacked-arrows gui-view-state label)
    (saam:get-parents-tacked-arrows (gui-view-state-gui-model-state gui-view-state) label))
  
  ; gui-view-state label -> non-negative-exact-integer
  (define (get-children-tacked-arrows gui-view-state label)
    (saam:get-children-tacked-arrows (gui-view-state-gui-model-state gui-view-state) label))
  
  ; gui-view-state label label boolean -> void
  (define (add-arrow gui-view-state start-label end-label tacked?)
    (saam:add-arrow (gui-view-state-gui-model-state gui-view-state) start-label end-label tacked?))
  
  ; gui-view-state label (union symbol boolean) boolean -> void
  (define (remove-arrows gui-view-state start-label tacked? exn?)
    (saam:remove-arrows (gui-view-state-gui-model-state gui-view-state) start-label tacked? exn?))
  
  
  ; COLORING / CLEARING
  ; gui-view-state -> void
  ; Color all registered labels.  Note that we know that no user modifications will be
  ; possible while we color (snips-and-arrows.ss takes care of that through can-insert?
  ; can-delete?) so there's no need to lock the editors.
  (define (color-all-labels gui-view-state)
    (let* ([gui-model-state (gui-view-state-gui-model-state gui-view-state)]
           [get-span-from-label (saam:make-get-span-from-label-from-model-state gui-model-state)]
           [get-style-delta-from-label (gui-view-state-get-style-delta-from-label gui-view-state)])
      (saam:for-each-source
       gui-model-state
       (lambda (source)
         (send source begin-edit-sequence #f)
         (saam:for-each-label-in-source
          gui-model-state
          source
          (lambda (label)
            (let ([label-left-pos (saam:get-position-from-label gui-model-state label)])
              (send source change-style (get-style-delta-from-label label)
                    label-left-pos (+ label-left-pos (get-span-from-label label)) #f))))
         (send source end-edit-sequence)))
      (invalidate-bitmap-cache gui-view-state)))
  
  ; gui-view-state -> void
  ; resets all colors to original style
  (define (clear-all-colors gui-view-state)
    (let ([original-style (gui-view-state-original-style gui-view-state)])
      (saam:for-each-source (gui-view-state-gui-model-state gui-view-state)
                            (lambda (source)
                              (send source begin-edit-sequence #f)
                              (send source change-style original-style 0 (send source last-position) #f)
                              (send source end-edit-sequence)))
      (invalidate-bitmap-cache gui-view-state)))
  
  ; gui-view-state -> void
  ; remove arrows and all snips, editor by editor.
  (define (remove-all-snips-and-arrows gui-view-state)
    (let ([gui-model-state (gui-view-state-gui-model-state gui-view-state)])
      (set-gui-view-state-analysis-currently-modifying?! gui-view-state #t)
      (saam:remove-all-arrows gui-model-state)
      (invalidate-bitmap-cache gui-view-state)
      (saam:for-each-source gui-model-state
                            (lambda (source)
                              (remove-all-snips-in-source gui-view-state source)))
      (set-gui-view-state-analysis-currently-modifying?! gui-view-state #f)))
  
  ; gui-view-state text% -> void
  ; Remove all snips in a given editor.  We loop over each label and then loop over each
  ; snip type and remove the corresponding snip group.  It would probably be much faster
  ; to first get the positions of the groups of all snips for each label (since for a given
  ; label all the groups of snips of different types are next to each other), sort them
  ; by decreasing position (so that removing a group of snip doesn't require recomputing
  ; the positions of the remaining groups), then remove them in that order.  I might do
  ; that one day if people complain of slowness...
  (define (remove-all-snips-in-source gui-view-state source)
    (let ([gui-model-state (gui-view-state-gui-model-state gui-view-state)])
      (send source begin-edit-sequence #f)
      (saam:for-each-label-in-source
       gui-model-state
       source
       (lambda (label)
         (saam:for-each-snip-type
          gui-model-state
          (lambda (type)
            (when (saam:label-has-snips-of-this-type? gui-model-state label type)
              (remove-inserted-snips gui-view-state label type source))))))
      (send source end-edit-sequence)))
  
  ; gui-view-state -> void
  ; invalidates the bitmap cache of the definition window, which will call the overridden
  ; on-paint method of the definition window and redraw the arrows.
  (define (invalidate-bitmap-cache gui-view-state)
    (send (gui-view-state-definitions-text gui-view-state) invalidate-bitmap-cache))
  
  ; gui-view-state -> void
  ; Resets the state of all sources we know about.  Last nail in the coffin for
  ; this analysis round.
  (define (reset-all-sources-state gui-view-state)
    (saam:for-each-source (gui-view-state-gui-model-state gui-view-state)
                          (lambda (source)
                            (send source reset-snips-and-arrows-state))))  
  
  
  ; EDITOR EVENTS INTERACTION
  ; gui-view-state -> void
  ; the user has started modifying stuff, so we just remove all snips (in other sources only,
  ; since we know a user modification is only allowed if the current source doesn't have
  ; any snips - the current source is currently locked anyway) and all arrows (in all sources),
  (define (after-user-action gui-view-state)
    (remove-all-snips-and-arrows gui-view-state)
    (when (gui-view-state-clear-colors-after-user-action? gui-view-state)
      (clear-all-colors gui-view-state)
      (reset-all-sources-state gui-view-state)))
  
  ; gui-view-state -> void
  ; clear all and reset all
  (define (remove-all-snips-and-arrows-and-colors gui-view-state)
    (remove-all-snips-and-arrows gui-view-state)
    (clear-all-colors gui-view-state)
    (reset-all-sources-state gui-view-state))
  
  ; gui-view-state dc% real real -> void
  ; redraws arrows during on-paint
  (define (redraw-arrows gui-view-state dc dx dy)
    (let ([top-source (gui-view-state-definitions-text gui-view-state)]
          [untacked-arrow-brush (send the-brush-list find-or-create-brush "white" 'solid)]
          [old-pen (send dc get-pen)]
          [old-brush (send dc get-brush)])
      (saam:for-each-arrow (gui-view-state-gui-model-state gui-view-state)
                           (lambda (start-label-pos-left end-label-pos-left
                                                         start-label-span end-label-span
                                                         start-source end-source
                                                         tacked? color)
                             (send dc set-pen (send the-pen-list find-or-create-pen color 1 'solid))
                             (if tacked?
                                 (send dc set-brush (send the-brush-list find-or-create-brush color 'solid))
                                 (send dc set-brush untacked-arrow-brush))
                             (draw-arrow start-label-pos-left
                                         (+ start-label-pos-left start-label-span)
                                         end-label-pos-left
                                         (+ end-label-pos-left end-label-span)
                                         top-source
                                         start-source
                                         end-source
                                         dc dx dy)))
      (send dc set-pen old-pen)
      (send dc set-brush old-brush)))
  
  ; TEXT
  ; gui-view-state label string -> void
  ; resize and re-color one label
  ; the caller of user-resize-label can't tell us what the source of label
  ; is, because the label to resize might be in a different editor than the one
  ; where the resize menu was used, so we have to use get-source-from-label
  (define (user-resize-label gui-view-state label new-string)
    (let ([gui-model-state (gui-view-state-gui-model-state gui-view-state)]
          [source ((gui-view-state-get-source-from-label gui-view-state) label)])
      (set-gui-view-state-analysis-currently-modifying?! gui-view-state #t)
      (let-values ([(starting-pos old-ending-pos new-ending-pos)
                    (saam:user-resize-label gui-model-state
                                            label
                                            (string-length new-string))])
        (send source begin-edit-sequence #f)
        (send source insert new-string starting-pos old-ending-pos)
        (send source change-style
              ((gui-view-state-get-style-delta-from-label gui-view-state) label)
              starting-pos new-ending-pos #f)
        (send source end-edit-sequence)
        )
      (set-gui-view-state-analysis-currently-modifying?! gui-view-state #f)))
  
  
  ; SNIPS
  ; gui-view-state label symbol text% (listof string) -> void
  ; Adds snips of given type to given label.
  ; We could get the source from the label, but there's no reason to bother...
  (define (add-snips gui-view-state label type source snips-content)
    (set-gui-view-state-analysis-currently-modifying?! gui-view-state #t)
    (let ([snip-style
           (cdr (assq type (gui-view-state-snip-types-and-styles gui-view-state)))]
          [starting-pos (saam:add-snips (gui-view-state-gui-model-state gui-view-state)
                                        label type source (length snips-content))])
      (send source begin-edit-sequence #f)
      (for-each (lambda (snip-content)
                  (let* ([snip-text (make-object text%)]
                         [snip (make-object editor-snip% snip-text)])
                    (send snip-text insert snip-content)
                    (send snip-text lock #t)
                    (send source insert snip starting-pos starting-pos)
                    ; XXX bug here on Solaris, can be worked around
                    ; (invalidate-bitmap-cache gui-view-state)
                    ; see collects/test/tool2.ss
                    (send source change-style snip-style
                          starting-pos (add1 starting-pos) #f)))
                snips-content)
      (send source end-edit-sequence))
    (invalidate-bitmap-cache gui-view-state)
    (set-gui-view-state-analysis-currently-modifying?! gui-view-state #f))
  
  ; gui-view-state label symbol text% -> void
  ; Remove snips for a given label and type.
  ; We could get the source from the label, but there's no reason to bother...
  (define (remove-inserted-snips gui-view-state label type source)
    (set-gui-view-state-analysis-currently-modifying?! gui-view-state #t)
    (let-values ([(starting-pos ending-pos)
                  (saam:remove-inserted-snips (gui-view-state-gui-model-state gui-view-state)
                                              label type source)])
      ; all the snips for a given label and type are contiguous and deleted at once.
      (send source begin-edit-sequence #f)
      (send source delete starting-pos ending-pos #f)
      (send source end-edit-sequence))
    (invalidate-bitmap-cache gui-view-state)
    (set-gui-view-state-analysis-currently-modifying?! gui-view-state #f))
  
  
  ; ARROWS
  ; (box number) (box number) -> number
  (define (average box1 box2)
    (/ (+ (unbox box1) (unbox box2)) 2))
  
  ; non-negative-exact-integer non-negative-exact-integer non-negative-exact-integer non-negative-exact-integer
  ; text% text% text% dc% real real -> void
  ; Computes actual locations for arrow and draws it.
  ; Note that we don't do anything to prevent arrows of length zero from being drawn - these
  ; might show up when using macros that duplicate terms, so arrows of length zero are then
  ; the correct thing to do as far as I am concerned).
  (define (draw-arrow start-label-pos-left start-label-pos-right
                      end-label-pos-left end-label-pos-right
                      top-source start-source end-source
                      dc dx dy)
    (let ([start-sub-ed-left-x-loc (box 0)]
          [start-sub-ed-top-y-loc (box 0)]
          [start-sub-ed-right-x-loc (box 0)]
          [start-sub-ed-bot-y-loc (box 0)]
          [end-sub-ed-left-x-loc (box 0)]
          [end-sub-ed-top-y-loc (box 0)]
          [end-sub-ed-right-x-loc (box 0)]
          [end-sub-ed-bot-y-loc (box 0)])
      (send start-source position-location start-label-pos-left start-sub-ed-left-x-loc start-sub-ed-top-y-loc #t)
      (send start-source position-location start-label-pos-right start-sub-ed-right-x-loc #f #f)
      (send start-source position-location (sub1 start-label-pos-right) #f start-sub-ed-bot-y-loc #f)
      (send end-source position-location end-label-pos-left end-sub-ed-left-x-loc end-sub-ed-top-y-loc #t)
      (send end-source position-location end-label-pos-right end-sub-ed-right-x-loc #f #f)
      (send end-source position-location (sub1 end-label-pos-right) #f end-sub-ed-bot-y-loc #f)
      (let*-values
          ([(start-sub-ed-x-loc) (average start-sub-ed-left-x-loc start-sub-ed-right-x-loc)]
           [(start-sub-ed-y-loc) (average start-sub-ed-top-y-loc start-sub-ed-bot-y-loc)]
           [(end-sub-ed-x-loc) (average end-sub-ed-left-x-loc end-sub-ed-right-x-loc)]
           [(end-sub-ed-y-loc) (average end-sub-ed-top-y-loc end-sub-ed-bot-y-loc)]
           [(start-dc-x-loc start-dc-y-loc)
            (send start-source editor-location-to-dc-location start-sub-ed-x-loc start-sub-ed-y-loc)]
           [(end-dc-x-loc end-dc-y-loc)
            (send end-source editor-location-to-dc-location end-sub-ed-x-loc end-sub-ed-y-loc)]
           [(start-top-ed-x-loc start-top-ed-y-loc)
            (send top-source dc-location-to-editor-location start-dc-x-loc start-dc-y-loc)]
           [(end-top-ed-x-loc end-top-ed-y-loc)
            (send top-source dc-location-to-editor-location end-dc-x-loc end-dc-y-loc)])
        (arrow:draw-arrow
         dc start-top-ed-x-loc start-top-ed-y-loc end-top-ed-x-loc end-top-ed-y-loc dx dy))))
  
  )
