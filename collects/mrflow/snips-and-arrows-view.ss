
(module snips-and-arrows-view mzscheme
  (require
   (lib "class.ss")
   (lib "list.ss")
   (lib "mred.ss" "mred")
   (prefix fw: (lib "framework.ss" "framework"))
   (prefix arrow: (lib "arrow.ss" "drscheme"))
   
   "assoc-set-hash.ss"
   (prefix saam: "snips-and-arrows-model.ss")
   )
  
  (provide
   make-gui-state ; (label -> top) (label -> non-negative-exact-integer) (label -> non-negative-exact-integer) (label -> style-delta%) (symbol -> style-delta%) (listof symbol) boolean -> gui-state
   
   snips-and-arrows-currently-running? ; gui-state -> boolean
   snips-currently-displayed-in-source? ; gui-state top -> boolean
   is-user-action? ; gui-state -> boolean
   color-all-labels ; gui-state -> void
   clear-all-colors ; gui-state -> void
   during-user-action ; gui-state -> void
   
   register-label-with-gui ; gui-state label -> void
   get-related-label-from-drscheme-pos-and-source ; gui-state non-negative-exact-integer top -> (setof label)
   user-resize-label ; gui-state label string -> void
   
   add-arrow ; gui-state label label boolean -> void
   get-parents-tacked-arrows ; gui-state label -> non-negative-exact-integer
   get-children-tacked-arrows ; gui-state label -> non-negative-exact-integer
   remove-arrows ; gui-state label (union symbol boolean) boolean -> void
   redraw-arrows ; gui-state dc% real real pen% brush% brush% -> void
   
   invalidate-bitmap-caches ; gui-state -> void
   
   label-has-snips-of-this-type? ; gui-state label symbol -> boolean
   add-snips ; gui-state label symbol (listof top) -> void
   remove-inserted-snips ; gui-state label symbol -> void
   remove-all-snips-in-source ; gui-state top -> void
   remove-all-snips-and-arrows-and-colors ; gui-state -> void
   )   
  
  ; gui-view-state gui-model-state
  (define-struct gui-state (gui-view-state gui-model-state))
  
  (define-struct gui-view-state (; (label -> top)
                                 get-source-from-label
                                 ; boolean
                                 snips-and-arrows-currently-running?
                                 ; boolean
                                 ; so we can differenciate between actions done by the analysis and actions
                                 ; done by the user. Also prevents an infinite loop when deleting: if the user
                                 ; deletes something, it triggers a call to after-delete, which deletes all the
                                 ; snips, which triggers calls to after-delete, etc... so after-delete needs to
                                 ; be wrapped to prevent an infinite loop.
                                 analysis-currently-modifying?
                                 ; (label -> style-delta%)
                                 get-style-delta-from-label
                                 ; (symbol -> style-delta%)
                                 get-box-style-delta-from-snip-type
                                 ; style-delta%
                                 original-style
                                 ; boolean
                                 clear-colors-after-user-action?
                                 ))
  
  ; (label -> top)
  ; (label -> non-negative-exact-integer)
  ; (label -> non-negative-exact-integer)
  ; (label -> style-delta%)
  ; (symbol -> style-delta%)
  ; (listof symbol)
  ; boolean
  ; -> gui-state
  (set! make-gui-state
        (let ([real-make-gui-state make-gui-state])
          (lambda (get-source-from-label
                   get-mzscheme-position-from-label
                   get-span-from-label
                   get-style-delta-from-label
                   get-box-style-delta-from-snip-type
                   snip-type-list
                   clear-colors-after-user-action?)
            (real-make-gui-state
             (make-gui-view-state get-source-from-label
                                  #f
                                  #f
                                  get-style-delta-from-label
                                  get-box-style-delta-from-snip-type
                                  (send (fw:scheme:get-style-list) find-named-style "standard")
                                  clear-colors-after-user-action?)
             (saam:make-gui-model-state get-source-from-label
                                        get-mzscheme-position-from-label
                                        get-span-from-label
                                        snip-type-list)))))
  
  ; gui-state -> void
  (define (reset-gui-state gui-state)
    (saam:reset-gui-model-state (gui-state-gui-model-state gui-state))
    (let ([gui-view-state (gui-state-gui-view-state gui-state)])
      (set-gui-view-state-analysis-currently-modifying?! gui-view-state #f)
      (set-gui-view-state-snips-and-arrows-currently-running?! gui-view-state #f)))
  
  
  ; INTERFACE BETWEEN MODEL AND USER PROGRAM
  ; gui-state non-negative-exact-integer top -> (setof label)
  (define (get-related-label-from-drscheme-pos-and-source gui-state pos source)
    (saam:get-related-label-from-drscheme-pos-and-source
     (gui-state-gui-model-state gui-state) pos source))
  
  ; gui-state label -> void
  (define (register-label-with-gui gui-state label)
    (saam:register-label-with-gui (gui-state-gui-model-state gui-state) label))
  
  ; gui-state label symbol -> boolean
  (define (label-has-snips-of-this-type? gui-state label type)
    (saam:label-has-snips-of-this-type? (gui-state-gui-model-state gui-state) label type))
  
  ; gui-state top -> boolean
  (define (snips-currently-displayed-in-source? gui-state source)
    (saam:snips-currently-displayed-in-source? (gui-state-gui-model-state gui-state) source))
  
  ; gui-state label -> non-negative-exact-integer
  (define (get-parents-tacked-arrows gui-state label)
    (saam:get-parents-tacked-arrows (gui-state-gui-model-state gui-state) label))
  
  ; gui-state label -> non-negative-exact-integer
  (define (get-children-tacked-arrows gui-state label)
    (saam:get-children-tacked-arrows (gui-state-gui-model-state gui-state) label))
  
  ; gui-state label label boolean -> void
  (define (add-arrow gui-state start-label end-label tacked?)
    (saam:add-arrow (gui-state-gui-model-state gui-state) start-label end-label tacked?))
  
  ; gui-state label (union symbol boolean) boolean -> void
  (define (remove-arrows gui-state start-label tacked? exn?)
    (saam:remove-arrows (gui-state-gui-model-state gui-state) start-label tacked? exn?))
  
  
  ; INTERNAL STATUS
  ; gui-state -> boolean
  (define (snips-and-arrows-currently-running? gui-state)
    (gui-view-state-snips-and-arrows-currently-running? (gui-state-gui-view-state gui-state)))
  
  ; gui-state -> boolean
  ; is the action user-initiated?
  (define (is-user-action? gui-state)
    (not (gui-view-state-analysis-currently-modifying? (gui-state-gui-view-state gui-state))))
  
  
  ; COLORING / CLEARING
  ; gui-state -> void
  (define (color-all-labels gui-state)
    (let* ([gui-model-state (gui-state-gui-model-state gui-state)]
           [gui-view-state (gui-state-gui-view-state gui-state)]
           ; we assume the model's state doesn't change throughout the coloring phase
           ; (i.e. the user doesn't start changing stuff while we color). If that's not
           ; true, then we'll need to lock the editor during coloring
           [get-span-from-label (saam:make-get-span-from-label-from-model-state gui-model-state)]
           [get-source-from-label (gui-view-state-get-source-from-label gui-view-state)]
           [get-style-delta-from-label (gui-view-state-get-style-delta-from-label gui-view-state)])
      (set-gui-view-state-snips-and-arrows-currently-running?! gui-view-state #t)
      (saam:for-each-source
       gui-model-state
       (lambda (source)
         (send source begin-edit-sequence #f)
         (saam:for-each-label-in-source
          gui-model-state source
          (lambda (label)
            (let ([label-left-pos (saam:get-position-from-label gui-model-state label)]
                  [source (get-source-from-label label)])
              (send source change-style (get-style-delta-from-label label)
                    label-left-pos (+ label-left-pos (get-span-from-label label))))))
         (send source end-edit-sequence))
       #t)
      (invalidate-bitmap-caches gui-state)))
  
  ; gui-state -> void
  ; resets all colors to original style
  (define (clear-all-colors gui-state)
    (let ([original-style (gui-view-state-original-style (gui-state-gui-view-state gui-state))])
      (saam:for-each-source (gui-state-gui-model-state gui-state)
                            (lambda (source)
                              (send source begin-edit-sequence #f)
                              (send source change-style original-style 0 (send source last-position))
                              (send source end-edit-sequence))
                            #t)
      (invalidate-bitmap-caches gui-state)))
  
  ; gui-state -> void
  ; remove all snips, group by group. We sort first, to make sure we remove snips from
  ; bottom to top. If we removed snips in any other order, we would have to recompute the
  ; positions of the remaining snips each time we removed one.
  (define (remove-all-snips-and-arrows gui-state)
    (let* ([gui-model-state (gui-state-gui-model-state gui-state)]
           [gui-view-state (gui-state-gui-view-state gui-state)])
      (set-gui-view-state-analysis-currently-modifying?! gui-view-state #t)
      (saam:remove-all-arrows gui-model-state)
      (invalidate-bitmap-caches gui-state)
      (saam:for-each-source gui-model-state
                            (lambda (source)
                              (remove-all-snips-in-source gui-state source)))))
  
  ; gui-state top -> void
  ; remove all snips in a given editor.
  (define (remove-all-snips-in-source gui-state source)
    (send source begin-edit-sequence #f)
    (saam:for-each-label-in-source
     (gui-state-gui-model-state gui-state)
     source
     (lambda (label)
       (remove-inserted-snips gui-state label 'all)))
    (send source end-edit-sequence))
  
  ; gui-state -> void
  ; invalidates the bitmap caches for sources that have been modified since the last call
  (define (invalidate-bitmap-caches gui-state)
    (saam:for-each-modified-source! (gui-state-gui-model-state gui-state)
                                    (lambda (source)
                                      (send source invalidate-bitmap-cache))))
  
  
  ; EDITOR EVENTS INTERACTION
  ; gui-state -> void
  ; the user has started modifying stuff, so we just remove all snips (in other sources only,
  ; since we know a user modification is only allowed if the current source doesn't have
  ; any snips - the current source is currently locked anyway) and all arrows (in all sources),
  (define (during-user-action gui-state)
    (remove-all-snips-and-arrows gui-state)
    (when (gui-view-state-clear-colors-after-user-action? (gui-state-gui-view-state gui-state))
      (clear-all-colors gui-state)
      (reset-gui-state gui-state)))
  
  ; gui-state -> void
  ; clear all and reset all
  (define (remove-all-snips-and-arrows-and-colors gui-state)
    (remove-all-snips-and-arrows gui-state)
    (clear-all-colors gui-state)
    (reset-gui-state gui-state))
  
  ; gui-state dc% real real pen% brush% brush% -> void
  ; redraws arrows during on-paint
  (define (redraw-arrows gui-state top-source dc dx dy arrow-pen tacked-arrow-brush untacked-arrow-brush)
    (when (snips-and-arrows-currently-running? gui-state)
      (let ([old-pen (send dc get-pen)]
            [old-brush (send dc get-brush)]
            [gui-view-state (gui-state-gui-view-state gui-state)])
        (send dc set-pen arrow-pen)
        (saam:for-each-arrow (gui-state-gui-model-state gui-state)
                             (lambda (start-label-pos-left end-label-pos-left
                                                           start-label-span end-label-span
                                                           start-source end-source tacked?)
                               (if tacked?
                                   (send dc set-brush tacked-arrow-brush)
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
        (send dc set-brush old-brush))))
  
  
  ; TEXT
  ; gui-state label string -> void
  ; resize and re-color one label
  (define (user-resize-label gui-state label new-string)
    (let* ([gui-model-state (gui-state-gui-model-state gui-state)]
           [gui-view-state (gui-state-gui-view-state gui-state)]
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
              starting-pos new-ending-pos)
        (send source end-edit-sequence)
        )
      (set-gui-view-state-analysis-currently-modifying?! gui-view-state #f)))
  
  
  ; SNIPS
  ; gui-state label symbol (listof top) -> void
  ; adds snips of given type to given label
  (define (add-snips gui-state label type snips-content)
    (let* ([gui-view-state (gui-state-gui-view-state gui-state)]
           [source ((gui-view-state-get-source-from-label gui-view-state) label)])
      (set-gui-view-state-analysis-currently-modifying?! gui-view-state #t)
      (let ([get-box-style-delta-from-snip-type
             (gui-view-state-get-box-style-delta-from-snip-type gui-view-state)]
            [starting-pos (saam:add-snips (gui-state-gui-model-state gui-state)
                                          label type (length snips-content))])
        (send source begin-edit-sequence #f)
        (for-each (lambda (snip-content)
                    (let* ([snip-text (make-object text%)]
                           [snip (make-object editor-snip% snip-text)])
                      (send snip-text insert snip-content)
                      (send source insert snip starting-pos starting-pos)
                      (send source change-style
                            (get-box-style-delta-from-snip-type type)
                            starting-pos (add1 starting-pos))))
                  snips-content)
        (send source end-edit-sequence))
      ; adding snips might make the arrow moves, but "insert" will provoke a call
      ; to "on-paint" even if the insertion occured in a sub-editor, so no need to
      ; manually invalidate the bitmap caches here.
      (set-gui-view-state-analysis-currently-modifying?! gui-view-state #f)))
  
  ; gui-state label symbol -> void
  ; remove snips for a given label and type
  (define (remove-inserted-snips gui-state label type)
    (let* ([gui-view-state (gui-state-gui-view-state gui-state)]
           [source ((gui-view-state-get-source-from-label gui-view-state) label)])
      (set-gui-view-state-analysis-currently-modifying?! gui-view-state #t)
      (let-values ([(starting-pos ending-pos)
                    (saam:remove-inserted-snips (gui-state-gui-model-state gui-state) label type)])
        ; all the snips for a given label and type are contiguous and deleted at once.
        ; starting-pos is #f when type is 'all and the label has no snips displayed.
        (when starting-pos
          (send source begin-edit-sequence #f)
          (send source delete starting-pos ending-pos #f)
          (send source end-edit-sequence)))
      ; removing snips might make the arrow moves, but "delete" will provoke a call
      ; to "on-paint" even if the deletion occured in a sub-editor, so no need to
      ; manually invalidate the bitmap caches here.
      (set-gui-view-state-analysis-currently-modifying?! gui-view-state #f)))
  
  
  ; ARROWS
  ; (box number) (box number) -> number
  (define (average box1 box2)
    (/ (+ (unbox box1) (unbox box2)) 2))
  
  ; non-negative-exact-integer non-negative-exact-integer non-negative-exact-integer non-negative-exact-integer
  ; text% text% text% dc% real real -> void
  ; computes actual locations for arrow and draws it
  (define (draw-arrow start-label-pos-left start-label-pos-right
                      end-label-pos-left end-label-pos-right
                      top-source start-source end-source
                      dc dx dy)
    (let* ([start-sub-ed-left-x-loc (box 0)]
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
