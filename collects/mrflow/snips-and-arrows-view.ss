; behavior enforced by this module:
; - snips always appear on the left of terms
; - user insertion or deletion makes snips and arrows disappear

(module snips-and-arrows-view mzscheme
  (require
   (lib "class.ss")
   (lib "list.ss")
   (lib "mred.ss" "mred")
   (prefix arrow: (lib "arrow.ss" "drscheme"))
   
   (prefix saam: "snips-and-arrows-model.ss")
   )
  
  (provide
   gui-state? ; top -> boolean
   make-gui-state  ; text% (non-negative-exact-integer -> label) (label -> non-negative-exact-integer) (label -> non-negative-exact-integer) (label -> style-delta%) (symbol -> style-delta%) (listof symbol) -> gui-state
   text-modified? ; gui-state -> boolean
   get-related-label-from-drscheme-pos ; gui-state non-negative-exact-integer -> (union label #f)
   label-has-snips-of-this-type? ; gui-state label symbol -> boolean
   color-text ; gui-state -> void
   clear-all-snips-and-arrows ; gui-state -> void
   after-insert ; gui-state exact-non-negative-integer exact-non-negative-integer -> void
   after-delete ; gui-state exact-non-negative-integer exact-non-negative-integer -> void
   on-paint ; gui-state dc% real real pen% brush% brush% -> void
   add-arrow ; gui-state label label boolean -> void
   get-parents-tacked-arrows  ; gui-state label -> non-negative-exact-integer
   get-children-tacked-arrows  ; gui-state label -> non-negative-exact-integer
   remove-arrows ; gui-state label (union symbol boolean) boolean -> void
   add-snips ; gui-state label symbol (listof top) -> void
   remove-inserted-snips ; gui-state label symbol -> void
   )   
  
  ; gui-view-state gui-model-state
  (define-struct gui-state (gui-view-state gui-model-state))
  
  (define-struct gui-view-state (; text%
                                 this
                                 ; boolean
                                 text-modified?
                                 ; boolean boolean
                                 ; so we can differenciate between actions done by the analysis and actions
                                 ; done by the user. Also prevents an infinite loop when deleting: if the user
                                 ; deletes something, it triggers a call to after-delete, which deletes all the
                                 ; snips, which triggers calls to after-delete, etc... so after-delete needs to
                                 ; be wrapped to prevent an infinite loop.
                                 analysis-currently-inserting?
                                 analysis-currently-deleting?
                                 ; (label -> style-delta%)
                                 get-style-delta-from-label
                                 ; (symbol -> style-delta%)
                                 get-box-style-delta-from-snip-type
                                 ))
  
  ; text%
  ; (non-negative-exact-integer -> label)
  ; (label -> non-negative-exact-integer)
  ; (label -> non-negative-exact-integer)
  ; (label -> style-delta%)
  ; (symbol -> style-delta%)
  ; (listof symbol)
  ; -> gui-state
  (set! make-gui-state
        (let ([real-make-gui-state make-gui-state])
          (lambda (this
                   get-label-from-mzscheme-position
                   get-mzscheme-position-from-label
                   get-span-from-label
                   get-style-delta-from-label
                   get-box-style-delta-from-snip-type
                   type-list)
            (real-make-gui-state (make-gui-view-state this
                                                      #f
                                                      #f
                                                      #f
                                                      get-style-delta-from-label
                                                      get-box-style-delta-from-snip-type)
                                 (saam:make-gui-model-state get-label-from-mzscheme-position
                                                            get-mzscheme-position-from-label
                                                            get-span-from-label
                                                            type-list)))))
  
  
  ; INTERFACE BETWEEN MODEL AND USER PROGRAM
  ; gui-state non-negative-exact-integer -> (union label #f)
  (define (get-related-label-from-drscheme-pos gui-state pos)
    (saam:get-related-label-from-drscheme-pos (gui-state-gui-model-state gui-state) pos))
  
  ; gui-state label symbol -> boolean
  (define (label-has-snips-of-this-type? gui-state label type)
    (saam:label-has-snips-of-this-type? (gui-state-gui-model-state gui-state) label type))
  
  ; gui-state label -> non-negative-exact-integer
  (define (get-parents-tacked-arrows gui-state label)
    (saam:get-parents-tacked-arrows (gui-state-gui-model-state gui-state) label))
  
  ; gui-state label -> non-negative-exact-integer
  (define (get-children-tacked-arrows gui-state label)
    (saam:get-children-tacked-arrows (gui-state-gui-model-state gui-state) label))
  
  ; gui-state label (union symbol boolean) boolean -> void
  (define (remove-arrows gui-state start-label tacked? exn?)
    (saam:remove-arrows (gui-state-gui-model-state gui-state) start-label tacked? exn?))

  ; gui-state label label boolean -> void
  (define (add-arrow gui-state start-label end-label tacked?)
    (saam:add-arrow (gui-state-gui-model-state gui-state) start-label end-label tacked?))
  
  
  ; MISC
  ; gui-state -> boolean
  (define (text-modified? gui-state)
    (gui-view-state-text-modified? (gui-state-gui-view-state gui-state)))
  
  
  ; COLORING / CLEARING
  ; gui-state -> void
  (define (color-text gui-state)
    (let* ([gui-model-state (gui-state-gui-model-state gui-state)]
           [gui-view-state (gui-state-gui-view-state gui-state)]
           [this (gui-view-state-this gui-view-state)]
           ; we assume the model's state doesn't change throughout the coloring phase
           ; (i.e. the user doesn't start changing stuff while we color). If that's not
           ; true, then we'll need to lock the editor during coloring
           [get-span-from-label (saam:get-get-span-from-label-from-model-state gui-model-state)]
           [get-style-delta-from-label (gui-view-state-get-style-delta-from-label gui-view-state)])
      (send this begin-edit-sequence #f)
      (let ([last-pos (send this last-position)])
        (let loop-pos ([pos-start 0])
          (when (< pos-start last-pos)
            (let ([label (saam:get-label-from-drscheme-pos gui-model-state pos-start)])
              (if label
                  (let ([pos-end (+ pos-start (get-span-from-label label))])
                    (send this change-style (get-style-delta-from-label label) pos-start pos-end)
                    (loop-pos pos-end))
                  ; nothing to color
                  (loop-pos (add1 pos-start)))))))
      (send this end-edit-sequence)
      (set-gui-view-state-text-modified?! gui-view-state #t)))
  
  ; gui-state -> void
  ; remove all snips, group by group. We sort first, to make sure we remove snips from
  ; bottom to top. If we removed snips in any other order, we would have to recompute the
  ; positions of the remaining snips each time we removed one.
  (define (clear-all-snips-and-arrows gui-state)
    (let* ([gui-model-state (gui-state-gui-model-state gui-state)]
           [gui-view-state (gui-state-gui-view-state gui-state)]
           [this (gui-view-state-this gui-view-state)])
      (set-gui-view-state-analysis-currently-deleting?! gui-view-state #t)
      (send this begin-edit-sequence #f)
      (for-each
       (lambda (snip-group-pos&size-pair)
         (let ([pos (car snip-group-pos&size-pair)])
           (send this delete pos (+ pos (cdr snip-group-pos&size-pair)) #f)))
       (quicksort (saam:remove-all-snips-and-arrows gui-model-state)
                  (lambda (snip-group-pos&size-pair1 snip-group-pos&size-pair2)
                    (> (car snip-group-pos&size-pair1) (car snip-group-pos&size-pair2)))))
      (send this end-edit-sequence)
      (send this invalidate-bitmap-cache)
      (set-gui-view-state-text-modified?! gui-view-state #f)
      (set-gui-view-state-analysis-currently-deleting?! gui-view-state #f)))
    
  
  ; EDITOR EVENTS INTERACTION
  ; gui-state exact-non-negative-integer exact-non-negative-integer -> void
  ; we should account for the user's mdifications before clearing, otherwise we'll
  ; delete snips at the wrong place
  (define (after-insert gui-state start len)
    (let ([gui-view-state (gui-state-gui-view-state gui-state)])
      (when (and (text-modified? gui-state)
                 (not (gui-view-state-analysis-currently-inserting? gui-view-state)))
        (saam:after-insert (gui-state-gui-model-state gui-state) start len)
        (send (gui-view-state-this gui-view-state) clear-text))))
  
  ; gui-state exact-non-negative-integer exact-non-negative-integer -> void
  ; we should account for the user's mdifications before clearing, otherwise we'll
  ; delete snips at the wrong place
  (define (after-delete gui-state start len)
    (let ([gui-view-state (gui-state-gui-view-state gui-state)])
      (when (and (text-modified? gui-state)
                 (not (gui-view-state-analysis-currently-deleting? gui-view-state)))
        (saam:after-delete (gui-state-gui-model-state gui-state) start len)
        (send (gui-view-state-this gui-view-state) clear-text))))

  ; gui-state dc% real real pen% brush% brush% -> void
  (define (on-paint gui-state dc dx dy arrow-pen tacked-arrow-brush untacked-arrow-brush)
    (when (text-modified? gui-state)
      (draw-arrows gui-state dc dx dy arrow-pen tacked-arrow-brush untacked-arrow-brush)))

  
  ; SNIPS
  ; gui-state label symbol (listof top) -> void
  (define (add-snips gui-state label type snips-content)
    (let* ([gui-view-state (gui-state-gui-view-state gui-state)]
           [this (gui-view-state-this gui-view-state)])
      (set-gui-view-state-analysis-currently-inserting?! gui-view-state #t)
      (let ([get-box-style-delta-from-snip-type
             (gui-view-state-get-box-style-delta-from-snip-type gui-view-state)]
            [starting-pos (saam:add-snips (gui-state-gui-model-state gui-state)
                                          label type (length snips-content))])
        (send this begin-edit-sequence #f)
        (for-each (lambda (snip-content)
                    (let* ([snip-text (make-object text%)]
                           [snip (make-object editor-snip% snip-text)])
                      (send snip-text insert snip-content)
                      (send this insert snip starting-pos starting-pos)
                      (send this change-style
                            (get-box-style-delta-from-snip-type type)
                            starting-pos (add1 starting-pos))))
                  snips-content)
        (send this end-edit-sequence))
      ; invalidate the cache, so the arrows are redrawn at the right position after
      ; inserting the snips
      (send this invalidate-bitmap-cache)
      (set-gui-view-state-analysis-currently-inserting?! gui-view-state #f)))
  
  ; gui-state label symbol -> void
  ; remove snips for a given label and type
  (define (remove-inserted-snips gui-state label type)
    (let* ([gui-view-state (gui-state-gui-view-state gui-state)]
           [this (gui-view-state-this gui-view-state)])
      (set-gui-view-state-analysis-currently-deleting?! gui-view-state #t)
      (let-values ([(starting-pos ending-pos)
                    (saam:remove-inserted-snips (gui-state-gui-model-state gui-state) label type)])
        ; all the snips for a given label and type are contiguous and deleted at once.
        (send this begin-edit-sequence #f)
        (send this delete starting-pos ending-pos #f)
        (send this end-edit-sequence)
        )
      (set-gui-view-state-analysis-currently-deleting?! gui-view-state #f)))
  
  
  ; ARROWS
  ; (box number) (box number) -> number
  (define (average box1 box2)
    (/ (+ (unbox box1) (unbox box2)) 2))
  
  ; non-negative-exact-integer non-negative-exact-integer non-negative-exact-integer non-negative-exact-integer
  ; text% dc% real real -> void
  (define (draw-arrow start-label-pos-start-left end-label-pos-start-left
                      start-label-span end-label-span
                      this dc dx dy)
    (let* ([start-label-pos-start-right (add1 start-label-pos-start-left)]
           [start-label-pos-end (+ start-label-pos-start-left start-label-span)]
           [end-label-pos-start-right (add1 end-label-pos-start-left)]
           [end-label-pos-end (+ end-label-pos-start-left end-label-span)]
           [start-label-start-left-x (box 0)]
           [start-label-start-right-x (box 0)]
           [start-label-start-left-y-top (box 0)]
           [start-label-start-right-y-bot (box 0)]
           [start-label-end-x (box 0)]
           [start-label-end-y-top (box 0)]
           [start-label-end-y-bot (box 0)]
           [end-label-start-left-x (box 0)]
           [end-label-start-right-x (box 0)]
           [end-label-start-left-y-top (box 0)]
           [end-label-start-right-y-bot (box 0)]
           [end-label-end-x (box 0)]
           [end-label-end-y-top (box 0)]
           [end-label-end-y-bot (box 0)])
      (send this position-location start-label-pos-start-left start-label-start-left-x start-label-start-left-y-top #t)
      (send this position-location start-label-pos-start-right start-label-start-right-x start-label-start-right-y-bot #f)
      (send this position-location start-label-pos-end start-label-end-x start-label-end-y-top #t)
      (send this position-location start-label-pos-end #f start-label-end-y-bot #f)
      (send this position-location end-label-pos-start-left end-label-start-left-x end-label-start-left-y-top #t)
      (send this position-location end-label-pos-start-right end-label-start-right-x end-label-start-right-y-bot #f)
      (send this position-location end-label-pos-end end-label-end-x end-label-end-y-top #t)
      (send this position-location end-label-pos-end #f end-label-end-y-bot #f)
      (let ([start-not-wrapped? (= (unbox start-label-start-left-y-top) (unbox start-label-end-y-top))]
            [end-not-wrapped? (= (unbox end-label-start-left-y-top) (unbox end-label-end-y-top))])
        (cond
          [(and start-not-wrapped? end-not-wrapped?)
           (arrow:draw-arrow dc
                             (average start-label-start-left-x start-label-end-x)
                             (average start-label-start-left-y-top start-label-end-y-bot)
                             (average end-label-start-left-x end-label-end-x)
                             (average end-label-start-left-y-top end-label-end-y-bot)
                             dx dy)]
          [start-not-wrapped?
           (arrow:draw-arrow dc
                             (average start-label-start-left-x start-label-end-x)
                             (average start-label-start-left-y-top start-label-end-y-bot)
                             (average end-label-start-left-x end-label-start-right-x)
                             (average end-label-start-left-y-top end-label-start-right-y-bot)
                             dx dy)]
          [end-not-wrapped?
           (arrow:draw-arrow dc
                             (average start-label-start-left-x start-label-start-right-x)
                             (average start-label-start-left-y-top start-label-start-right-y-bot)
                             (average end-label-start-left-x end-label-end-x)
                             (average end-label-start-left-y-top end-label-end-y-bot)
                             dx dy)]
          [else
           (arrow:draw-arrow dc
                             (average start-label-start-left-x start-label-start-right-x)
                             (average start-label-start-left-y-top start-label-start-right-y-bot)
                             (average end-label-start-left-x end-label-start-right-x)
                             (average end-label-start-left-y-top end-label-start-right-y-bot)
                             dx dy)]))))
  
  ; gui-state dc% real real pen% brush% brush% -> void
  (define (draw-arrows gui-state dc dx dy arrow-pen tacked-arrow-brush untacked-arrow-brush)
    (let ([old-pen (send dc get-pen)]
          [old-brush (send dc get-brush)]
          [gui-view-state (gui-state-gui-view-state gui-state)])
      (send dc set-pen arrow-pen)
      ; draw each arrow by using a callback through the model
      (saam:for-each-arrow (gui-state-gui-model-state gui-state)
                           (lambda (start-label-pos-start-left end-label-pos-start-left start-label-span end-label-span tacked?)
                             (if tacked?
                                 (send dc set-brush tacked-arrow-brush)
                                 (send dc set-brush untacked-arrow-brush))
                             (draw-arrow start-label-pos-start-left end-label-pos-start-left
                                         start-label-span end-label-span
                                         (gui-view-state-this gui-view-state)
                                         dc dx dy)))
      (send dc set-pen old-pen)
      (send dc set-brush old-brush)))
  
  )
