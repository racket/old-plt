; DrScheme starts counting positions at 0, MzScheme starts counting positions at 1.
; Syntax objects use MzScheme positions, all the positions in this file use DrScheme
; positions. In all cases positions are exact non-negative integer.
; Among DrScheme positions, some are so-called new positions "new-pos" and some are
; old positions "old-pos". An old position is a position in the editor before any snip
; was inserted. A new position is the same position in the editor, but after snips
; might have been inserted.
; (define-type position exact-non-negative-integer)
; DrScheme also has locations, which are real x and y coordinates in the editor.
; (define-type location real) these are not used here but are used in the view part.

(module snips-and-arrows-model mzscheme
  (require
   (prefix argexn: "arg-mismatch-exn.ss")
   (prefix cst: "constants.ss")
   ;(prefix set: "set-list.ss")
   (prefix set: "set-hash.ss")
   ;(prefix asset: "assoc-set-list.ss")
   (prefix asset: "assoc-set-hash.ss")
   )
  
  (provide
   make-gui-model-state ; (non-negative-exact-integer -> label) (label -> non-negative-exact-integer) (label -> non-negative-exact-integer) -> gui-model-state
   (rename get-label-from-drscheme-new-pos get-label-from-drscheme-pos) ; gui-model-state non-negative-exact-integer -> (union #f label)
   (rename get-related-label-from-drscheme-new-pos get-related-label-from-drscheme-pos) ; gui-model-state non-negative-exact-integer -> (union #f label)
   after-insert ; gui-model-state exact-non-negative-integer exact-non-negative-integer -> void
   after-delete ; gui-model-state exact-non-negative-integer exact-non-negative-integer -> void
   add-arrow ; gui-model-state label label boolean -> void
   for-each-arrow ; gui-model-state (non-negative-exact-integer non-negative-exact-integer non-negative-exact-integer non-negative-exact-integer boolean -> void) -> void
   remove-arrows ; gui-model-state label (union symbol boolean) boolean -> void
   remove-all-arrows ; gui-model-state -> void
   get-parents-tacked-arrows  ; gui-model-state label -> non-negative-exact-integer
   get-children-tacked-arrows  ; gui-model-state label -> non-negative-exact-integer
   label-has-snips-of-this-type? ; gui-model-state label symbol -> boolean
   add-snips ; gui-model-state label symbol non-negative-exact-integer -> non-negative-exact-integer
   remove-inserted-snips ; gui-model-state label symbol -> (value non-negative-exact-integer non-negative-exact-integer)
   remove-all-snips ; gui-model-state -> (listof non-negative-exact-integer)
   )
  
  ; DATA STRUCTURES
  ; label label boolean
  (define-struct arrow (start-label end-label tacked?))
  
  ; exact-non-negative-integer exact-non-negative-integer
  (define-struct snip-group (left-new-pos size))
  
  ; (assoc-setof symbol snip-group)
  (define-struct snips-groups (by-type))
  
  ; position (setof arrow) (setof arrow)
  ; We could recompute left-new-pos on the fly (from the MzScheme
  ; pos from the label itself and old-pos->new-pos) each time we needed to repaint,
  ; but in practice we repaint much more often then we add snips, so we keep the pos
  ; here as a cache which is computed once from scratch when we add the label to
  ; displayed-arrows and which is then just updated each time we add a new snip.
  ; Note that the data structure for a single arrow will be shared between two
  ; label-gui-data structures: it will appear once in the "starting-arrows"
  ; set for its start label, and once in the "ending-arrows" set for its end label.
  ; We need this because we need to be able to click at the end of an arrow and
  ; remove it if necessary.
  (define-struct label-gui-data (left-new-pos starting-arrows ending-arrows))
  
  (define-struct gui-model-state (; (assoc-setof label label-gui-data) (assoc-setof label snips-groups)
                                  ; arrows and snips are fairly independant so we keep them in two
                                  ; separate hash tables, even though they use the same keys
                                  arrows
                                  snips
                                  ; (non-negative-exact-integer -> label)
                                  get-label-from-mzscheme-position
                                  ; (label -> non-negative-exact-integer)
                                  get-mzscheme-position-from-label
                                  ; (label -> non-negative-exact-integer)
                                  get-span-from-label
                                  ; (listof symbol)
                                  type-list
                                  ))
  
  ; (non-negative-exact-integer -> label)
  ; (label -> non-negative-exact-integer)
  ; (label -> non-negative-exact-integer)
  ; -> gui-model-state
  (set! make-gui-model-state
        (let ([real-make-gui-model-state make-gui-model-state])
          (lambda (get-label-from-mzscheme-position
                   get-mzscheme-position-from-label
                   get-span-from-label
                   type-list)
            (real-make-gui-model-state (asset:make-assoc-set)
                                       (asset:make-assoc-set)
                                       get-label-from-mzscheme-position
                                       get-mzscheme-position-from-label
                                       get-span-from-label
                                       type-list))))
  
  
  ; DRSCHEME / MZSCHEME CONVERSIONS
  ; non-negative-exact-integer -> non-negative-exact-integer
  (define (drscheme-pos->mzscheme-pos pos)
    (add1 pos))
  ; non-negative-exact-integer -> non-negative-exact-integer
  (define (mzscheme-pos->drscheme-pos pos)
    (sub1 pos))
  
  
  ; LABEL / POS CONVERSIONS
  ; gui-model-state non-negative-exact-integer -> (union #f label)
  (define (get-label-from-drscheme-new-pos gui-model-state new-pos)
    ((gui-model-state-get-label-from-mzscheme-position gui-model-state)
     (drscheme-pos->mzscheme-pos
      (new-pos->old-pos gui-model-state new-pos))))
  
  ; gui-model-state non-negative-exact-integer -> (union #f label)
  ; finds the label corresponding to a given new-pos. Note that, because we
  ; use get-related-label-from-drscheme-old-pos for that purpose, we ignore
  ; the presence of snips, so the user will be able to click on the snips
  ; for that label and still get the label (in addition of being able to get
  ; the label by clicking on the term itself).
  (define (get-related-label-from-drscheme-new-pos gui-model-state new-pos)
    (get-related-label-from-drscheme-old-pos
     gui-model-state
     (new-pos->old-pos gui-model-state new-pos)))
  
  ; gui-model-state non-negative-exact-integer -> (union #f label)
  ; we loop down starting from old-pos, until we find a label. Then we have to check
  ; that the original old-pos falls within the span of that label.
  (define (get-related-label-from-drscheme-old-pos gui-model-state old-pos)
    (let ([get-label-from-mzscheme-position
           (gui-model-state-get-label-from-mzscheme-position gui-model-state)]
          [get-span-from-label
           (gui-model-state-get-span-from-label gui-model-state)])
      (let loop ([current-old-pos old-pos])
        (if (> 0 current-old-pos)
            #f
            (let ([label (get-label-from-mzscheme-position
                          (drscheme-pos->mzscheme-pos current-old-pos))])
              (if label
                  (if (< (- old-pos current-old-pos) (get-span-from-label label))
                      label
                      #f)
                  (loop (sub1 current-old-pos))))))))
  
  
  ; OLD-POS / NEW-POS CONVERSIONS
  ; gui-model-state exact-non-negative-int -> exact-non-negative-int
  ; converts an old position (before insertion of any snip) to a new position
  ; (after insertion of all the currently inserted snips).
  ; Note: the test is "<=", which means the new position is to the right of all
  ; the current snips that have positions corresponding to the same old position.
  (define (old-pos->new-pos gui-model-state old-pos)
    (let ([new-pos old-pos]
          [get-mzscheme-position-from-label
           (gui-model-state-get-mzscheme-position-from-label gui-model-state)])
      (asset:assoc-set-for-each
       (gui-model-state-snips gui-model-state)
       (lambda (label snips-groups)
         (when (<= (mzscheme-pos->drscheme-pos (get-mzscheme-position-from-label label))
                   old-pos)
           (asset:assoc-set-for-each
            (snips-groups-by-type snips-groups)
            (lambda (type snip-group)
              (set! new-pos (+ new-pos (snip-group-size snip-group))))))))
      new-pos))
  
  ; gui-model-state exact-non-negative-int -> exact-non-negative-int
  ; Note: the test is "<", because there might a snip that has the exact same
  ; position as new-pos, so, since a snip at position n is shown graphically
  ; between position n and n+1, we don't want to take that snip into account
  ; (i.e. that snip is on the right of the cursor or mouse pointer, not on the
  ; left).
  ; Note also that we have to be carefull: in old-pos->new-pos we add all the snips
  ; to the new-pos when the label has an old-pos to the left of or at the cursor.
  ; But here the cursor might be between two snips. So we have to consider each snip
  ; separately, we can't consider them group by group anymore.
  (define (new-pos->old-pos gui-model-state new-pos)
    (let ([old-pos new-pos])
      (asset:assoc-set-for-each
       (gui-model-state-snips gui-model-state)
       (lambda (label snips-groups)
         (asset:assoc-set-for-each
          (snips-groups-by-type snips-groups)
          (lambda (type snip-group)
            (let* ([left-new-pos (snip-group-left-new-pos snip-group)]
                   [size (snip-group-size snip-group)]
                   [right-new-pos (+ left-new-pos size)])
              (cond
                ; whole group of snips is on the left of new-pos
                [(< right-new-pos new-pos) (set! old-pos (- old-pos size))]
                ; if we arrive here, it means new-pos is somewhere in the middle of the
                ; current group of snips, so the number of snips of this group that's on
                ; the left of new-pos is (- new-pos left-new-pos).
                [(< left-new-pos new-pos) (set! old-pos (- old-pos (- new-pos left-new-pos)))]
                ; else the whole group of snips is on the right of the new-pos, so do nothing
                ))))))
      old-pos))
  
  ; gui-model-state exact-non-negative-integer exact-integer (exact-non-negative-integer exact-integer -> exact-integer) -> void
  ; moves all snips and arrows that are after start, by len. start is a new position (i.e. after
  ; insertion of snips). We need to do all that so that old-pos->new-pos and new-pos->old-pos
  ; and the arrow display keep working correctly when we add new snips in the middle of others.
  (define (move-poss gui-model-state start len add)
    (let ([move-pos (lambda (pos) (if (< pos start) pos (add pos len)))])
      (asset:assoc-set-for-each
       (gui-model-state-arrows gui-model-state)
       (lambda (label label-gui-data)
         (set-label-gui-data-left-new-pos! label-gui-data (move-pos (label-gui-data-left-new-pos label-gui-data)))))
      (asset:assoc-set-for-each
       (gui-model-state-snips gui-model-state)
       (lambda (label snips-groups)
         (asset:assoc-set-for-each
          (snips-groups-by-type snips-groups)
          (lambda (type snip-group)
            (set-snip-group-left-new-pos! snip-group (move-pos (snip-group-left-new-pos snip-group)))))))
      cst:void))
  
  
  ; EVENTS
  ; simulates user's actions. Of course all the snips and arrows will be deleted as soon
  ; a user modifies the definition window, but we still neep to update the snip
  ; positions so deleting will occur correctly.
  ; gui-model-state exact-non-negative-integer exact-non-negative-integer -> void
  (define (after-insert gui-model-state start len)
    (move-poss gui-model-state start len +))
  
  ; gui-model-state exact-non-negative-integer exact-non-negative-integer -> void
  (define (after-delete gui-model-state start len)
    (move-poss gui-model-state start len -))
  
  
  ; ARROWS
  ; gui-model-state label label boolean -> void
  (define (add-arrow gui-model-state start-label end-label tacked?)
    (let* ([displayed-arrows (gui-model-state-arrows gui-model-state)]
           [new-arrow (make-arrow start-label end-label tacked?)]
           [start-label-gui-data (asset:assoc-set-get displayed-arrows start-label cst:thunk-false)]
           [end-label-gui-data (asset:assoc-set-get displayed-arrows end-label cst:thunk-false)]
           [get-mzscheme-position-from-label
            (gui-model-state-get-mzscheme-position-from-label gui-model-state)])
      (if (and start-label-gui-data end-label-gui-data)
          ; check if arrow already exists
          (let* ([starting-arrows-set (label-gui-data-starting-arrows start-label-gui-data)]
                 [ending-arrows-set (label-gui-data-ending-arrows end-label-gui-data)]
                 [arrow-set (set:set-filter starting-arrows-set
                                            (lambda (arrow)
                                              (eq? end-label (arrow-end-label arrow))))])
            (if (set:set-empty? arrow-set)
                ; the arrow doesn't already exist, but the labels both already have had some arrows,
                ; so we just add the new one.
                (begin
                  (set:set-set starting-arrows-set new-arrow)
                  (set:set-set ending-arrows-set new-arrow)
                  cst:void)
                ; the arrow already exists
                (if tacked?
                    ; just make sure the arrow is tacked, possibly transforming an
                    ; untacked arrow into a tacked one
                    (set-arrow-tacked?! (car (set:set-map arrow-set cst:id)) tacked?)
                    ; the arrow is already there, either tacked or untacked, but we
                    ; just want to add the same arrow untacked. Since tacked has priority
                    ; over untacked, we do nothing.
                    cst:void)
                ))
          ; arrow doesn't exist yet and one or both of the labels never had
          ; arrows associated with it before.
          (begin
            (if start-label-gui-data
                (set:set-set (label-gui-data-starting-arrows start-label-gui-data) new-arrow)
                (let ([new-pos (old-pos->new-pos gui-model-state
                                                 (mzscheme-pos->drscheme-pos
                                                  (get-mzscheme-position-from-label start-label)))])
                  (asset:assoc-set-set displayed-arrows
                                       start-label
                                       (make-label-gui-data new-pos
                                                            (set:set-set (set:make-set) new-arrow)
                                                            (set:make-set)))))
            (if end-label-gui-data
                (set:set-set (label-gui-data-ending-arrows end-label-gui-data) new-arrow)
                (let ([new-pos (old-pos->new-pos gui-model-state
                                                 (mzscheme-pos->drscheme-pos
                                                  (get-mzscheme-position-from-label end-label)))])
                  (asset:assoc-set-set displayed-arrows
                                       end-label
                                       (make-label-gui-data new-pos
                                                            (set:make-set)
                                                            (set:set-set (set:make-set) new-arrow)))))
            cst:void))))
  
  ; gui-model-state label (union symbol boolean) boolean -> void
  ; remove arrows starting at given label AND arrows ending at same given label
  ; Note that assoc-set-get will fail if we try to remove non-existant arrows...
  (define (remove-arrows gui-model-state start-label tacked? exn?)
    (let* ([displayed-arrows (gui-model-state-arrows gui-model-state)]
           [start-label-gui-data
            (if exn?
                (asset:assoc-set-get displayed-arrows start-label)
                (asset:assoc-set-get displayed-arrows start-label cst:thunk-false))])
      ; at this point, if the key was not found, either exn? was true and an exception
      ; was raised, or it was false and start-label-gui-data is false
      (when start-label-gui-data
        (remove-both-ends gui-model-state
                          (label-gui-data-starting-arrows start-label-gui-data)
                          tacked?
                          arrow-end-label
                          label-gui-data-ending-arrows)
        (remove-both-ends gui-model-state
                          (label-gui-data-ending-arrows start-label-gui-data)
                          tacked?
                          arrow-start-label
                          label-gui-data-starting-arrows)))
    cst:void)
  
  ; gui-model-state (setof arrow) (union symbol boolean)
  ; (arrow -> label) (label-gui-data -> (setof arrow))
  ; -> (setof arrow)
  ; remove arrows starting at given label OR arrows ending at given
  ; label (depending on selectors/settors)
  (define (remove-both-ends gui-model-state set tacked? select-other-end-label select-other-end-set)
    (if (eq? tacked? 'all)
        ; remove all the other ends and reset this end
        ; we could do without this case and use the set-filter way used in the "else" case
        ; of this if, but doing it that way here is faster because we don't bother testing
        ; and removing each arrow from the set one by one, we just reset the whole thing.
        (begin
          (set:set-for-each set (lambda (arrow)
                                  (remove-other-end gui-model-state arrow select-other-end-label select-other-end-set)))
          (set:set-reset set))
        ; remove other end while filtering this set
        (set:set-filter set
                        (lambda (arrow)
                          (if (eq? tacked? (arrow-tacked? arrow))
                              (begin
                                (remove-other-end gui-model-state arrow select-other-end-label select-other-end-set)
                                #f)
                              #t))
                        'same)))
  
  ; gui-model-state arrow (arrow -> label) (label-gui-data -> (setof arrow))
  ; -> (setof arrow)
  ; removes one arrow structure reference corresponding to the remote end of the arrow we
  ; are removing in remove-both-ends above.
  (define (remove-other-end gui-model-state arrow select-other-end-label select-other-end-set)
    (set:set-remove (select-other-end-set (asset:assoc-set-get (gui-model-state-arrows gui-model-state)
                                                               (select-other-end-label arrow)))
                    arrow))
  
  ; gui-model-state -> void
  (define (remove-all-arrows gui-model-state)
    (set-gui-model-state-arrows! gui-model-state (asset:make-assoc-set)))
  
  ; gui-model-state
  ; (non-negative-exact-integer non-negative-exact-integer non-negative-exact-integer non-negative-exact-integer boolean -> void)
  ; -> void
  ; applies f to each arrow. The args for f are: the left new-pos of the start label, the
  ; left new-pos of the end label, the corresponding spans, and whether the arrow is tacked
  ; or not.
  (define (for-each-arrow gui-model-state f)
    (let ([get-span-from-label (gui-model-state-get-span-from-label gui-model-state)]
          [all-arrows (gui-model-state-arrows gui-model-state)])
      (asset:assoc-set-for-each
       all-arrows
       (lambda (start-label start-label-gui-data)
         (let ([start-label-pos (label-gui-data-left-new-pos start-label-gui-data)]
               [start-label-span (get-span-from-label start-label)]
               [starting-arrows-set
                (label-gui-data-starting-arrows start-label-gui-data)])
           (set:set-for-each starting-arrows-set
                             (lambda (arrow)
                               (let ([end-label (arrow-end-label arrow)])
                                 (f start-label-pos
                                    (label-gui-data-left-new-pos (asset:assoc-set-get all-arrows end-label))
                                    start-label-span
                                    (get-span-from-label end-label)
                                    (arrow-tacked? arrow))))))))))
  
  ; (label-gui-data -> (setof arrow)) -> (gui-model-state label -> non-negative-exact-integer)
  ; Given an arrow set selector (to select the arrows that start or end at the given label),
  ; returns a function that will count how many of these arrows are tacked
  (define (get-relative-tacked-arrows arrow-set-selector)
    (lambda (gui-model-state label)
      (let* ([displayed-arrows (gui-model-state-arrows gui-model-state)]
             [label-gui-data (asset:assoc-set-get displayed-arrows label cst:thunk-false)])
        (if label-gui-data
            (set:set-cardinality (set:set-filter (arrow-set-selector label-gui-data) arrow-tacked?))
            0))))
  
  ; gui-model-state label -> non-negative-exact-integer
  ; actual tacked arrows couting functions
  (define get-parents-tacked-arrows (get-relative-tacked-arrows label-gui-data-ending-arrows))
  (define get-children-tacked-arrows (get-relative-tacked-arrows label-gui-data-starting-arrows))
  
  
  ; SNIPS
  ; gui-model-state label symbol -> boolean
  (define (label-has-snips-of-this-type? gui-model-state label type)
    (let* ([inserted-snips (gui-model-state-snips gui-model-state)]
           [snips-groups (asset:assoc-set-get inserted-snips label cst:thunk-false)])
      (if snips-groups
          (asset:assoc-set-in? (snips-groups-by-type snips-groups) type)
          #f)))
  
  ; (assoc-setof symbol (setof exact-non-negative-integer)) symbol (listof symbol) -> non-negative-exact-integer
  (define (get-number-of-snips-on-right-from-type snips-groups-by-type type type-list)
    (let ([snip-types-on-right 
           (let ([types (memq type type-list)])
             (if types
                 types
                 (argexn:raise-arg-mismatch-exn "get-number-of-snips-on-right-from-type"
                                                (format "(list ... ~a ...)" type)
                                                type-list)))])
      (let loop ([snip-types-on-right (cdr snip-types-on-right)]
                 [number-of-snips-on-right 0])
        (if (null? snip-types-on-right)
            number-of-snips-on-right
            (loop (cdr snip-types-on-right)
                  (+ number-of-snips-on-right
                     (let ([snip-group (asset:assoc-set-get snips-groups-by-type (car snip-types-on-right) cst:thunk-false)])
                       (if snip-group
                           (snip-group-size snip-group)
                           0))))))))
  
  ; gui-model-state label symbol non-negative-exact-integer -> non-negative-exact-integer
  ; updates state (move existing snips and add new ones) and returns the position where
  ; the snips should be inserted in the text
  (define (add-snips gui-model-state label type number-of-snips)
    (let* ([inserted-snips (gui-model-state-snips gui-model-state)]
           [snips-groups (asset:assoc-set-get inserted-snips label cst:thunk-false)]
           [get-mzscheme-position-from-label
            (gui-model-state-get-mzscheme-position-from-label gui-model-state)]
           [label-starting-pos
            (old-pos->new-pos gui-model-state
                              (mzscheme-pos->drscheme-pos
                               (get-mzscheme-position-from-label label)))])
      (if snips-groups
          ; the label has already some snips attached to it.
          (let* ([snips-groups-by-type (snips-groups-by-type snips-groups)]
                 [insertion-starting-pos
                  (- label-starting-pos
                     (get-number-of-snips-on-right-from-type
                      snips-groups-by-type type (gui-model-state-type-list gui-model-state)))])
            (move-poss gui-model-state insertion-starting-pos number-of-snips +)
            (if (asset:assoc-set-in? snips-groups-by-type type)
                ; type already present, but, for a given label and type, we can have only one
                ; group of snips
                (error 'add-snips gui-model-state
                       "snips-and-arrows internal error; label ~a has already a snip group of type ~a"
                       label type)
                ; new snip type for this label
                (asset:assoc-set-set snips-groups-by-type type (make-snip-group insertion-starting-pos number-of-snips)))
            insertion-starting-pos)
          ; create new snips-groups associative set for that label
          (begin
            (move-poss gui-model-state label-starting-pos number-of-snips +)
            (asset:assoc-set-set
             inserted-snips
             label
             (make-snips-groups
              (asset:assoc-set-set (asset:make-assoc-set)
                                   type (make-snip-group label-starting-pos number-of-snips))))
            label-starting-pos))))
  
  ; gui-model-state label symbol -> (value non-negative-exact-integer non-negative-exact-integer)
  ; removes all snips for a given label and type, move remaining snips, and returns the interval
  ; to delete in the editor
  (define (remove-inserted-snips gui-model-state label type)
    (let ([snips-groups (asset:assoc-set-get (gui-model-state-snips gui-model-state) label cst:thunk-false)])
      (if snips-groups
          (let* ([snips-groups-by-type (snips-groups-by-type snips-groups)]
                 [snip-group (asset:assoc-set-get snips-groups-by-type type cst:thunk-false)])
            (if snip-group
                (let* ([size (snip-group-size snip-group)]
                       [get-mzscheme-position-from-label
                        (gui-model-state-get-mzscheme-position-from-label gui-model-state)]
                       [label-starting-pos
                        (old-pos->new-pos gui-model-state
                                          (mzscheme-pos->drscheme-pos
                                           (get-mzscheme-position-from-label label)))]
                       [deletion-ending-pos
                        (- label-starting-pos
                           (get-number-of-snips-on-right-from-type
                            snips-groups-by-type type (gui-model-state-type-list gui-model-state)))])
                  (asset:assoc-set-remove snips-groups-by-type type)
                  (move-poss gui-model-state deletion-ending-pos size -)
                  (values (- deletion-ending-pos size) deletion-ending-pos)
                  )
                ; label has no snips of this type associated with it, delete nothing
                ; this should normally not happen, because no menu should be available for this
                (error 'remove-inserted-snips
                       "snips-and-arrows internal error; label ~a has no snip group of type ~a"
                       label type)))
          ; label has no snips at all associated with it, delete nothing
          ; this should normally not happen, because no menu should be available for this
          (error 'remove-inserted-snips
                 "snips-and-arrows internal error; label ~a has no snip group at all, let alone of type ~a"
                 label type))))
  
  ; gui-model-state -> (listof (cons non-negative-exact-integer non-negative-exact-integer))
  ; returns the position of all the existing snip groups
  (define (remove-all-snips gui-model-state)
    (let ([all-snip-groups (asset:assoc-set-fold
                            (gui-model-state-snips gui-model-state)
                            (lambda (label snips-groups acc)
                              (asset:assoc-set-fold
                               (snips-groups-by-type snips-groups)
                               (lambda (type snip-group acc)
                                 (cons (cons (snip-group-left-new-pos snip-group)
                                             (snip-group-size snip-group))
                                       acc))
                               acc))
                            '())])
      (set-gui-model-state-snips! gui-model-state (asset:make-assoc-set))
      all-snip-groups))
  
  )
