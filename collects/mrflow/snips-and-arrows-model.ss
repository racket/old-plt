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
;
; This whole module can only deal with snips that are on the left of the label (see
; new-pos->old-pos and old-pos->new-pos for example).

(module snips-and-arrows-model mzscheme
  (require
   (prefix cst: "constants.ss")
   ;"set-list.ss"
   "set-hash.ss"
   ;"assoc-set-list.ss"
   "assoc-set-hash.ss"
   )
  
  (provide
   make-gui-model-state ; (label -> top) (label -> non-negative-exact-integer) (label -> non-negative-exact-integer) (listof symbol) -> gui-model-state
   reset-gui-model-state ; gui-model-state -> void
   
   (rename get-related-label-from-drscheme-new-pos-and-source
           get-related-label-from-drscheme-pos-and-source) ; gui-model-state non-negative-exact-integer top -> (union label #f)
   (rename gui-model-state-get-span-from-label
           make-get-span-from-label-from-model-state) ; gui-model-state -> (label -> non-negative-exact-integer)
   
   for-each-source ; gui-model-state (top -> void) -> void
   for-each-modified-source! ; gui-model-state (top -> void) -> void
   
   register-label-with-gui ; gui-model-state label -> void
   (rename get-new-pos-from-label get-position-from-label) ; gui-model-state label -> exact-non-negative-integer
   user-resize-label ; gui-model-state label exact-integer -> (values non-negative-exact-integer non-negative-exact-integer non-negative-exact-integer)
   for-each-label ; gui-model-state (label -> void) -> void
   
   after-user-action ; gui-model-state exact-non-negative-integer exact-non-negative-integer -> void
   
   add-arrow ; gui-model-state label label boolean -> void
   remove-arrows ; gui-model-state label (union symbol boolean) boolean -> void
   for-each-arrow ; gui-model-state (non-negative-exact-integer non-negative-exact-integer non-negative-exact-integer non-negative-exact-integer top top boolean -> void) -> void
   get-parents-tacked-arrows ; gui-model-state label -> non-negative-exact-integer
   get-children-tacked-arrows ; gui-model-state label -> non-negative-exact-integer
   
   label-has-snips-of-this-type? ; gui-model-state label symbol -> boolean
   add-snips ; gui-model-state label symbol non-negative-exact-integer -> non-negative-exact-integer
   remove-inserted-snips ; gui-model-state label symbol -> (value non-negative-exact-integer non-negative-exact-integer)
   
   remove-all-snips-and-arrows ; gui-model-state -> (assoc-setof top (listof (cons non-negative-exact-integer non-negative-exact-integer)))
   )
  
  ; DATA STRUCTURES
  ; label label boolean
  (define-struct arrow (start-label end-label tacked?))
  
  ; exact-non-negative-integer
  (define-struct snip-group (size))
  
  ; position exact-integer exact-non-negative-integer (assoc-setof symbol snip-group) (setof arrow) (setof arrow)
  ; We could recompute left-new-pos on the fly (from the MzScheme
  ; pos from the label itself and old-pos->new-pos) each time we needed to repaint,
  ; but in practice we repaint much more often then we add snips, so we keep the pos
  ; here as a cache which is computed once from scratch when we add the label to
  ; displayed-arrows and which is then just updated each time we add a new snip.
  ; Likewise, total-number-of-snips could be recomputed on the fly from snip-groups-by-type,
  ; but is used as a cache to speed up old-pos->new-pos and new-pos->old-pos, which are used
  ; pretty often.
  ; Note that the data structure for a single arrow will be shared between two
  ; label-gui-data structures: it will appear once in the "starting-arrows"
  ; set for its start label, and once in the "ending-arrows" set for its end label.
  ; We need this because we need to be able to click at the end of an arrow and
  ; remove it if necessary.
  (define-struct label-gui-data (left-new-pos
                                 span-change
                                 total-number-of-snips
                                 snip-groups-by-type
                                 starting-arrows
                                 ending-arrows))
  
  ; (assoc-setof label label-gui-data) (assoc-setof non-negative-exact-integer label)
  ; Note that, while several labels might have a given position (due to macros), only
  ; one label can be associated with that position here.
  (define-struct source-gui-data (label-gui-data-by-label
                                  labels-by-mzscheme-position))
  
  (define-struct gui-model-state (; (assoc-setof source source-gui-data)
                                  source-gui-data-by-source
                                  ; (label -> top)
                                  get-source-from-label
                                  ; (label -> non-negative-exact-integer)
                                  get-mzscheme-position-from-label
                                  ; (label -> non-negative-exact-integer)
                                  get-original-span-from-label
                                  ; (label -> non-negative-exact-integer)
                                  get-span-from-label
                                  ; (listof symbol)
                                  snip-type-list
                                  ; (setof top)
                                  sources-recently-modified
                                  ))
  
  ; (label -> top)
  ; (label -> non-negative-exact-integer)
  ; (label -> non-negative-exact-integer)
  ; (listof symbol)
  ; -> gui-model-state
  (set! make-gui-model-state
        (let ([real-make-gui-model-state make-gui-model-state])
          (lambda (get-source-from-label
                   get-mzscheme-position-from-label
                   get-span-from-label
                   snip-type-list)
            (let ([source-gui-data-by-source (assoc-set-make)])
              (real-make-gui-model-state
               source-gui-data-by-source
               get-source-from-label
               get-mzscheme-position-from-label
               get-span-from-label
               (lambda (label)
                 (let* ([span (get-span-from-label label)]
                        [source-gui-data
                         (assoc-set-get source-gui-data-by-source (get-source-from-label label))]
                        [label-gui-data
                         (assoc-set-get (source-gui-data-label-gui-data-by-label source-gui-data)
                                        label cst:thunk-false)])
                   (if label-gui-data
                       (+ span (label-gui-data-span-change label-gui-data))
                       span)))
               snip-type-list
               (set-make))))))
  
  ; gui-model-state -> void
  (define (reset-gui-model-state gui-model-state)
    (assoc-set-reset (gui-model-state-source-gui-data-by-source gui-model-state))
    (set-reset (gui-model-state-sources-recently-modified gui-model-state)))
  
  
  ; DRSCHEME / MZSCHEME CONVERSIONS
  ; non-negative-exact-integer -> non-negative-exact-integer
  (define drscheme-pos->mzscheme-pos add1)
  
  ; non-negative-exact-integer -> non-negative-exact-integer
  (define mzscheme-pos->drscheme-pos sub1)
  
  
  ; MISC
  ; gui-model-state (assoc-setof label label-gui-data) label exact-integer (-> (setof arrow)) (-> (setof arrow)) -> exact-non-negative-integer
  (define (associate-label-gui-data-with-label gui-model-state label-gui-data-by-label label change make-starting-arrows-set make-ending-arrows-set)
    (let ([left-new-pos (get-new-pos-from-label gui-model-state label)])
      (assoc-set-set label-gui-data-by-label
                     label
                     (make-label-gui-data left-new-pos
                                          change
                                          0
                                          (assoc-set-make)
                                          (make-starting-arrows-set)
                                          (make-ending-arrows-set)))
      left-new-pos))
  
  
  ; SOURCES
  ; gui-model-state (top -> void) -> void
  ; applies f to each source
  (define (for-each-source gui-model-state f)
    (assoc-set-for-each (gui-model-state-source-gui-data-by-source gui-model-state)
                        (lambda (source source-gui-data)
                          (f source)))
    cst:void)
  
  ; gui-model-state (top -> void) -> void
  ; applies f to each modified source, and forgets source was modified
  (define (for-each-modified-source! gui-model-state f)
    (let ([modified-sources-set (gui-model-state-sources-recently-modified gui-model-state)])
      (set-for-each modified-sources-set f)
      (set-reset modified-sources-set)
      cst:void))
  
  
  ; LABELS
  ; gui-model-state label -> exact-non-negative-integer
  ; returns the left position of the expression.  The computation is done from scratch,
  ; so only use this function if the position hasn't been yet cached in the label's gui data.
  (define (get-new-pos-from-label gui-model-state label)
    (old-pos->new-pos
     gui-model-state
     (mzscheme-pos->drscheme-pos
      ((gui-model-state-get-mzscheme-position-from-label gui-model-state) label))
     ((gui-model-state-get-source-from-label gui-model-state) label)))
  
  ; gui-model-state label -> void
  ; we register the source of the label, and the label by its position,
  ; but we don't associate any label-gui-data with it yet, to save memory.
  ; We'll associate some label-gui-data with it on the fly, as needed (when
  ; needing to remember some arrows or snips for that label, not before).
  (define (register-label-with-gui gui-model-state label)
    (let* ([source-gui-data-by-source
            (gui-model-state-source-gui-data-by-source gui-model-state)]
           [source
            ((gui-model-state-get-source-from-label gui-model-state) label)]
           [source-gui-data
            (assoc-set-get source-gui-data-by-source source cst:thunk-false)]
           [mzscheme-pos
            ((gui-model-state-get-mzscheme-position-from-label gui-model-state) label)])
      (if source-gui-data
          (let ([labels-by-mzscheme-position
                 (source-gui-data-labels-by-mzscheme-position source-gui-data)])
            (if (assoc-set-in? labels-by-mzscheme-position mzscheme-pos)
                (error 'register-label-with-model
                       "a label is already registered for position ~a" mzscheme-pos)
                (begin
                  (assoc-set-set labels-by-mzscheme-position mzscheme-pos label)
                  cst:void)))
          (begin
            (assoc-set-set source-gui-data-by-source source
                           (make-source-gui-data (assoc-set-make)
                                                 (assoc-set-set (assoc-set-make)
                                                                mzscheme-pos label)))
            cst:void))))
  
  ; gui-model-state label exact-integer -> (values non-negative-exact-integer non-negative-exact-integer non-negative-exact-integer)
  ; modify the span of a label and move snips on the right, returning the interval
  ; that has to be deleted and the new interval that has to be colored (for a total of
  ; three numbes, since both intervals start at the same position)
  (define (user-resize-label gui-model-state label new-span)
    (let* ([source ((gui-model-state-get-source-from-label gui-model-state) label)]
           [source-gui-data
            (assoc-set-get (gui-model-state-source-gui-data-by-source gui-model-state) source)]
           [old-span ((gui-model-state-get-span-from-label gui-model-state) label)]
           [change (- new-span old-span)]
           [left-new-pos
            (let* ([label-gui-data-by-label
                    (source-gui-data-label-gui-data-by-label source-gui-data)]
                   [label-gui-data
                    (assoc-set-get label-gui-data-by-label label cst:thunk-false)])
              (if label-gui-data
                  (begin
                    (set-label-gui-data-span-change!
                     label-gui-data
                     (+ change (label-gui-data-span-change label-gui-data)))
                    (label-gui-data-left-new-pos label-gui-data))
                  (associate-label-gui-data-with-label gui-model-state label-gui-data-by-label
                                                       label change set-make set-make)))])
      (move-poss gui-model-state source left-new-pos change + >)
      (values left-new-pos (+ left-new-pos old-span) (+ left-new-pos new-span))))
  
  ; gui-model-state (label -> void) -> void
  ; apply f to all registered labels
  (define (for-each-label gui-model-state f)
    (assoc-set-for-each
     (gui-model-state-source-gui-data-by-source gui-model-state)
     (lambda (source source-gui-data)
       (assoc-set-for-each
        (source-gui-data-labels-by-mzscheme-position source-gui-data)
        (lambda (mzscheme-pos label)
          (f label))))))
  
  
  ; POS AND SOURCE TO LABEL CONVERSIONS
  ; gui-model-state non-negative-exact-integer top -> (union label #f)
  ; finds the label corresponding to a given new-pos. Note that, because we
  ; use get-related-label-from-drscheme-old-pos-and-source for that purpose, we ignore
  ; the presence of snips, so the user will be able to click on the snips
  ; for that label and still get the label (in addition of being able to get
  ; the label by clicking on the term itself).
  (define (get-related-label-from-drscheme-new-pos-and-source gui-model-state new-pos source)
    (get-related-label-from-drscheme-old-pos-and-source
     gui-model-state
     (new-pos->old-pos gui-model-state new-pos source)
     source))
  
  ; gui-model-state non-negative-exact-integer top -> (union label #f)
  ; we loop down starting from old-pos, until we find a label. Then we have to check
  ; that the original old-pos falls within the original span of that label.
  (define (get-related-label-from-drscheme-old-pos-and-source gui-model-state old-pos source)
    (let ([get-original-span-from-label
           (gui-model-state-get-original-span-from-label gui-model-state)]
          [source-gui-data
           (assoc-set-get (gui-model-state-source-gui-data-by-source gui-model-state)
                          source cst:thunk-false)])
      (if source-gui-data
          (let ([labels-by-mzscheme-position (source-gui-data-labels-by-mzscheme-position source-gui-data)]
                [starting-mzscheme-pos (drscheme-pos->mzscheme-pos old-pos)])
            (let loop ([current-mzscheme-pos starting-mzscheme-pos])
              (if (> 0 current-mzscheme-pos)
                  #f
                  (let ([label (assoc-set-get labels-by-mzscheme-position current-mzscheme-pos cst:thunk-false)])
                    (if label
                        ; Note that if the label's span is too small, we stop looping.
                        ; This means that in an expression like (abc def), if the mouse
                        ; pointer points at the space character, #f will be returned,
                        ; not the label for the whole expression.
                        (if (< (- starting-mzscheme-pos current-mzscheme-pos)
                               (get-original-span-from-label label))
                            label
                            #f)
                        (loop (sub1 current-mzscheme-pos)))))))
          #f)))
  
  
  ; OLD-POS / NEW-POS CONVERSIONS
  ; gui-model-state exact-non-negative-integer top -> exact-non-negative-integer
  ; converts an old position (before insertion of any snip) to a new position
  ; (after insertion of all the currently inserted snips).
  ; Note: the test is "<=", which means the new position is to the right of all
  ; the current snips that have positions corresponding to the same old position.
  (define (old-pos->new-pos gui-model-state old-pos source)
    (let ([new-pos old-pos]
          [get-mzscheme-position-from-label
           (gui-model-state-get-mzscheme-position-from-label gui-model-state)]
          [get-original-span-from-label
           (gui-model-state-get-original-span-from-label gui-model-state)]
          [get-span-from-label
           (gui-model-state-get-span-from-label gui-model-state)]
          [source-gui-data
           (assoc-set-get (gui-model-state-source-gui-data-by-source gui-model-state)
                          source cst:thunk-false)])
      (when source-gui-data
        (assoc-set-for-each
         (source-gui-data-label-gui-data-by-label source-gui-data)
         (lambda (label label-gui-data)
           (let ([label-left-old-pos (mzscheme-pos->drscheme-pos (get-mzscheme-position-from-label label))])
             (cond
               ; the order of the clauses is important here
               ; old-pos is on the right of the original expression represented by the label
               [(<= (+ label-left-old-pos (get-original-span-from-label label)) old-pos)
                (set! new-pos (+ new-pos
                                 (label-gui-data-span-change label-gui-data)
                                 (label-gui-data-total-number-of-snips label-gui-data)))]
               ; old-pos is somewhere in the middle of the expression represented by the label
               ; then we have to take care of the case when the current expression is smaller than
               ; the original expression (because an identifier was changed)
               [(<= label-left-old-pos old-pos)
                (if (<= (+ label-left-old-pos (get-span-from-label label)) old-pos)
                    ; expression has shrinked, and old-pos was in the part that disappeared,
                    ; so we make sure the new-pos is at least within the current expression
                    ; by acting as if old-pos were label-left-old-pos (i.e. moving old-pos
                    ; to the left end of the expression). Note that this makes old-pos->new-pos
                    ; not bijective anymore.
                    (set! new-pos (+ new-pos
                                     (- label-left-old-pos old-pos)
                                     (label-gui-data-total-number-of-snips label-gui-data)))
                    ; either expression has not shrinked, or if it has, old-pos is sufficiently
                    ; in the left part that we don't have to worry about it
                    (set! new-pos (+ new-pos
                                     (label-gui-data-total-number-of-snips label-gui-data))))]
               ; old-pos is on the left of the expression => do nothing
               )))))
      new-pos))
  
  ; gui-model-state exact-non-negative-integer top -> exact-non-negative-integer
  ; Note: the test is "<", because there might a snip that has the exact same
  ; position as new-pos, so, since a snip at position n is shown graphically
  ; between position n and n+1, we don't want to take that snip into account
  ; (i.e. that snip is on the right of the cursor or mouse pointer, not on the
  ; left).
  ; Note also that we have to be carefull: in old-pos->new-pos we add all the snips
  ; to the new-pos when the label has an old-pos to the left of or at the cursor.
  ; But here the cursor might be between two snips. So we have to consider each snip
  ; separately, we can't consider them group by group anymore.
  (define (new-pos->old-pos gui-model-state new-pos source)
    (let ([old-pos new-pos]
          [get-original-span-from-label
           (gui-model-state-get-original-span-from-label gui-model-state)]
          [get-span-from-label
           (gui-model-state-get-span-from-label gui-model-state)]
          [source-gui-data
           (assoc-set-get (gui-model-state-source-gui-data-by-source gui-model-state)
                          source cst:thunk-false)])
      (when source-gui-data
        (assoc-set-for-each
         (source-gui-data-label-gui-data-by-label source-gui-data)
         (lambda (label label-gui-data)
           (let ([label-left-new-pos (label-gui-data-left-new-pos label-gui-data)]
                 [total-number-of-snips (label-gui-data-total-number-of-snips label-gui-data)])
             (cond
               ; the order of the clauses is important here
               ; new-pos is on the right of the expression represented by the label
               [(<= (+ label-left-new-pos (get-span-from-label label)) new-pos)
                (set! old-pos (- old-pos
                                 (label-gui-data-span-change label-gui-data)
                                 (label-gui-data-total-number-of-snips label-gui-data)))]
               ; new-pos is somewhere in the middle of the expression represented by the label
               ; then we have to take care of the case when the current expression is bigger than
               ; the original expression (because an identifier was changed)
               [(<= label-left-new-pos new-pos)
                (if (<= (+ label-left-new-pos (get-original-span-from-label label)) new-pos)
                    ; expression has expanded, and new-pos was in the part that was added,
                    ; so we make sure the old-pos is at least within the current expression
                    ; by acting as if new-pos were label-left-new-pos (i.e. moving new-pos
                    ; to the left end of the expression). Note that this makes new-pos->old-pos
                    ; not bijective anymore.
                    (set! old-pos (- old-pos
                                     (- new-pos label-left-new-pos)
                                     (label-gui-data-total-number-of-snips label-gui-data)))
                    ; either expression has not expanded, or if it has, new-pos is sufficiently
                    ; in the left part that we don't have to worry about it
                    (set! old-pos (- old-pos
                                     (label-gui-data-total-number-of-snips label-gui-data))))]
               ; new-pos is on the left of the expression but in the middle of the snips
               ; at that point we could either loop over the snips groups one by one and test
               ; them using their left-new-pos, or we can directly compute the total number of
               ; snips on the left of new-pos using the label's left-new-pos and
               ; total-number-of-snips. Since the second method is easier, we do it that way.
               [(<= (- label-left-new-pos total-number-of-snips) new-pos)
                (set! old-pos (- old-pos
                                 (- total-number-of-snips (- label-left-new-pos new-pos))))]
               ; new-pos is on the left of the expression and the snips => do nothing
               )))))
      old-pos))
  
  ; gui-model-state top exact-non-negative-integer exact-integer
  ; (exact-non-negative-integer exact-integer -> exact-integer)
  ; (exact-non-negative-integer exact-integer -> boolean) -> void
  ; moves all snips and arrows that are after start, by len. start is a new position (i.e. after
  ; insertion of snips). We need to do all that so that old-pos->new-pos and new-pos->old-pos
  ; and the arrow display keep working correctly when we add new snips in the middle of others.
  ; We add the source to the set of recently modifed sources because moving stuff might change
  ; tha arrows for that source.
  (define (move-poss gui-model-state source start len add comp)
    (set-set (gui-model-state-sources-recently-modified gui-model-state) source #f)
    (let ([move-pos (lambda (pos) (if (comp pos start) (add pos len) pos))]
          [source-gui-data
           (assoc-set-get (gui-model-state-source-gui-data-by-source gui-model-state) source)])
      (assoc-set-for-each
       (source-gui-data-label-gui-data-by-label source-gui-data)
       (lambda (label label-gui-data)
         (set-label-gui-data-left-new-pos!
          label-gui-data (move-pos (label-gui-data-left-new-pos label-gui-data))))))
    cst:void)
  
  
  ; EVENTS
  ; gui-model-state exact-non-negative-integer exact-non-negative-integer -> void
  ; simulates user's actions. Of course all the snips and arrows will be deleted as soon
  ; a user modifies the definition window, but we still neep to update the snip
  ; positions so deleting will occur correctly.
  (define (after-user-action gui-model-state source start len)
    (when (assoc-set-in? (gui-model-state-source-gui-data-by-source gui-model-state) source)
      (move-poss gui-model-state source start len + >=)))
  
  
  ; ARROWS
  ; gui-model-state label label boolean -> void
  ; add one arrow going from start-label to end-label, duh.
  (define (add-arrow gui-model-state start-label end-label tacked?)
    (let ([new-arrow (make-arrow start-label end-label tacked?)])
      (add-one-arrow-end gui-model-state
                         new-arrow
                         start-label
                         end-label
                         arrow-end-label
                         label-gui-data-starting-arrows
                         (lambda () (set-set (set-make) new-arrow))
                         set-make)
      (add-one-arrow-end gui-model-state
                         new-arrow
                         end-label
                         start-label
                         arrow-start-label
                         label-gui-data-ending-arrows
                         set-make
                         (lambda () (set-set (set-make) new-arrow)))))
  
  ; gui-model-state arrow label label (arrow -> label) (label-gui-data -> (setof arrow)
  ; (-> (setof arrow)) (-> (setof arrow)) -> void
  ; adds arrow structure to the label's gui data, for one end of the arrow
  (define (add-one-arrow-end gui-model-state new-arrow this-end-label other-end-label
                             arrow-other-end-label-selector label-gui-data-this-end-arrow-set-selector
                             make-starting-arrow-set make-ending-arrow-set)
    (let* ([this-end-source
            ((gui-model-state-get-source-from-label gui-model-state) this-end-label)]
           [this-end-source-gui-data
            (assoc-set-get (gui-model-state-source-gui-data-by-source gui-model-state) this-end-source)]
           [this-end-label-gui-data-by-label
            (source-gui-data-label-gui-data-by-label this-end-source-gui-data)]
           [this-end-label-gui-data
            (assoc-set-get this-end-label-gui-data-by-label this-end-label cst:thunk-false)])
      (set-set (gui-model-state-sources-recently-modified gui-model-state) this-end-source #f)
      (if this-end-label-gui-data
          (let* ([this-end-arrow-set
                  (label-gui-data-this-end-arrow-set-selector this-end-label-gui-data)]
                 [same-arrow-set (set-filter this-end-arrow-set
                                             (lambda (arrow)
                                               (eq? other-end-label
                                                    (arrow-other-end-label-selector arrow))))])
            (if (set-empty? same-arrow-set)
                ; the arrow doesn't already exist, so add the arrow to the start set
                (set-set this-end-arrow-set new-arrow)
                ; the arrow already exists
                (let* ([new-arrow-tacked? (arrow-tacked? new-arrow)]
                       [old-arrow (if (= (set-cardinality same-arrow-set) 1)
                                      (car (set-map same-arrow-set cst:id))
                                      (error 'add-one-arrow-end "duplicate arrows"))]
                       [old-arrow-tacked? (arrow-tacked? old-arrow)])
                  (if new-arrow-tacked?
                      (if old-arrow-tacked?
                          ; can't tack an already tacked arrow
                          (error 'add-one-arrow-end "tacked arrow already exists")
                          ; forcibly tack existing arrow
                          (set-arrow-tacked?! old-arrow #t))
                      (if old-arrow-tacked?
                          ; trying to add an untacked arrow when tacked one already exists
                          ; silently ignore
                          cst:void
                          (error 'add-one-arrow-end "untacked arrow already exists"))))))
          (associate-label-gui-data-with-label gui-model-state
                                               this-end-label-gui-data-by-label
                                               this-end-label
                                               0
                                               make-starting-arrow-set
                                               make-ending-arrow-set))))
  
  ; gui-model-state label (union symbol boolean) boolean -> void
  ; remove arrows starting at given label AND arrows ending at same given label
  ; Note that assoc-set-get will fail if we try to remove non-existant arrows...
  (define (remove-arrows gui-model-state start-label tacked? exn?)
    (let* ([sources-recently-modified (gui-model-state-sources-recently-modified gui-model-state)]
           [source-gui-data-by-source
            (gui-model-state-source-gui-data-by-source gui-model-state)]
           [get-source-from-label (gui-model-state-get-source-from-label gui-model-state)]
           [source (get-source-from-label start-label)]
           [source-gui-data (assoc-set-get source-gui-data-by-source source cst:thunk-false)]
           [label-gui-data-by-label
            (source-gui-data-label-gui-data-by-label source-gui-data)]
           [start-label-gui-data
            (if exn?
                (assoc-set-get label-gui-data-by-label start-label)
                (assoc-set-get label-gui-data-by-label start-label cst:thunk-false))])
      ; at this point, if the key was not found, either exn? was true and an exception
      ; was raised, or it was false and start-label-gui-data is false
      (set-set sources-recently-modified source #f)
      (when start-label-gui-data
        (remove-both-ends source-gui-data-by-source
                          (label-gui-data-starting-arrows start-label-gui-data)
                          tacked?
                          arrow-end-label
                          label-gui-data-ending-arrows
                          get-source-from-label
                          sources-recently-modified)
        (remove-both-ends source-gui-data-by-source
                          (label-gui-data-ending-arrows start-label-gui-data)
                          tacked?
                          arrow-start-label
                          label-gui-data-starting-arrows
                          get-source-from-label
                          sources-recently-modified)))
    cst:void)
  
  ; (assoc-setof top source-gui-data) (setof arrow) (union symbol boolean)
  ; (arrow -> label) (label-gui-data -> (setof arrow))
  ; (label -> top) (setof top)
  ; -> (setof arrow)
  ; remove arrows starting at given label OR arrows ending at given
  ; label (depending on selectors/settors)
  ; the result is thrown away by the caller...
  (define (remove-both-ends source-gui-data-by-source set tacked?
                            arrow-other-end-label-selector label-gui-data-other-end-arrow-set-selector
                            get-source-from-label sources-recently-modified)
    (if (eq? tacked? 'all)
        ; remove all the other ends and reset this end
        ; we could do without this case and use the set-filter way used in the "else" case
        ; of this if, but doing it that way here is faster because we don't bother testing
        ; and removing each arrow from the set one by one, we just reset the whole thing.
        (begin
          (set-for-each set (lambda (arrow)
                              (remove-other-end source-gui-data-by-source arrow
                                                arrow-other-end-label-selector label-gui-data-other-end-arrow-set-selector
                                                get-source-from-label sources-recently-modified)))
          (set-reset set))
        ; remove other end while filtering this set
        (set-filter set
                    (lambda (arrow)
                      (if (eq? tacked? (arrow-tacked? arrow))
                          (begin
                            (remove-other-end source-gui-data-by-source arrow
                                              arrow-other-end-label-selector label-gui-data-other-end-arrow-set-selector
                                              get-source-from-label sources-recently-modified)
                            #f)
                          #t))
                    'same)))
  
  ; (assoc-setof top source-gui-data) arrow (arrow -> label) (label-gui-data -> (setof arrow))
  ; (label -> top) (setof top) -> (setof arrow)
  ; removes one arrow structure reference corresponding to the remote end of the arrow we
  ; are removing in remove-both-ends above.  We know the arrow is there, so no need to test
  ; whether label-gui-data-by-source-and-label and label-gui-data-by-label are false or not.
  ; the result is thrown away by the caller...
  (define (remove-other-end source-gui-data-by-source arrow
                            arrow-other-end-label-selector label-gui-data-other-end-arrow-set-selector
                            get-source-from-label sources-recently-modified)
    (let* ([other-end-label (arrow-other-end-label-selector arrow)]
           [other-end-source (get-source-from-label other-end-label)]
           [other-end-source-gui-data
            (assoc-set-get source-gui-data-by-source other-end-source)]
           [other-end-label-gui-data
            (assoc-set-get (source-gui-data-label-gui-data-by-label other-end-source-gui-data)
                           other-end-label)]
           [other-end-arrow-set (label-gui-data-other-end-arrow-set-selector other-end-label-gui-data)])
      (set-set sources-recently-modified other-end-source #f)
      (set-remove other-end-arrow-set arrow)))
  
  ; gui-model-state
  ; (non-negative-exact-integer non-negative-exact-integer non-negative-exact-integer non-negative-exact-integer top top boolean -> void)
  ; -> void
  ; applies f to each arrow. The args for f are: the left new-pos of the start label, the
  ; left new-pos of the end label, the corresponding spans, the corresponding sources, 
  ; and whether the arrow is tacked or not.
  ; Note: we don't mark the sources here as being recently modifed, because whatever f does,
  ; it doesn't modify gui-model-state.
  (define (for-each-arrow gui-model-state f)
    (let ([get-span-from-label (gui-model-state-get-span-from-label gui-model-state)]
          [get-source-from-label (gui-model-state-get-source-from-label gui-model-state)]
          [source-gui-data-by-source (gui-model-state-source-gui-data-by-source gui-model-state)])
      (assoc-set-for-each
       source-gui-data-by-source
       (lambda (start-source start-source-gui-data)
         (let ([label-gui-data-by-label (source-gui-data-label-gui-data-by-label start-source-gui-data)])
           (assoc-set-for-each
            label-gui-data-by-label
            (lambda (start-label start-label-gui-data)
              (set-for-each (label-gui-data-starting-arrows start-label-gui-data)
                            (lambda (arrow)
                              (let* ([end-label (arrow-end-label arrow)]
                                     [end-source (get-source-from-label end-label)]
                                     [end-source-gui-data ; the arrow exists, so this is not #f
                                      (assoc-set-get source-gui-data-by-source end-source)]
                                     [end-label-gui-data-by-label
                                      (source-gui-data-label-gui-data-by-label end-source-gui-data)]
                                     [end-label-gui-data
                                      (assoc-set-get end-label-gui-data-by-label end-label)])
                                (f (label-gui-data-left-new-pos start-label-gui-data)
                                   (label-gui-data-left-new-pos end-label-gui-data)
                                   (get-span-from-label start-label)
                                   (get-span-from-label end-label)
                                   start-source
                                   end-source
                                   (arrow-tacked? arrow))))))))))))
  
  ; (label-gui-data -> (setof arrow)) -> (gui-model-state label -> non-negative-exact-integer)
  ; Given an arrow set selector (to select the arrows that start or end at the given label),
  ; returns a function that will count how many of these arrows are tacked
  (define (get-relative-tacked-arrows arrow-set-selector)
    (lambda (gui-model-state label)
      (let ([source-gui-data
             (assoc-set-get (gui-model-state-source-gui-data-by-source gui-model-state)
                            ((gui-model-state-get-source-from-label gui-model-state) label)
                            cst:thunk-false)])
        (if source-gui-data
            (let* ([label-gui-data-by-label (source-gui-data-label-gui-data-by-label source-gui-data)]
                   [label-gui-data (assoc-set-get label-gui-data-by-label label cst:thunk-false)])
              (if label-gui-data
                  (set-cardinality (set-filter (arrow-set-selector label-gui-data) arrow-tacked?))
                  0))
            0))))
  
  ; gui-model-state label -> non-negative-exact-integer
  ; actual tacked arrows couting functions
  (define get-parents-tacked-arrows (get-relative-tacked-arrows label-gui-data-ending-arrows))
  (define get-children-tacked-arrows (get-relative-tacked-arrows label-gui-data-starting-arrows))
  
  
  ; SNIPS
  ; gui-model-state label symbol -> boolean
  ; does the label have snips of a given type currently displayed by the gui?
  (define (label-has-snips-of-this-type? gui-model-state label type)
    (let ([source-gui-data
           (assoc-set-get (gui-model-state-source-gui-data-by-source gui-model-state)
                          ((gui-model-state-get-source-from-label gui-model-state) label)
                          cst:thunk-false)])
      (if source-gui-data
          (let ([label-gui-data
                 (assoc-set-get (source-gui-data-label-gui-data-by-label source-gui-data)
                                label cst:thunk-false)])
            (if label-gui-data
                (assoc-set-in? (label-gui-data-snip-groups-by-type label-gui-data) type)
                #f))
          #f)))
  
  ; (assoc-setof symbol snip-group) symbol (listof symbol) -> non-negative-exact-integer
  ; counts how many snips are currently displayed on the right of the position where
  ; the snips of the given type currently are or would be displayed
  (define (get-number-of-snips-on-right-from-type snip-groups-by-type type snip-type-list)
    (let ([snip-types-on-right 
           (let ([types (memq type snip-type-list)])
             (if types
                 types
                 (error 'get-number-of-snips-on-right-from-type
                        "unknown snip type: ~a" type)))])
      (let loop ([snip-types-on-right (cdr snip-types-on-right)]
                 [number-of-snips-on-right 0])
        (if (null? snip-types-on-right)
            number-of-snips-on-right
            (loop (cdr snip-types-on-right)
                  (+ number-of-snips-on-right
                     (let ([snip-group (assoc-set-get snip-groups-by-type (car snip-types-on-right) cst:thunk-false)])
                       (if snip-group
                           (snip-group-size snip-group)
                           0))))))))
  
  ; gui-model-state label symbol non-negative-exact-integer -> non-negative-exact-integer
  ; updates state (move existing snips and add new ones) and returns the position where
  ; the snips should be inserted in the text
  (define (add-snips gui-model-state label type number-of-snips)
    (let* ([source ((gui-model-state-get-source-from-label gui-model-state) label)]
           [source-gui-data
            (assoc-set-get (gui-model-state-source-gui-data-by-source gui-model-state)
                           source cst:thunk-false)]
           [label-gui-data-by-label (source-gui-data-label-gui-data-by-label source-gui-data)]
           [label-gui-data (assoc-set-get label-gui-data-by-label label cst:thunk-false)])
      (if label-gui-data
          ; the label might already have some snips attached to it.
          (let* ([snip-groups-by-type (label-gui-data-snip-groups-by-type label-gui-data)]
                 [label-starting-pos (label-gui-data-left-new-pos label-gui-data)]
                 [insertion-starting-pos
                  (- label-starting-pos
                     (get-number-of-snips-on-right-from-type
                      snip-groups-by-type type (gui-model-state-snip-type-list gui-model-state)))])
            (move-poss gui-model-state source insertion-starting-pos number-of-snips + >=)
            (if (assoc-set-in? snip-groups-by-type type)
                ; type already present, but, for a given label and type, we can have only one
                ; group of snips
                (error 'add-snips gui-model-state
                       "snips-and-arrows internal error; label ~a has already a snip group of type ~a"
                       label type)
                ; new snip type for this label
                (begin
                  (assoc-set-set snip-groups-by-type type (make-snip-group number-of-snips))
                  (set-label-gui-data-total-number-of-snips!
                   label-gui-data
                   (+ (label-gui-data-total-number-of-snips label-gui-data) number-of-snips))))
            insertion-starting-pos)
          ; create new label-gui-data for that label
          (let ([label-starting-pos (get-new-pos-from-label gui-model-state label)])
            (move-poss gui-model-state source label-starting-pos number-of-snips + >=)
            (assoc-set-set label-gui-data-by-label
                           label
                           (make-label-gui-data (+ label-starting-pos number-of-snips)
                                                0
                                                number-of-snips
                                                (assoc-set-set (assoc-set-make)
                                                               type
                                                               (make-snip-group number-of-snips))
                                                (set-make)
                                                (set-make)))
            label-starting-pos))))
  
  ; gui-model-state label symbol -> (value non-negative-exact-integer non-negative-exact-integer)
  ; removes all snips for a given label and type, move remaining snips, and returns the interval
  ; to delete in the editor
  (define (remove-inserted-snips gui-model-state label type)
    (let* ([source ((gui-model-state-get-source-from-label gui-model-state) label)]
           [source-gui-data
            (assoc-set-get (gui-model-state-source-gui-data-by-source gui-model-state)
                           source cst:thunk-false)]
           [label-gui-data
            (assoc-set-get (source-gui-data-label-gui-data-by-label source-gui-data)
                           label cst:thunk-false)])
      (if label-gui-data
          (let* ([snip-groups-by-type (label-gui-data-snip-groups-by-type label-gui-data)]
                 [snip-group (assoc-set-get snip-groups-by-type type cst:thunk-false)])
            (if snip-group
                (let* ([size (snip-group-size snip-group)]
                       [label-starting-pos (label-gui-data-left-new-pos label-gui-data)]
                       [deletion-ending-pos
                        (- label-starting-pos
                           (get-number-of-snips-on-right-from-type
                            snip-groups-by-type type (gui-model-state-snip-type-list gui-model-state)))])
                  (assoc-set-remove snip-groups-by-type type)
                  (move-poss gui-model-state source deletion-ending-pos size - >=)
                  (set-label-gui-data-total-number-of-snips!
                   label-gui-data
                   (- (label-gui-data-total-number-of-snips label-gui-data)
                      size))
                  (values (- deletion-ending-pos size) deletion-ending-pos))
                (error 'remove-inserted-snips
                       "label ~a has no snip group of type ~a"
                       label type)))
          (error 'remove-inserted-snips
                 "label ~a has no snip groups at all, let alone of type ~a"
                 label type))))
  
  ; gui-model-state -> (assoc-setof top (listof (cons non-negative-exact-integer non-negative-exact-integer)))
  ; deletes all snips and arrows and returns an associative set that, for each source, tells the
  ; starting and ending positions of all groups of snips currently displayed (so they can actually
  ; be deleted in the GUI)
  (define (remove-all-snips-and-arrows gui-model-state)
    (let ([source-gui-data-by-source (gui-model-state-source-gui-data-by-source gui-model-state)]
          [sources-recently-modified (gui-model-state-sources-recently-modified gui-model-state)]
          [result-assoc-set (assoc-set-make)])
      (assoc-set-for-each
       source-gui-data-by-source
       (lambda (source source-gui-data)
         (set-set sources-recently-modified source #f)
         (assoc-set-set result-assoc-set
                        source
                        (let* ([label-gui-data-by-label
                                (source-gui-data-label-gui-data-by-label source-gui-data)]
                               [result-list
                                (assoc-set-fold
                                 label-gui-data-by-label
                                 (lambda (label label-gui-data acc)
                                   (let ([label-left-new-pos (label-gui-data-left-new-pos label-gui-data)])
                                     (cons (cons (- label-left-new-pos (label-gui-data-total-number-of-snips label-gui-data))
                                                 label-left-new-pos)
                                           acc)))
                                 '())])
                          (assoc-set-reset label-gui-data-by-label)
                          (assoc-set-reset (source-gui-data-labels-by-mzscheme-position source-gui-data))
                          result-list))))
      result-assoc-set))
  
  )
