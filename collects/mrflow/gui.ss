; DrScheme starts counting positions at 0, MzScheme starts counting positions at 1.
; Labels (and syntax objects) use MzScheme positions, all the positions
; in this file use DrScheme positions. In all cases positions are exact non-negative
; integer.
; Among DrScheme positions, some are so-called new positions "new-pos" and some are
; old positions "old-pos". An old position is a position in the editor before any snip
; was inserted. A new position is the same position in the editor's, but after snips
; might have been inserted.
; (define-type position exact-integer)
; DrScheme also has locations, which are real x and y coordinates in the editor.
; (define-type location real)

(module gui mzscheme
  
  (require
   (lib "tool.ss" "drscheme")
   (lib "unitsig.ss")
   (lib "mred.ss" "mred")
   (lib "class.ss")
   (lib "list.ss")
   (prefix arrow: (lib "arrow.ss" "drscheme"))
   (prefix frame: (lib "framework.ss" "framework"))
   (prefix strcst: (lib "string-constant.ss" "string-constants"))

   (prefix sba: (lib "constraints-gen-and-prop.ss" "mrflow"))
   (prefix sba-gui: (lib "sba-gui-interface.ss" "mrflow"))
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
          (define/public (render-value-set val) "render-value-set-mixin")
          ; -> string
          (define/public (get-mrflow-primitives-filename)
            (build-path (collection-path "mrflow")
                        "primitives"
                        "r5rs.ss")) ; default language for MrFlow
          (super-instantiate ())))
      
      (define (phase1) 
        (drscheme:language:extend-language-interface
         mrflow-language-extension-interface<%>
         mrflow-default-implementation-mixin))
      
      (define (phase2) (void))
      

      ; GUI STYLES
      (define can-click-style (make-object style-delta% 'change-weight 'bold))
      (send can-click-style set-delta-foreground "purple")
      
      (define red-style (make-object style-delta% 'change-weight 'bold))
      (send red-style set-delta-foreground "red")
      (send red-style set-underlined-on #t)

      ; normal style must cancel everything
      (define normal-style (make-object style-delta% 'change-weight 'normal))
      (send normal-style set-delta-foreground "black")
      (send normal-style set-underlined-off #t)

      (define error-box-style (make-object style-delta%))
      (send error-box-style set-delta-foreground "red")
      
      (define type-box-style (make-object style-delta%))
      (send type-box-style set-delta-foreground "blue")
      
      ; arrow styles
      (define tacked-arrow-brush (send the-brush-list find-or-create-brush "BLUE" 'solid))
      (define untacked-arrow-brush (send the-brush-list find-or-create-brush "WHITE" 'solid))
      (define arrow-pen (send the-pen-list find-or-create-pen "BLUE" 1 'solid))

      (define mrflow-bitmap
        (drscheme:unit:make-bitmap
         (strcst:string-constant mrflow-button-title)
         (build-path (collection-path "icons") "mrflow-small.bmp")))
      
      
      ; DEFINITION WINDOW MIXIN
      (drscheme:get/extend:extend-definitions-text
       (lambda (super%)
         (class super%
           ; GUI data
           ; (cons label label-gui-data) boolean
           (define-struct arrow (other-end tacked?))
           ; position (listof arrow) (listof arrow)
           ; We could recompute pos (a "new" DrScheme position) on the fly (from the MzScheme
           ; pos in the label itself and old-pos->new-pos) each time we needed to repaint,
           ; but in practice we repaint much more often then we add snips, so we keep the pos
           ; here as a cache which is computed once from scratch when we add the label to
           ; displayed-arrows and which is then just updated each time we add a new snip.
           (define-struct label-gui-data (pos starting-arrows ending-arrows))

           (define label-gui-data-starting-arrows!
             (case-lambda
               [(label-gui-data) (label-gui-data-starting-arrows label-gui-data)]
               [(label-gui-data new-starting-arrows)
                (set-label-gui-data-starting-arrows! label-gui-data new-starting-arrows)]))
           (define label-gui-data-ending-arrows!
             (case-lambda
               [(label-gui-data) (label-gui-data-ending-arrows label-gui-data)]
               [(label-gui-data new-ending-arrows)
                (set-label-gui-data-ending-arrows! label-gui-data new-ending-arrows)]))
           
           ; (listof exact-non-negative-integer) (cons exact-non-negative-integer '())
           ; the second list has one single element, but is a list for symmetry with the first list
           (define-struct snips-poss (error type))
           
           (define snips-error!
             (case-lambda
               [() (lambda (error) (make-snips-poss error '()))]
               [(snips-poss) (snips-poss-error snips-poss)]
               [(snips-poss error-poss) (set-snips-poss-error! snips-poss error-poss)]))
           (define snips-type!
             (case-lambda
               [() (lambda (type) (make-snips-poss '() type))]
               [(snips-poss) (snips-poss-type snips-poss)]
               [(snips-poss type-poss) (set-snips-poss-type! snips-poss type-poss)]))
           

           ; GUI state
           ; boolean
           (define text-modified? #f)
           ; (union false label)
           (define previous-label #f)

           ; (union false (vector label))
           ; Translation from old positions to labels. Several positions might correspond to the
           ; same label (when an identifer is more than one character long, for example).
           (define old-pos->label #f)

           ; (listof (cons label label-gui-data))
           ; the label is used as an index, the gui-data corresponds to arrows starting
           ; or ending at such a label. Note that the data for a single arrow will appear twice
           ; in the whole data structure: once in the "starting-arrows" list for its start label,
           ; and once in the "ending-arrows" list for its end label. We need this because we need
           ; to be able to click at end of an arrow and use a menu to remove it.
           (define displayed-arrows '())
           
           ; (listof (cons label snips-poss))
           ; same kind of stuff for snips.
           (define inserted-snips '())
           
           
           ; CONVERSIONS
           (define (drscheme-pos->mzscheme-pos pos)
             (add1 pos))
           (define (mzscheme-pos->drscheme-pos pos)
             (sub1 pos))
           ; label -> exact-non-negative-integer
           ; span conversation: for all graphical purposes, the span of a compound expression is 1
           (define (get-span label)
             (if (sba:is-atom? label)
                 (sba:get-span label)
                 1))
           
           ; COLORING / CLEARING
           (inherit last-position begin-edit-sequence end-edit-sequence change-style)
           ; -> void
           (define/public (color-text)
             (begin-edit-sequence #f)
               
             ; color clickable positions
             (let ([last-pos (last-position)])
               (set! old-pos->label (make-vector (add1 last-pos) #f))
               (let loop-pos ([pos-start 0])
                 (when (< pos-start last-pos)
                   (let ([label (sba:get-label (drscheme-pos->mzscheme-pos pos-start))])
                     (if label
                         (let ([pos-end (+ pos-start (get-span label))])
                           (change-style can-click-style pos-start pos-end)
                           (let loop-range ([pos pos-start])
                             (when (< pos pos-end)
                               (vector-set! old-pos->label pos label)
                               (loop-range (add1 pos))))
                           (loop-pos pos-end))
                         ; nothing to color
                         (loop-pos (add1 pos-start)))))))
             ; color errors and such
             (for-each
              (lambda (region-to-color)
                (let ([start (sba-gui:region-to-color-start region-to-color)]
                      [end (sba-gui:region-to-color-end region-to-color)]
                      [source (sba-gui:region-to-color-source region-to-color)]
                      [color (sba-gui:region-to-color-color region-to-color)])
                  ; "this" is the definition window, so we don't try to highlight stuff
                  ; anywhere else. "source" comes from the call to expand-program below,
                  ; and remains unchanged throughout the analysis.
                  (when (eq? this source)
                    (case color
                      [(red) (change-style red-style start end)]
                      [else (message-box (strcst:string-constant mrflow-coloring-error-title)
                                         (format (strcst:string-constant mrflow-coloring-error)
                                                 color))
                            (void)]))))
              (sba:get-regions-to-color))
             
             (end-edit-sequence)
             (set! text-modified? #t))

           ; -> void
           (define/public (clear-text)
             (begin-edit-sequence #f)
             
             (clear-arrows)
             (clear-snips)
             (change-style normal-style 0 (last-position))
             
             (end-edit-sequence)
             (set! text-modified? #f))

           ; OLD-POS / NEW-POS
           (inherit delete invalidate-bitmap-cache)
           ; exact-non-negative-int -> exact-non-negative-int
           ; converts an old position (before insertion of any snip) to a new position
           ; (after insertion of all the currently inserted snips).
           ; Note: the test is "<=", which means the new position is to the right of
           ; the current snips that have positions corresponding to the same old position.
           (define (old-pos->new-pos old-pos)
             (let loop-over-labels ([snips-by-label inserted-snips]
                                    [new-pos old-pos])
               (if (null? snips-by-label)
                   new-pos
                   (let ([current-label-and-snips-pair (car snips-by-label)])
                     (if (<= (mzscheme-pos->drscheme-pos (sba:get-mzscheme-position (car current-label-and-snips-pair)))
                             old-pos)
                         (let ([snips (cdr current-label-and-snips-pair)])
                           (loop-over-labels (cdr snips-by-label)
                                             (+ new-pos
                                                (length (snips-poss-type snips))
                                                (length (snips-poss-error snips)))))
                         (loop-over-labels (cdr snips-by-label)
                                           new-pos))))))

           ; exact-non-negative-int -> exact-non-negative-int
           ; Note: the test is "<", because there might a snip that has the exact same
           ; position as new-pos, so, since a snip at position n is shown graphically
           ; between position n and n+1, we don't want to take that snip into account
           ; (i.e. that snip is on the right of the cursor or mouse pointer, not on the
           ; left).
           ; Note also that we have to be carefull: in old-pos->new-pos we add all the snips
           ; to the new-pos when the label has an old-pos to the left of or at the cursor.
           ; But here the cursor might be between two snips. So we have to consider each snip
           ; separately, we can't consider them group by group anymore.
           (define (new-pos->old-pos new-pos)
             (let loop-over-labels ([snips-by-labels inserted-snips]
                                    [old-pos new-pos])
               (if (null? snips-by-labels)
                   old-pos
                   (loop-over-labels
                    (cdr snips-by-labels)
                    (let ([snips-for-current-label (cdar snips-by-labels)])
                      (let loop-over-new-poss ([snips-new-poss (append (snips-poss-type snips-for-current-label)
                                                                       (snips-poss-error snips-for-current-label))]
                                               [old-pos old-pos])
                        (if (null? snips-new-poss)
                            old-pos
                            (if (< (car snips-new-poss) new-pos)
                                (loop-over-new-poss (cdr snips-new-poss) (sub1 old-pos))
                                (loop-over-new-poss (cdr snips-new-poss) old-pos)))))))))
           
           ; exact-non-negative-integer exact-integer (exact-non-negative-integer exact-integer -> exact-integer) -> void
           ; moves all snips and arrows that are after start, by len. start is a new position (i.e. after
           ; insertion of snips). We need to do all that so that old-pos->new-pos and new-pos->old-pos
           ; and the arrow display keep working correctly when we add new snips in the middle of others.
           (define (move-poss start len add)
             (let* ([move-pos (lambda (pos)
                                (if (< pos start) pos (add pos len)))]
                    [move-arrows (lambda (label-and-label-gui-data-pair)
                                   (let ([label-gui-data (cdr label-and-label-gui-data-pair)])
                                     (set-label-gui-data-pos! label-gui-data
                                                              (move-pos (label-gui-data-pos label-gui-data)))))]
                    [move-snips (lambda (label-snips-poss-pair)
                                  (let ([snips-poss (cdr label-snips-poss-pair)])
                                    (set-snips-poss-error! snips-poss
                                                           (map move-pos (snips-poss-error snips-poss)))
                                    (set-snips-poss-type! snips-poss
                                                          (map move-pos (snips-poss-type snips-poss)))))])
               (for-each move-arrows displayed-arrows)
               (for-each move-snips inserted-snips)))
           
           
           ; ARROWS / SNIPS
           ; label label boolean -> void
           ; To add a new arrow between start-label and end-label, we create data structures
           ; that look like this:
           ; displayed-arrows -> (listof ...
           ;                             (cons1 start-label (arrows (... (arrow cons2 tacked?) ...)
           ;                                                        (...)))
           ;                             ...
           ;                             (cons2 end-label (arrows (...)
           ;                                                      (... (arrow cons1 tacked?) ...)))
           ;                             ...)
           ; so we have mutually recursive structures. And we have to be careful about not
           ; adding twice the same arrow.
           (define (add-arrow start-label end-label tacked?)
             (let ([new-arrow-start (make-arrow #f tacked?)]
                   [new-arrow-end (make-arrow #f tacked?)]
                   [start-label-and-label-gui-data-pair (assq start-label displayed-arrows)]
                   [end-label-and-label-gui-data-pair (assq end-label displayed-arrows)])
               (if (and start-label-and-label-gui-data-pair end-label-and-label-gui-data-pair)
                   ; check if arrow already exists
                   (let* ([start-label-gui-data (cdr start-label-and-label-gui-data-pair)]
                          [end-label-gui-data (cdr end-label-and-label-gui-data-pair)]
                          [starting-arrows (label-gui-data-starting-arrows start-label-gui-data)]
                          [ending-arrows (label-gui-data-ending-arrows end-label-gui-data)]
                          [arrow-start-l (filter (lambda (arrow)
                                                 (eq? end-label-and-label-gui-data-pair
                                                      (arrow-other-end arrow)))
                                               starting-arrows)]
                          [arrow-end-l (filter (lambda (arrow)
                                               (eq? start-label-and-label-gui-data-pair
                                                    (arrow-other-end arrow)))
                                             ending-arrows)])
                     (if (null? arrow-start-l)
                         ; the arrow doesn't already exist, but the labels both already have had some arrows
                         (begin
                           (set-label-gui-data-starting-arrows! start-label-gui-data
                                                                (cons new-arrow-start starting-arrows))
                           (set-label-gui-data-ending-arrows! end-label-gui-data
                                                              (cons new-arrow-end ending-arrows))
                           (set-arrow-other-end! new-arrow-start end-label-and-label-gui-data-pair)
                           (set-arrow-other-end! new-arrow-end start-label-and-label-gui-data-pair))
                         ; the arrow already exists
                         (if tacked?
                             ; just make sure the arrow is tacked, possibly transforming an
                             ; untacked arrow into a tacked one
                             (begin
                               (set-arrow-tacked?! (car arrow-start-l) tacked?)
                               (set-arrow-tacked?! (car arrow-end-l) tacked?))
                             ; the arrow is already there, either tacked or untacked, but we
                             ; just want to add the same arrow untacked. Since tacked has priority
                             ; over untacked, we do nothing.
                             (void))
                         ))
                   ; arrow doesn't exist yet and one or both of the ending labels never had
                   ; arrows associated with it before.
                   (begin
                     (if start-label-and-label-gui-data-pair
                         (let ([start-label-gui-data (cdr start-label-and-label-gui-data-pair)])
                           (set-label-gui-data-starting-arrows!
                            start-label-gui-data
                            (cons new-arrow-start (label-gui-data-starting-arrows start-label-gui-data))))
                         (let ([new-pos (old-pos->new-pos (mzscheme-pos->drscheme-pos (sba:get-mzscheme-position start-label)))])
                           (set! start-label-and-label-gui-data-pair
                                 (cons start-label (make-label-gui-data new-pos (list new-arrow-start) '())))
                           (set! displayed-arrows (cons start-label-and-label-gui-data-pair displayed-arrows))))
                     (if end-label-and-label-gui-data-pair
                         (let ([end-label-gui-data (cdr end-label-and-label-gui-data-pair)])
                           (set-label-gui-data-ending-arrows!
                            end-label-gui-data
                            (cons new-arrow-end (label-gui-data-ending-arrows end-label-gui-data))))
                         (let ([new-pos (old-pos->new-pos (mzscheme-pos->drscheme-pos (sba:get-mzscheme-position end-label)))])
                           (set! end-label-and-label-gui-data-pair
                                 (cons end-label (make-label-gui-data new-pos '() (list new-arrow-end))))
                           (set! displayed-arrows (cons end-label-and-label-gui-data-pair displayed-arrows))))
                     (set-arrow-other-end! new-arrow-start end-label-and-label-gui-data-pair)
                     (set-arrow-other-end! new-arrow-end start-label-and-label-gui-data-pair)))))
           
           ; exact-non-negative-integer symbol -> void
           ; remove arrows starting at given label and arrows ending at same given label
           (define (remove-arrows start-label tacked?)
             (let ([start-label-and-label-gui-data-pair (assq start-label displayed-arrows)])
               (remove-ends start-label-and-label-gui-data-pair tacked?
                            label-gui-data-starting-arrows! label-gui-data-ending-arrows!)
               (remove-ends start-label-and-label-gui-data-pair tacked?
                            label-gui-data-ending-arrows! label-gui-data-starting-arrows!)))
           
           ; (cons label arrows) symbol
           ; (case-lambda
           ;   [label-gui-data -> (listof (cons label label-gui-data))]
           ;   [label-gui-data (listof (cons label label-gui-data)) -> void])
           ; (case-lambda
           ;   [label-gui-data -> (listof (cons label label-gui-data))]
           ;   [label-gui-data (listof (cons label label-gui-data)) -> void])
           ; -> void
           ; remove arrows starting at given label or arrows ending at given
           ; label (depending on selectors/settors)
           (define (remove-ends start-label-and-label-gui-data-pair tacked? select/set-this-end! select/set-other-end!)
             (when start-label-and-label-gui-data-pair
               (let ([label-gui-data (cdr start-label-and-label-gui-data-pair)])
                 (let-values ([(arrows-keep arrows-throw)
                               (let loop ([arrows (select/set-this-end! label-gui-data)]
                                          [keep '()]
                                          [throw '()])
                                 (if (null? arrows)
                                     (values keep throw)
                                     (let ([current-arrow (car arrows)])
                                       ; if tacked? is 'all, "keep" will end up being '()
                                       (if (or (eq? tacked? 'all)
                                               (eq? tacked? (arrow-tacked? current-arrow)))
                                           (loop (cdr arrows) keep (cons current-arrow throw))
                                           (loop (cdr arrows) (cons current-arrow keep) throw)))))])
                   (for-each (lambda (arrow-throw)
                               (let ([other-end-label-and-label-gui-data-pair (arrow-other-end arrow-throw)])
                                 (remove-other-end (cdr other-end-label-and-label-gui-data-pair)
                                                   start-label-and-label-gui-data-pair
                                                   select/set-other-end!)))
                             arrows-throw)
                   (select/set-this-end! label-gui-data arrows-keep)))))
           
           ; label-gui-data (cons label label-gui-data) (case-lambda
           ;                                              [label-gui-data -> (listof (cons label label-gui-data))]
           ;                                              [label-gui-data (listof (cons label label-gui-data)) -> void])
           ; -> void
           ; removes one pair corresponding to the remote end of the arrow we are removing
           ; in remove-ends
           (define (remove-other-end label-gui-data start-label-and-label-gui-data-pair select/set!)
             (select/set! label-gui-data
                          (filter (lambda (arrow)
                                    (let ([other-end-pair-of-this-other-end (arrow-other-end arrow)])
                                      (not (eq? other-end-pair-of-this-other-end
                                                start-label-and-label-gui-data-pair))))
                                  (select/set! label-gui-data))))

           ; label exact-non-negative-integer
           ; (case-lambda
           ;   [-> ((listof exact-non-negative-integer) -> snips-poss)]
           ;   [snips-poss -> (listof exact-non-negative-integer)]
           ;   [snips-poss (listof exact-non-negative-integer) -> void])
           ; -> void
           (define (add-inserted-snip label label-starting-pos snips-poss!)
             (move-poss label-starting-pos 1 +)
             (let ([label-snips-poss-pair (assq label inserted-snips)])
               (if label-snips-poss-pair
                   ; the label has already some snips attached to it.
                   (let ([snips-poss (cdr label-snips-poss-pair)])
                     (snips-poss! snips-poss (cons label-starting-pos (snips-poss! snips-poss))))
                   (set! inserted-snips
                         (cons (cons label ((snips-poss!) (list label-starting-pos)))
                               inserted-snips)))))

           ; (cons label snips-poss) exact-non-negative-integer symbol -> void
           ; remove all snips with the same old pos as the old pos corresponsing to the
           ; given new pos. We have to go through the old pos because two snips can't have
           ; the same new position.
           (define (remove-inserted-snips snips-poss label-starting-pos snips-poss!)
             (let ([len (length (snips-poss! snips-poss))])
               (snips-poss! snips-poss '())
               (move-poss label-starting-pos len -)))
      
           ; -> void
           (define (clear-arrows)
             (set! displayed-arrows '())
             (invalidate-bitmap-cache))
           
           ; -> void
           ; remove all snips. We sort first, to make sure we remove snips from bottom to
           ; top. If we removed snips from top to bottom, we would have to recompute the
           ; positions of the remaining snips each time we removed one. If we removed snips
           ; in the order in which they appear in the list, we wouldn't be able to keep track
           ; of their positions at all.
           (define (clear-snips)
             (let ([all-snips (foldr (lambda (label-snips-poss-pair other-snips)
                                       (let ([snips-poss (cdr label-snips-poss-pair)])
                                         (append (snips-poss-type snips-poss)
                                                 (snips-poss-error snips-poss)
                                                 other-snips)))
                                     '()
                                     inserted-snips)])
               (set! analysis-currently-deleting? #t)
               (begin-edit-sequence #f)
               (for-each
                (lambda (new-pos)
                  (delete new-pos (add1 new-pos) #f))
                (quicksort all-snips >=))
               (end-edit-sequence)
               (set! inserted-snips '())
               (set! analysis-currently-deleting? #f)))

           ; EDITOR MODIFICATIONS
           (rename [super-after-insert after-insert]
                   [super-after-delete after-delete])
           ; so we can differenciate between actions done by the analysis and actions
           ; done by the user. Also prevents an infinite loop when deleting: if the user
           ; deletes something, it triggers a call to after-delete, which deletes all the
           ; snips, which triggers calls to after-delete, etc... so after-delete needs to
           ; wrapper to prevent an infinite loop.
           (define analysis-currently-inserting? #f)
           (define analysis-currently-deleting? #f)

           ; exact-non-negative-integer exact-non-negative-integer -> void
           (define/override (after-insert start len)
             (super-after-insert start len)
             (when (and text-modified?
                        (not analysis-currently-inserting?))
               (move-poss start len +)
               (clear-text))
             )
           
           ; exact-non-negative-integer exact-non-negative-integer -> void
           (define/override (after-delete start len)
             (super-after-delete start len)
             (when (and text-modified?
                        (not analysis-currently-deleting?))
               (move-poss start len -)
               (clear-text))
             )
           
           
           ; EDITOR INTERFACE
           (inherit dc-location-to-editor-location find-position)
           ; mouse-event% -> exact-non-negative-integer
           (define (get-new-pos event)
             (let*-values ([(event-x) (send event get-x)]
                           [(event-y) (send event get-y)]
                           [(x y) (dc-location-to-editor-location
                                   event-x event-y)])
               (find-position x y)))
           
           ; (box number) (box number) -> number
           (define (average box1 box2)
             (/ (+ (unbox box1) (unbox box2)) 2))

           (rename [super-on-paint on-paint]
                   [super-on-local-event on-local-event])
           (inherit insert position-location)
           ; boolean dc<%> real real real real real real symbol -> void
           (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
             (super-on-paint before? dc left top right bottom dx dy draw-caret)
             (let ([old-pen (send dc get-pen)]
                   [old-brush (send dc get-brush)]
                   ; (cons exact-non-negative-integer exact-non-negative-integer) -> void
                   [draw-arrow
                    (lambda (arrow start-label start-label-pos-start-left)
                      (let* ([start-label-pos-start-right (add1 start-label-pos-start-left)]
                             [start-label-pos-end (+ start-label-pos-start-left
                                                     (get-span start-label))]
                             [end-label-and-label-gui-data (arrow-other-end arrow)]
                             [end-label (car end-label-and-label-gui-data)]
                             [end-label-pos-start-left (label-gui-data-pos (cdr end-label-and-label-gui-data))]
                             [end-label-pos-start-right (add1 end-label-pos-start-left)]
                             [end-label-pos-end (+ end-label-pos-start-left
                                                   (get-span end-label))]
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
                        (position-location start-label-pos-start-left start-label-start-left-x start-label-start-left-y-top #t)
                        (position-location start-label-pos-start-right start-label-start-right-x start-label-start-right-y-bot #f)
                        (position-location start-label-pos-end start-label-end-x start-label-end-y-top #t)
                        (position-location start-label-pos-end #f start-label-end-y-bot #f)
                        (position-location end-label-pos-start-left end-label-start-left-x end-label-start-left-y-top #t)
                        (position-location end-label-pos-start-right end-label-start-right-x end-label-start-right-y-bot #f)
                        (position-location end-label-pos-end end-label-end-x end-label-end-y-top #t)
                        (position-location end-label-pos-end #f end-label-end-y-bot #f)
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
                                               dx dy)]))))])
               (send dc set-pen arrow-pen)
               (for-each (lambda (label-and-label-gui-data-pair)
                           (let* ([label (car label-and-label-gui-data-pair)]
                                  [label-gui-data (cdr label-and-label-gui-data-pair)]
                                  ; all the starting arrows for a given label have the same starting position
                                  [label-new-pos (label-gui-data-pos label-gui-data)])
                             (for-each (lambda (arrow)
                                         (if (arrow-tacked? arrow)
                                             (send dc set-brush tacked-arrow-brush)
                                             (send dc set-brush untacked-arrow-brush))
                                         (draw-arrow arrow label label-new-pos))
                                       ; only draw the arrows starting at this label, not looking at
                                       ; the ones ending there.
                                       (label-gui-data-starting-arrows label-gui-data))))
                         displayed-arrows)
               (send dc set-pen old-pen)
               (send dc set-brush old-brush)))

           (inherit get-canvas)
           ; mouse-event% -> void
           (define/override (on-local-event event)
             (cond
               [(not text-modified?) (super-on-local-event event)]
               [(and (send event button-down? 'right)
                     (vector-ref old-pos->label (new-pos->old-pos (get-new-pos event))))
                =>
                (lambda (label)
                  (let* ([menu (make-object popup-menu%)]
                         ; (get-new-pos event) will give us a position that might be somewhere
                         ; in the middle of an identifier. So either we loop down the old-pos->label
                         ; vector, starting from (get-new-pos event), until we find the first
                         ; position refering to that label, or we recompute the label's starting
                         ; position from scratch.
                         [label-starting-pos (old-pos->new-pos (mzscheme-pos->drscheme-pos (sba:get-mzscheme-position label)))]
                         [label-snips-poss-pair (assq label inserted-snips)])
                    ; TYPE
                    (if (or (not label-snips-poss-pair)
                            (null? (snips-poss-type (cdr label-snips-poss-pair))))
                        (let ([types (list (sba:get-type label))])
                          (unless (null? types) ; always false, for now.
                            (make-object menu-item%
                              (strcst:string-constant mrflow-popup-menu-show-type)
                              menu
                              (lambda (item event)
                                (set! analysis-currently-inserting? #t)
                                (begin-edit-sequence #f)
                                (for-each (lambda (type)
                                            (let* ([snip-text (make-object text%)]
                                                   [snip (make-object editor-snip% snip-text)])
                                              (send snip-text insert (sba:pp-type type 'gui))
                                              (insert snip label-starting-pos label-starting-pos)
                                              (change-style type-box-style label-starting-pos (add1 label-starting-pos))
                                              (add-inserted-snip label label-starting-pos snips-type!)))
                                          types)
                                (invalidate-bitmap-cache)
                                (end-edit-sequence)
                                (set! analysis-currently-inserting? #f)
                                ))))
                        (make-object menu-item%
                          (strcst:string-constant mrflow-popup-menu-hide-type)
                          menu
                          (lambda (item event)
                            (let* ([snips-poss (cdr label-snips-poss-pair)]
                                   [type-snips-starting-pos (- label-starting-pos
                                                               (length (snips-poss-type snips-poss)))])
                              (set! analysis-currently-deleting? #t)
                              (begin-edit-sequence #f)
                              (delete type-snips-starting-pos label-starting-pos #f)
                              (remove-inserted-snips snips-poss type-snips-starting-pos snips-type!)
                              (end-edit-sequence)
                              (set! analysis-currently-deleting? #f)
                              ))))
                    ; ERRORS
                    (if (or (not label-snips-poss-pair)
                            (null? (snips-poss-error (cdr label-snips-poss-pair))))
                        (let ([errors (sba:get-errors label)])
                          (unless (null? errors)
                            (make-object menu-item%
                              (strcst:string-constant mrflow-popup-menu-show-errors)
                              menu
                              (lambda (item event)
                                ; insert to the left of any type snip
                                (let ([type-snips-starting-pos
                                       (if label-snips-poss-pair
                                           (- label-starting-pos
                                              (length (snips-poss-type (cdr label-snips-poss-pair))))
                                           label-starting-pos)])
                                  (set! analysis-currently-inserting? #t)
                                  (begin-edit-sequence #f)
                                  (for-each (lambda (error-msg)
                                              (let* ([snip-text (make-object text%)]
                                                     [snip (make-object editor-snip% snip-text)])
                                                (send snip-text insert error-msg)
                                                (insert snip type-snips-starting-pos type-snips-starting-pos)
                                                (change-style error-box-style type-snips-starting-pos (add1 type-snips-starting-pos))
                                                (add-inserted-snip label type-snips-starting-pos snips-error!)))
                                            errors)
                                  (invalidate-bitmap-cache)
                                  (end-edit-sequence)
                                  (set! analysis-currently-inserting? #f)
                                  )))))
                        (make-object menu-item%
                          (strcst:string-constant mrflow-popup-menu-hide-errors)
                          menu
                          (lambda (item event)
                            (let* ([snips-poss (cdr label-snips-poss-pair)]
                                   [type-snips-starting-pos (- label-starting-pos
                                                               (length (snips-poss-type snips-poss)))]
                                   [error-snips-starting-pos (- type-snips-starting-pos
                                                                (length (snips-poss-error snips-poss)))])
                              (set! analysis-currently-deleting? #t)
                              (begin-edit-sequence #f)
                              (delete error-snips-starting-pos type-snips-starting-pos #f)
                              (remove-inserted-snips snips-poss error-snips-starting-pos snips-error!)
                              (end-edit-sequence)
                              (set! analysis-currently-deleting? #f)
                              ))))
                    ; ARROWS
                    (let* ([label-and-label-gui-data-pair (assq label displayed-arrows)]
                           [parents (sba:parents label)]
                           [parents-max-arrows (length parents)]
                           [parents-tacked-arrows
                            (if label-and-label-gui-data-pair
                                (length (filter arrow-tacked? (label-gui-data-ending-arrows (cdr label-and-label-gui-data-pair))))
                                0)]
                           [children (sba:children label)]
                           [children-max-arrows (length children)]
                           [children-tacked-arrows
                            (if label-and-label-gui-data-pair
                                (length (filter arrow-tacked? (label-gui-data-starting-arrows (cdr label-and-label-gui-data-pair))))
                                0)]
                           [max-arrows (+ parents-max-arrows children-max-arrows)]
                           [tacked-arrows (+ parents-tacked-arrows children-tacked-arrows)])
                      (when (< tacked-arrows max-arrows)
                        (make-object menu-item%
                          (strcst:string-constant mrflow-popup-menu-tack-all-arrows)
                          menu
                          (lambda (item event)
                            ; remove all (possibly untacked) arrows and add all arrows, tacked.
                            (remove-arrows label 'all)
                            (for-each (lambda (parent-label)
                                        (add-arrow parent-label label #t))
                                      parents)
                            (for-each (lambda (child-label)
                                        (add-arrow label child-label #t))
                                      children)
                            (invalidate-bitmap-cache))))
                      (when (> tacked-arrows 0)
                        (make-object menu-item%
                          (strcst:string-constant mrflow-popup-menu-untack-all-arrows)
                          menu
                          (lambda (item event)
                            (remove-arrows label 'all)
                            (invalidate-bitmap-cache)))))
                    (send (get-canvas) popup-menu menu
                          (add1 (inexact->exact (floor (send event get-x))))
                          (add1 (inexact->exact (floor (send event get-y)))))
                    ))]
               [(send event leaving?)
                (when previous-label
                  (remove-arrows previous-label #f)
                  (set! previous-label #f)
                  (invalidate-bitmap-cache))]
               [(or (send event moving?)
                    (send event entering?))
                (let ([label (vector-ref old-pos->label (new-pos->old-pos (get-new-pos event)))])
                  (if previous-label
                      (if (eq? label previous-label)
                          ; nothing to do, still pointing at same stuff
                          (void)
                          (if label
                              ; pointing at something new, remove old untacked arrows, add new ones
                              (let ([parents (sba:parents label)]                                
                                    [children (sba:children label)])
                                (remove-arrows previous-label #f)
                                (set! previous-label label)
                                (for-each (lambda (parent-label)
                                            (add-arrow parent-label label #f))
                                          parents)
                                (for-each (lambda (child-label)
                                            (add-arrow label child-label #f))
                                          children)
                                (invalidate-bitmap-cache))
                              ; not pointing at anything new, just remove old arrows
                              (begin
                                (remove-arrows previous-label #f)
                                (set! previous-label #f)
                                (invalidate-bitmap-cache))))
                      (if label
                          ; pointing at something, coming from nowhere
                          (let ([parents (sba:parents label)]                                
                                [children (sba:children label)])
                            (set! previous-label label)
                            (for-each (lambda (parent-label)
                                        (add-arrow parent-label label #f))
                                      parents)
                            (for-each (lambda (child-label)
                                        (add-arrow label child-label #f))
                                      children)
                            (invalidate-bitmap-cache))
                          ; pointing at nothing, coming from nowhere
                          (void))))]
               [else (super-on-local-event event)]
               )) ; on-local-event
           (super-instantiate ())
           ) ; class
         )) ; drscheme:get/extend:extend-definitions-text
      
      ; UNIT FRAME MIXIN
      (drscheme:get/extend:extend-unit-frame
       (lambda (super%)
         (class super%
           (inherit get-button-panel get-definitions-text get-interactions-text)
           (rename [super-disable-evaluation disable-evaluation]
                   [super-enable-evaluation enable-evaluation]
                   [super-clear-annotations clear-annotations])
           
           (define/override (clear-annotations)
             (super-clear-annotations)
             (send (get-definitions-text) clear-text))
           
           (define/override (enable-evaluation)
             (send analyze-button enable #t)
             (super-enable-evaluation))

           (define/override (disable-evaluation)
             (send analyze-button enable #f)
             (super-disable-evaluation))
           
           ; should be called before we instantiate below
           (super-instantiate ())
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
                            ; REAL SBA WORK STARTS HERE
                            (sba:initialize-primitive-type-schemes primitive-table-file)
                            (send interactions-text
                                  expand-program
                                  (drscheme:language:make-text/pos definitions-text
                                                                   0
                                                                   (send definitions-text last-position))
                                  language-settings
                                  (lambda (exception? syntax-object-or-exception run-in-expansion-thread loop)
                                    (if exception?
                                        (let ([message (car syntax-object-or-exception)]
                                              [exn (cdr syntax-object-or-exception)])
                                          (cond
                                            [(exn:read? exn)
                                             (let ([end (exn:read-position exn)])
                                               (send definitions-text change-style red-style (sub1 end) end)
                                               (message-box (strcst:string-constant mrflow-read-exception-title)
                                                            (format (strcst:string-constant mrflow-read-exception) message)
                                                            #f '(ok)))]
                                            [(exn:syntax? exn)
                                             (let ([syntax-object (exn:syntax-expr exn)])
                                               (when syntax-object
                                                 (let ([start (sub1 (syntax-position syntax-object))])
                                                   (send definitions-text change-style red-style
                                                         start (+ start (syntax-span syntax-object)))
                                                   (message-box (strcst:string-constant mrflow-syntax-exception-title)
                                                                (format (strcst:string-constant mrflow-syntax-exception) message)
                                                                #f '(ok)))))]
                                            [else
                                             (message-box (strcst:string-constant mrflow-unknown-exception-title)
                                                          (format (strcst:string-constant mrflow-unknown-exception) exn))]))
                                        (unless (eof-object? syntax-object-or-exception)
                                          ;(printf "~a~n~n" (syntax-object->datum syntax-object-or-exception))
                                          (sba:create-label-from-term syntax-object-or-exception '() #f)))
                                    (loop)))
                            (sba:check-primitive-types)
                            ; REAL SBA WORK ENDS HERE
                            
                            ;(printf "time: ~a ms~n" (- (current-milliseconds) start))
                            (send definitions-text color-text)
                            )
                          ; get-mrflow-primitives-filename defaults to R5RS
                          ; (see mrflow-default-implementation-mixin above), so if we arrive here,
                          ; we know we are in trouble.
                          (message-box (strcst:string-constant mrflow-language-primitives-error-title)
                                       (format (strcst:string-constant mrflow-language-primitives-error)
                                               primitive-table-file)
                                       #f '(ok)))))))))

           (send (get-button-panel) change-children
                     (lambda (button-list)
                       (cons analyze-button (remq analyze-button button-list))))
               ) ; class
         )) ; drscheme:get/extend:extend-unit-frame
                    
      )) ; tool@ unit/sig
  ); module
