;;note:   at first glance it may not be obvious why the same-size? check is being done within resized
;;      or for that matter done at all. This is one small optimization that has been added because
;;      snips have a tendency to call resized when they are not actually resized. Also if they are
;;      explicitly resized to the same size we do not want to resize them. This is normally not a problem
;;      but as the geometry management is O(n) on the number of snips in the entire top-level, simply
;;      typing into a text boxs causes alot of overhead.

(module event-handling-pasteboard mzscheme
  
  (require
   (lib "class.ss")
   (lib "list.ss")
   (lib "contract.ss")
   (lib "etc.ss")
   "pasteboard-lib.ss"
   "interface.ss"
   "snip-lib.ss")
  
  (provide/contract
   (event-handling-pasteboard-mixin mixin-contract))
  (define (event-handling-pasteboard-mixin super%)
    (class super%
      
      (inherit refresh-delayed? realign find-first-snip)
      (inherit-field aligned-rects needs-realign? ignore-resizing?)
      (field [in-edit-sequence? false])
      
      ;; after-insert ((is-a?/c snip%) (is-a?/c snip%) number? number? . -> . void?)
      ;; called after a snip is inserted to the pasteboard
      (rename [super-after-insert after-insert])
      (define/override (after-insert snip before x y)
        (calc/realign)
        (super-after-insert snip before x y))
      
      ;; after-delete ((is-a?/c snip%) . -> . void?)
      ;; called after a snip is deleted from the pasteboard%
      (rename [super-after-delete after-delete])
      (define/override (after-delete snip)
        (calc/realign)
        (super-after-delete snip))
      
      ;; after-reorder ((is-a?/c snip%) (is-a?/c snip%) boolean? . -> . void?)
      ;; called after a snip is moved in the front to back snip order
      (rename [super-after-reorder after-reorder])
      (define/override (after-reorder snip to-snip before?)
        (realign)
        (super-after-reorder snip to-snip before?))
      
      ;; resized ((is-a?/c snip%) . -> . void?)
      ;; called when a snip inside the editor is resized
      (rename [super-resized resized])
      (define/override (resized snip redraw-now?)
        (super-resized snip redraw-now?)
        (unless ignore-resizing?
          (when (or redraw-now?
                    (and (not (refresh-delayed?))
                         (needs-resize? snip)))
            (calc/realign))))
      
      ;; after-edit-sequence (-> void?)
      ;; called after an edit-sequence ends
      (rename [super-after-edit-sequence after-edit-sequence])
      (define/override (after-edit-sequence)
        (set! in-edit-sequence? false)
        (when needs-realign? (calc/realign)))
      
      (rename [super-on-edit-sequence on-edit-sequence])
      (define/override (on-edit-sequence)
        (set! in-edit-sequence? true)
        (super-on-edit-sequence))
      
      ;; calc/realign (-> void?)
      ;; sends a message to the pasteboard to recalculate min sizes and realign
      (define/private (calc/realign)
        (if in-edit-sequence?
            (set! needs-realign? true)
            (let* ([root (pasteboard-root this)]
                   [parent (pasteboard-parent root)])
              (when parent
                (send parent set-aligned-min-sizes)
                (send root realign)))))
      
      ;; needs-resize? ((is-a?/c snip%) . -> . boolean?)
      ;; determines if the snip's size is smaller than it's min size
      (define/private (needs-resize? snip)
        (cond
          [(is-a? snip aligned-snip<%>)
           (or (< (snip-width snip)
                  (send snip get-aligned-min-width))
               (< (snip-height snip)
                  (send snip get-aligned-min-height))
               (and (not (send snip stretchable-width))
                    (> (snip-width snip)
                        (send snip get-aligned-min-width)))
               (and (not (send snip stretchable-height))
                    (> (snip-height snip)
                        (send snip get-aligned-min-height))))]
          [else false]))
      
      ;; find-rect ((is-a?/c snip%) . -> . rect?)
      ;; finds the rect that corresponds to the given snip
      (define/private (find-rect target-snip)
        (letrec ([find-rect-aux
                  (lambda (snip rects)
                    (cond
                      [(or (equal? snip false) (empty? rects))
                       (error 'find-rect "Snip not found")]
                      [else
                       (if (equal? snip target-snip)
                           (car rects)
                           (find-rect-aux (send snip next)
                                          (rest rects)))]))])
          (find-rect-aux (find-first-snip) aligned-rects)))
      
      (super-instantiate ())
      ))
  )
