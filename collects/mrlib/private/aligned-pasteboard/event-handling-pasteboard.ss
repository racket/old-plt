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
   (lib "contracts.ss")
   (lib "etc.ss")
   "pasteboard-lib.ss"
   "snip-lib.ss"
   "alignment.ss")
  
  (provide/contract
   (event-handling-pasteboard-mixin mixin-contract))
  
  (define (event-handling-pasteboard-mixin super%)
    (class super%
      
      (inherit refresh-delayed? find-first-snip)
      (inherit-field aligned-rects)
      
      ;; after-insert ((is-a?/c snip%) (is-a?/c snip%) number? number? . -> . void?)
      ;; called after a snip is inserted to the pasteboard
      (rename [super-after-insert after-insert])
      (define/override (after-insert snip before x y)
        (calc/realign (pasteboard-root this))
        (super-after-insert snip before x y))
      
      ;; after-delete ((is-a?/c snip%) . -> . void?)
      ;; called after a snip is deleted from the pasteboard%
      (rename [super-after-delete after-delete])
      (define/override (after-delete snip)
        (calc/realign (pasteboard-root this))
        (super-after-delete snip))
      
      ;; after-reorder ((is-a?/c snip%) (is-a?/c snip%) boolean? . -> . void?)
      ;; called after a snip is moved in the front to back snip order
      (rename [super-after-reorder after-reorder])
      (define/override (after-reorder snip to-snip before?)
        (send (pasteboard-root this) realign)
        (super-after-reorder snip to-snip before?))
      
      ;; resized ((is-a?/c snip%) . -> . void?)
      ;; called when a snip inside the editor is resized
      (rename [super-resized resized])
      (define/override (resized snip redraw-now?)
        (super-resized snip redraw-now?)
        (unless (or (refresh-delayed?)
                    (same-size? snip))
          (calc/realign (pasteboard-root this))))
      
      ;; same-size? ((is-a?/c snip%) . -> . boolean?)
      ;; determines whether the snip is the same size as it was the last time the alignment was invoked
      (define/private (same-size? snip)
        (if (empty? aligned-rects)
            false
            (let ([r (find-rect snip)])
              (and (equal? (snip-min-width snip)
                           (dim-size (rect-x r)))
                   (equal? (snip-min-height snip)
                           (dim-size (rect-y r)))))))
      
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
  
  ;; calc/realign ((is-a?/c aligned-pasteboard<%>) . -> . void?)
  ;; sends a message to the pasteboard to recalculate min sizes and realign
  (define (calc/realign pasteboard)
    (let ([parent (pasteboard-parent pasteboard)])
      (when parent
        (send parent set-aligned-min-sizes)
        (send pasteboard realign))))
  )
