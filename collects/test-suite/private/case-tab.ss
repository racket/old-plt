(module case-tab mzscheme
  
  (require
   (lib "unitsig.ss")
   (lib "class.ss")
   (lib "etc.ss")
   "signatures.ss"
   "interfaces.ss")
  
  (provide case-tab@)
  
  (define case-tab@
    (unit/sig case^
      (import (super : case^))
      (define case%
        (class* super:case% (test-suite:item<%>)
          (inherit previous next)
          (inherit-field call expected test test-showing?)
          
          ;; focus-first (-> void?)
          ;; give keyboard focus to the call
          (define/public (focus-first)
            (send call set-caret-owner false 'global)
            (scroll-to this))
          
          ;; focus-last (-> void?)
          ;; give focus to the last text box
          (define/public (focus-last)
            (if test-showing?
                (send test set-caret-owner false 'global)
                (send expected set-caret-owner false 'global))
            (scroll-to this))
          
          ;; tab-ahead ((is-a?/c text%) . -> . void?)
          ;; go to the next tab position from the text
          (define/public (tab-ahead text)
            (cond
              [(eq? text call)
               (send expected set-caret-owner false 'global)]
              [(eq? text expected)
               (if test-showing?
                   (send test set-caret-owner false 'global)
                   (let ([case (next)])
                     (when case (send case focus-first))))]
              [(eq? text test)
               (let ([case (next)])
                 (when case (send case focus-first)))]))
          
          ;; tab-back ((is-a?/c text%) . -> . void?)
          ;; go to the previous tab position from the text
          (define/public (tab-back text)
            (cond
              [(eq? text call)
               (let ([case (previous)])
                 (when case (send case focus-last)))]
              [(eq? text expected)
               (send call set-caret-owner false 'global)]
              [(eq? text test)
               (send expected set-caret-owner false 'global)]))
          
          (super-instantiate ())
          ))
      ))
  
  ;; scroll-to ((is-a?/c snip%) . -> . void?)
  ;; scroll a snips parent to show it in the visable area
  (define (scroll-to snip)
    (let* ([editor (send (send snip get-admin) get-editor)]
           [left (box 0)]
           [right (box 0)]
           [top (box 0)]
           [bottom (box 0)])
      (send editor get-snip-location snip left top false)
      (send editor get-snip-location snip right bottom true)
      (send editor scroll-to snip 0 0
            (- (unbox right) (unbox left))
            (- (unbox bottom) (unbox top))
            true 'start)))
  )