(module case-layout mzscheme
  
  (require
   (lib "unit.ss")
   (lib "unitsig.ss")
   (lib "class.ss")
   (lib "aligned-pasteboard.ss" "mrlib")
   (lib "etc.ss")
   (lib "mred.ss" "mred")
   "test-text.ss"
   "signatures.ss"
   "interfaces.ss")
  
  (provide case-layout@)
  
  (define case-layout@
    (unit/sig case^
      (import (super : case^))
      (define case%
        (class* super:case% (test-suite:item<%>)
          (inherit get-editor)
          (inherit-field call expected actual test pass test-showing?)
          
          ;; show-test (boolean? . -> . void?)
          ;; show/hide the test in the display
          (rename [super-show-test show-test])
          (define/override (show-test show?)
            (cond
              [(and test-showing? (not show?))
               (send bottom-pb release-snip test-snip)]
              [(and (not test-showing?) show?)
               (send bottom-pb insert test-snip pass)]
              [else (void)])
            (super-show-test show?))
          
          (super-instantiate ()
            (editor (instantiate vertical-pasteboard% ()))
            (stretchable-width true)
            (stretchable-height false))
          
          (field
           [bottom-pb (instantiate horizontal-pasteboard% ())]
           [call-snip (label-box "Call" call)]
           [expected-snip (label-box "Expected" expected)]
           [actual-snip (label-box "Actual" actual)]
           [test-snip (label-box "Equality Test" test)])
          
          (send* bottom-pb
            (begin-edit-sequence)
            (insert expected-snip false)
            (insert actual-snip false))
          (when test-showing?
            (send bottom-pb insert test-snip false))
          (send* bottom-pb
            (insert pass false)
            (end-edit-sequence))
          
          (send* (get-editor)
            (begin-edit-sequence)
            (insert call-snip false)
            (insert (instantiate aligned-editor-snip% ()
                      (editor bottom-pb)
                      (with-border? false)
                      (top-margin 0)
                      (bottom-margin 0)
                      (left-margin 0)
                      (right-margin 0))
                    false)
            (end-edit-sequence))
          ))
      ))
  
  ;; label-box (string? (is-a?/c editor<%>) . -> . (is-a?/c aligned-editor-snip%))
  ;; a snip with a box to type in and a label
  (define (label-box label text)
    (let ([sd (make-object style-delta% 'change-normal-color)]
          [pb (instantiate vertical-pasteboard% ())]
          [label-snip (make-object string-snip% label)])
      (send sd set-delta-foreground "indigo")
      (send sd set-delta 'change-weight 'bold)
      (send* pb
        (begin-edit-sequence)
        (insert label-snip false)
        (change-style sd label-snip)
        (insert (instantiate test:editor-snip% ()
                  (editor text)
                  (stretchable-width true)
                  (stretchable-height false))
                false)
        (end-edit-sequence))
      (instantiate aligned-editor-snip% ()
        (with-border? false)
        (top-margin 0)
        (bottom-margin 0)
        (left-margin 0)
        (right-margin 0)
        (editor pb))))
  )