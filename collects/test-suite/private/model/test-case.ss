(module test-case mzscheme
  
  (require
   (lib "class.ss")
   (lib "mred.ss" "mred")
   (lib "etc.ss")
   (lib "framework.ss" "framework")
   (lib "aligned-pasteboard.ss" "mrlib")
   "test-text.ss")
  
  (provide test-case%)
  
  (define *unknown* (build-path (collection-path "test-suite" "icons") "question-mark.jpeg"))
  (define *success* (build-path (collection-path "test-suite" "icons") "check-mark.jpeg"))
  (define *failure* (build-path (collection-path "test-suite" "icons") "cross.jpeg"))
  (define *error* (build-path (collection-path "icons") "bug09.gif"))
  
  (define test-case%
    (class aligned-editor-snip%
      (inherit next)
      
      (field
       (call (instantiate test:text% ()))
       (expected (instantiate test:text% ()))
       (actual (instantiate (text:hide-caret/selection-mixin test:text%) ()))
       (test (instantiate test:text% ()))
       (pass (make-object image-snip% *unknown*)))
      
      (send* actual
        (lock true)
        (hide-caret true))
      (send test insert "equal?")
      (send pass load-file *unknown*)
      
      ;; get-call (-> (is-a?/c text%))
      ;; get the call text box
      (define/public (get-call)
        call)
      
      ;; get-expected (-> (is-a?/c text%))
      ;; get the expected text box
      (define/public (get-expected)
        expected)
      
      ;; get-test (-> (is-a?/c text%))
      ;; get the test text box
      (define/public (get-test)
        test)
      
      ;; reset (-> void?)
      ;; resets the result of the test case
      (define/public (reset)
        (send* actual
          (lock false)
          (erase)
          (lock true))
        (send pass load-file *unknown*))
      
      ;; set-actual (string? . -> . void?)
      ;; set the text in the actual field to the string given
      (define/public (set-actual string)
        (send* actual
          (lock false)
          (erase)
          (insert string)
          (lock true)))
      
      ;; set-icon (boolean? . -> . void?)
      ;; set the image of the icon to either pass or fail
      (define/public (set-icon pass?)
        (send pass load-file
              (if pass?
                  *success*
                  *failure*)))
      
      (super-instantiate ()
        (stretchable-width true)
        (stretchable-height false)
        (editor
         (test-case-editor
          call expected actual test pass)))
      ))
  
  ;; test-case-editor ((is-a?/c editor<%>) (is-a?/c editor<%>) (is-a?/c editor<%>)
  ;;                   (is-a?/c editor<%>) (is-a?/c image-snip%) . -> . (is-a?/c editor<%>))
  ;; an editor to display the test case with the given fields
  (define (test-case-editor call expected actual test pass)
    (let ([main-pb (instantiate vertical-pasteboard% ())]
          [bottom-pb (instantiate horizontal-pasteboard% ())])
      (send* bottom-pb
        (begin-edit-sequence)
        (insert (labeled-box "Expected" expected) false)
        (insert (labeled-box "Actual" actual) false)
        (insert (labeled-box "Equality Test" test) false)
        (insert pass false)
        (end-edit-sequence))
      (send* main-pb
        (begin-edit-sequence)
        (insert (labeled-box "Call" call) false)
        (insert (instantiate aligned-editor-snip% ()
                  (with-border? false)
                  (editor bottom-pb)
                  (top-margin 0)
                  (bottom-margin 0)
                  (left-margin 0)
                  (right-margin 0))
                false)
        (end-edit-sequence))
      main-pb))
  
  ;; labeled-box: (string? (is-a?/c editor<%>) . -> . (is-a?/c editor-snip%))
  ;; a snip with a box to type in and a label
  (define (labeled-box label text)
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
        (editor pb))))
  )
