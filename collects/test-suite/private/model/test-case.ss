(module test-case mzscheme
  
  (require
   (lib "class.ss")
   (lib "mred.ss" "mred")
   (lib "etc.ss")
   (lib "framework.ss" "framework")
   (lib "aligned-pasteboard.ss" "mrlib")
   (lib "contracts.ss")
   "test-text.ss")
  
  (provide test-case%)
  
  (define *unknown* (build-path (collection-path "test-suite" "icons") "question-mark.jpeg"))
  (define *success* (build-path (collection-path "test-suite" "icons") "check-mark.jpeg"))
  (define *failure* (build-path (collection-path "test-suite" "icons") "cross.jpeg"))
  (define *error* (build-path (collection-path "icons") "bug09.gif"))
  
  (define test-case-class%
    (class snip-class%
      ;; read ((is-a?/c editor-stream-in%) . -> . void?)
      ;; read a snip from the stream
      (define/override (read f)
        (let ([call-text (instantiate test:text% ())]
              [expected-text (instantiate test:text% ())]
              [test-text (instantiate test:text% ())])
          (send call-text read-from-file f)
          (send expected-text read-from-file f)
          (send test-text read-from-file f)
          (instantiate test-case% ()
            (call call-text)
            (expected expected-text)
            (test test-text)
            (test-showing? (string=? "#t" (send f get-string))))))
      (super-instantiate ())))
  
  (define tcc (instantiate test-case-class% ()))
  (send tcc set-classname "test-case%")
  (send tcc set-version 1)
  (send (get-the-snip-class-list) add tcc)
  
  (define test-case%
    (class aligned-editor-snip%
      
      (inherit get-editor next set-snipclass)
      
      (init-field
       [call (instantiate test:text% ())]
       [expected (instantiate test:text% ())]
       [test (let ([tmp-text (instantiate test:text% ())])
               (send tmp-text insert "equal?")
               tmp-text)]
       [test-showing? false])
      
      (field
       [actual (instantiate (text:hide-caret/selection-mixin test:text%) ())]
       [pass (make-object image-snip% *unknown*)])
      
      (send* actual
        (lock true)
        (hide-caret true))
      (send pass load-file *unknown*)
      
      ;; show-test (boolean? . -> . void?)
      ;; show/hide the test in the display
      (define/public (show-test show?)
        (set! test-showing? show?)
        (send (get-editor) show-test show?))
      
      ;; reset (-> void?)
      ;; resets the result of the test case
      (define/public (reset)
        (set-actual "")
        (send pass load-file *unknown*))
      
      ;; set-actual (string? . -> . void?)
      ;; set the text in the actual field to the string given
      (define/private (set-actual string)
        (send* actual
          (lock false)
          (erase)
          (insert string)
          (lock true)))
      
      ;; set-icon (boolean? . -> . void?)
      ;; set the image of the icon to either pass or fail
      (define/private (set-icon pass?)
        (send pass load-file
              (if pass?
                  *success*
                  *failure*)))
      
      ;; execute ((is-a?/c expand-program%) ((union (id-s?/c snip%) false?) . -> . void?) . -> . void?)
      ;; execute the test case
      ;; status: set-actual should really be called from within the test as a 3D value but I couldn't
      ;;         get it to work
      (define/public (execute expander continue)
        (let ([call-with-test
               (lambda (f)
                 (if test-showing?
                     (send expander expand-text test f)
                     (f (syntax equal?))))])
          (send expander expand-text call
                (lambda (call-syntax)
                  (send expander eval-syntax call-syntax
                        (lambda (call-value)
                          (send expander user-format call-value
                                (lambda (call-string)
                                  (set-actual call-string)
                                  (send expander expand-text expected
                                        (lambda (expected-syntax)
                                          (call-with-test
                                           (lambda (test-syntax)
                                             (send expander eval-syntax
                                                   (with-syntax ([call call-syntax]
                                                                 [expected expected-syntax]
                                                                 [test test-syntax])
                                                     (syntax (#%app test call expected)))
                                                   (lambda (test-value) ; =drscheme-eventspace=
                                                     (set-icon test-value)
                                                     (let ([next-case (next)])
                                                       (if next-case
                                                           (send next-case execute expander continue)
                                                           (continue)))))))))))))))))
      
      ;; write ((is-a?/c editor-stream-out%) . -> . void?)
      ;; write the snip out to the stream
      (rename [super-write write])
      (define/override (write f)
        (send call write-to-file f)
        (send expected write-to-file f)
        (send test write-to-file f)
        (send f put (format "~s" test-showing?)))
      
      (super-instantiate ()
        (stretchable-width true)
        (stretchable-height false)
        (editor (instantiate test-case-editor% ()
                  (call-text call)
                  (expected-text expected)
                  (actual-text actual)
                  (test-text test)
                  (pass-image pass)
                  (test-showing? test-showing?))))
      (set-snipclass tcc)
      ))
  
  (define test-case-editor%
    (class vertical-pasteboard%
      (inherit begin-edit-sequence end-edit-sequence insert)
      (init-field call-text expected-text actual-text test-text pass-image test-showing?)
      
      (field
       [bottom-pb (instantiate horizontal-pasteboard% ())]
       [call-snip (label-box "Call" call-text)]
       [expected-snip (label-box "Expected" expected-text)]
       [actual-snip (label-box "Actual" actual-text)]
       [test-snip (label-box "Equality Test" test-text)])
      
      ;; show-test (boolean? . -> . void?)
      ;; show/hide the test in the display
      ;; status: the insert fails because the snip was once deleted so instead I need
      ;;         to create a new one every time. It's less readable and a bit slower
      (define/public (show-test show?)
        (cond
          [(and test-showing? (not show?))
           (send bottom-pb release-snip test-snip)]
          [(and (not test-showing?) show?)
           (send bottom-pb insert test-snip pass-image)]
          [else (void)])
        (set! test-showing? show?))
      
      (super-instantiate ())
      (send* bottom-pb
        (begin-edit-sequence)
        (insert expected-snip false)
        (insert actual-snip false))
      (when test-showing?
        (send bottom-pb insert test-snip false))
      (send* bottom-pb
        (insert pass-image false)
        (end-edit-sequence))
      
      (begin-edit-sequence)
      (insert call-snip false)
      (insert (instantiate aligned-editor-snip% ()
                (with-border? false)
                (editor bottom-pb)
                (top-margin 0)
                (bottom-margin 0)
                (left-margin 0)
                (right-margin 0))
              false)
      (end-edit-sequence)
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
