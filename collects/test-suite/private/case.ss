(module case mzscheme
  
  (require
   (lib "class.ss")
   (lib "mred.ss" "mred")
   (lib "etc.ss")
   (lib "framework.ss" "framework")
   (lib "aligned-pasteboard.ss" "mrlib")
   (lib "unitsig.ss")
   (lib "tool.ss" "drscheme")
   "signatures.ss"
   "interfaces.ss"
   "test-text.ss")
  
  (provide
   case@
   case-snipclass@)
  
  (define *unknown* (build-path (collection-path "test-suite") "private" "icons" "question-mark.jpeg"))
  (define *success* (build-path (collection-path "test-suite") "private" "icons" "check-mark.jpeg"))
  (define *failure* (build-path (collection-path "test-suite") "private" "icons" "cross.jpeg"))
  (define *error* (build-path (collection-path "icons") "bug09.gif"))
  
  (define case@
    (unit/sig case^
      (import)
      (define case%
        (class* aligned-editor-snip% (test-suite:item<%>)
          (inherit get-editor next set-snipclass)
          (init-field
           [call (instantiate test:text% ()
                   (parent this))]
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
          ;;         get it to work. will do later.
          (define/public (execute expander continue) ; =drscheme-eventspace=
            (let ([call-with-test
                   (lambda (f) ; =drscheme-eventspace=
                     (if test-showing?
                         (send expander expand-text test f)
                         (f (syntax equal?))))])
              (send expander expand-text call
                    (lambda (call-syntax) ; =drscheme-eventspace=
                      (send expander eval-syntax call-syntax
                            (lambda (call-value) ; =drscheme-eventspace=
                              (send expander user-format call-value
                                    (lambda (call-string) ; =drscheme-eventspace=
                                      (set-actual call-string)
                                      (send expander expand-text expected
                                            (lambda (expected-syntax) ; =drscheme-eventspace=
                                              (call-with-test
                                               (lambda (test-syntax) ; =drscheme-eventspace=
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
          ;; show-test (boolean? . -> . void?)
          ;; show/hide the test in the display
          (define/public (show-test show?)
            (set! test-showing? show?))
          
          (super-instantiate ())
          ))
      ))
      
  (define case-snipclass@
    (unit/sig case^
      (import (super : case^))
      (define case%
        (class* super:case% (test-suite:item<%>)
          (inherit set-snipclass)
          (inherit-field call expected test test-showing?)
          
          ;; write ((is-a?/c editor-stream-out%) . -> . void?)
          ;; write the snip out to the stream
          (rename [super-write write])
          (define/override (write f)
            (send call write-to-file f)
            (send expected write-to-file f)
            (send test write-to-file f)
            (send f put (format "~s" test-showing?)))
          (super-instantiate ())
          (set-snipclass csc)
          ))
          
      (define case-snip-class%
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
              (instantiate case% ()
                (call call-text)
                (expected expected-text)
                (test test-text)
                (test-showing? (string=? "#t" (send f get-string))))))
          (super-instantiate ())
          ))
  
      (define csc (instantiate case-snip-class% ()))
      (send csc set-classname "case%")
      (send csc set-version 1)
      (send (get-the-snip-class-list) add csc)
      ))
  )
