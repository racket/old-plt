(module case mzscheme
  
  (require
   (lib "class.ss")
   (lib "mred.ss" "mred")
   (lib "etc.ss")
   (lib "aligned-pasteboard.ss" "mrlib")
   (lib "unitsig.ss")
   (lib "tool.ss" "drscheme")
   "signatures.ss"
   "interfaces.ss"
   "test-text.ss"
   (lib "framework.ss" "framework")
   (lib "text-string-style-desc.ss" "mrlib"))
  
  (provide
   case@
   case-snipclass@)

  (define *unknown* (build-path (collection-path "test-suite") "private" "icons" "question-mark.jpeg"))
  (define *success* (build-path (collection-path "test-suite") "private" "icons" "check-mark.jpeg"))
  (define *failure* (build-path (collection-path "test-suite") "private" "icons" "cross.jpeg"))
  ;(define *error* (build-path (collection-path "icons") "bug09.gif"))
  
  (define original-output-port (current-output-port))
  (define (oprintf . args) (apply fprintf original-output-port args))
  
  (define case@
    (unit/sig case^
      (import drscheme:tool^)
      (define case%
        (class* aligned-editor-snip% (test-suite:item<%>)
          (inherit next get-admin)
          
          (init-field [test-showing? false])
          
          (field
           [call (instantiate call-text% () (case this))]
           [call-io (instantiate text:hide-caret/selection%  ())]
           [expected (instantiate expected-text% () (case this))]
           [expected-io (instantiate text:hide-caret/selection%  ())]
           [test (instantiate test-text% () (case this))]
           [actual (instantiate actual-text% () (case this))]
           [pass (make-object image-snip% *unknown*)])
          
          (let ([init-io-text
                 (lambda (io-text)
                   (send io-text set-styles-sticky #f)
                   (send io-text lock #t))])
            (init-io-text call-io)
            (init-io-text expected-io))
          
          (send pass load-file *unknown*)
          
          ;; reset (-> void?)
          ;; resets the result of the test case
          (define/public (reset)
            (send actual erase)
            (reset-io-text call-io)
            (reset-io-text expected-io)
            (send pass load-file *unknown*))

          ;; reset-io-text : -> void
          (define (reset-io-text io-text)
            (send io-text begin-edit-sequence)
            (send io-text lock #f)
            (send io-text erase)
            (send io-text lock #t)
            (send io-text end-edit-sequence))
          
          ;; set-actuals ((is-a?/c expand-program%) (listof any?) . -> . void?)
          ;; set the text in the actual field to the value given
          (define/private (set-actuals expander values)
            (send* actual
              (lock false)
              (begin-edit-sequence)
              (erase))
            (send (send (get-admin) get-editor) begin-edit-sequence)
            (unless (andmap void? values)
              (let* ([lang (send expander get-language)]
                     [last false]
                     [port
                      (make-custom-output-port
                       false
                       (lambda (s start end block?)
                         (when last (send actual insert last))
                         (set! last s)
                         (string-length s))
                       void 
                       void)])
                (for-each
                 (lambda (value)
                   (send (drscheme:language-configuration:language-settings-language lang)
                         render-value/format
                         value
                         (drscheme:language-configuration:language-settings-settings lang)
                         port
                         (lambda (snip) (send actual insert snip))
                         false))
                 values)
                (unless (equal? "\n" last)
                  (send actual insert last))))
            (send (send (get-admin) get-editor) end-edit-sequence)
            (send* actual
              (end-edit-sequence)
              (lock true)))
          
          ;; set-icon (boolean? . -> . void?)
          ;; set the image of the icon to either pass or fail
          (define/private (set-icon pass?)
            (send pass load-file (if pass? *success* *failure*)))
          
          ;; clear-highlighting (-> void?)
          ;; clear any highlighting from the text boxes
          (define/public (clear-highlighting)
            (for-each
             (lambda (text)
               (let* ([list (send text get-style-list)]
                      [style (send list find-named-style "Standard")])
                 (when style
                   (send text change-style
                         style 0 (send text last-position) #f))))
             (list call expected test)))
          
          ;; lock (boolean? . -> . void?)
          ;; lock or unlock the test case for modification
          (define/public (lock lock?)
            (send call lock lock?)
            (send expected lock lock?)
            (send test lock lock?))
          
          ;; execute ((is-a?/c expand-program%) ((union (id-s?/c snip%) false?) . -> . void?) . -> . void?)
          ;; execute the test case
          (define/public (execute expander continue) ; =drscheme-eventspace=
            (let ([call-with-test
                   (lambda (f) ; =drscheme-eventspace=
                     (if test-showing?
                         (send expander eval-text test f)
                         (f (syntax equal?))))])
              (let-values ([(call-output-port call-error-port) (send expander make-ports call-io
                                                                     (lambda ()
                                                                       (send this show-call-io)))]
                           [(expected-output-port expected-error-port) (send expander make-ports expected-io 
                                                                             (lambda ()
                                                                               (send this show-expected-io)))])
                (send expander eval-text/values/ports call
                      (lambda (call-values) ; =drscheme-eventspace=
                        (set-actuals expander call-values)
                        (send expander eval-text/values/ports expected
                              (lambda (expected-values) ; =drscheme-eventspace=
                                (call-with-test
                                 (lambda (test-value) ; =drscheme-eventspace=
                                   (if (= (length call-values)
                                          (length expected-values))
                                       (send expander eval-stx
                                             (with-syntax ([test-value test-value]
                                                           [(call-values ...) call-values]
                                                           [(expected-values ...) expected-values])
                                               (syntax 
                                                (and (test-value 'call-values 'expected-values) ...)))
                                             (lambda (result) ; =drscheme-eventspace=
                                               (set-icon 
                                                (and result
                                                     (equal? 
                                                      (get-string/style-desc call-io)
                                                      (get-string/style-desc expected-io))))))
                                       (set-icon #f))
                                   (let ([next-case (next)])
                                     (if next-case
                                         (send next-case execute expander continue)
                                         (continue))))))
                              expected-output-port
                              expected-error-port))
                      call-output-port
                      call-error-port))))

          ;; show-test (boolean? . -> . void?)
          ;; show/hide the test in the display
          (define/public (show-test show?)
            (set! test-showing? show?))
          
          (super-instantiate ())))))
    
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
          
          ;; read-from-file ((is-a?/c editor-stream-in%) . -> . void?)
          ;; read saved information form a file
          (define/public (read-from-file f)
            (send* call
              (erase)
              (read-from-file f))
            (send* expected
              (erase)
              (read-from-file f))
            (send* test
              (erase)
              (read-from-file f))
            (set! test-showing? (string=? "#t" (send f get-string))))
          
          (super-instantiate ())
          (set-snipclass csc)
          ))
          
      (define case-snip-class%
        (class snip-class%
          ;; read ((is-a?/c editor-stream-in%) . -> . snip%)
          ;; read a snip from the stream
          (define/override (read f)
            (let ([case (instantiate case% ())])
              (send case read-from-file f)
              case))
          (super-instantiate ())
          ))
  
      (define csc (instantiate case-snip-class% ()))
      (send csc set-classname "case%")
      (send csc set-version 1)
      (send (get-the-snip-class-list) add csc)
      ))
  )