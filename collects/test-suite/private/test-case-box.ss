(module test-case-box mzscheme
  
  (provide test-case-box^ test-case-box@)
  
  (require
   (lib "class.ss")
   (lib "list.ss")
   (lib "mred.ss" "mred")
   (lib "unitsig.ss")
   (lib "tool.ss" "drscheme")
   (lib "etc.ss")
   (lib "match.ss")
   (lib "framework.ss" "framework")
   (lib "readerr.ss" "syntax")
   (lib "string-constant.ss" "string-constants")
   (lib "embedded-gui.ss" "embedded-gui")
   (lib "string-constant.ss" "string-constants")
   "make-snipclass.ss"
   "text-syntax-object.ss"
   "print-to-text.ss"
   "test-case.ss")
  
  (define-signature test-case-box^ (test-case-box% phase1 phase2))
  (define test-case-box@
    (unit/sig test-case-box^
      (import drscheme:tool^ text->syntax-object^)
      
      (define test-case:program-editor% false)
      
      (define (phase1) (void))
      (define (phase2)
        (set! test-case:program-editor%
              (tabbable-text-mixin 
               ((drscheme:unit:get-program-editor-mixin)
                scheme:text%))))
      
      ;; The test case box that is inserted into the drscheme frame
      (define test-case-box%
        (class* (decorated-editor-snip-mixin editor-snip%) (readable-snip<%>)
          (inherit get-admin)
          
          ;; A text that will uncollapse the test case when it is highlighted for an error
          (define error-alert-text%
            (class test-case:program-editor%
              (define/override highlight-range
                (opt-lambda (start end color (bitmap false) (caret-space false) (priority 'low))
                  (when collapsed? (collapse false))
                  (super highlight-range start end color bitmap caret-space priority)))
              (super-new)))
          
          (init-field
           [enabled? true]
           [actual-show? false]
           [collapsed? false]
           [to-test (new error-alert-text%)]
           [expected (new error-alert-text%)])
          
          (field
           [actual (new actual-text%)]
           [result (new result-snip%
                        (status (if enabled?
                                    'unknown
                                    'disabled)))])
          
          #;(any? (union integer? false?) (union integer? false?) (union integer? false?) . -> . any?)
          ;; Called by the execution to get a syntax object that represents this box.
          (define/public read-special
            (opt-lambda (source (line false) (column false) (position false))
              #;((is-a?/c text%) . -> . syntax-object?)
              ;; Creates a single syntax object out of the text or raises read-error
              (define (text->syntax-object text)
                (match (text->syntax-objects text)
                  [() (raise-read-error (string-constant test-case-empty-error)
                                        source line #f position 1)]
                  [(stx) stx]
                  [(stx next rest-stx ...)
                   (raise-read-error (string-constant test-case-too-many-expressions-error)
                                     text
                                     (syntax-line next)
                                     (syntax-column next)
                                     (syntax-position next)
                                     (syntax-span next))]))
              (if enabled?
                  (with-syntax ([to-test-stx (text->syntax-object to-test)]
                                [exp-stx (text->syntax-object expected)]
                                [update-stx (lambda (x) (update x))]
                                [set-actuals-stx set-actuals])
                    (syntax/loc (datum->syntax-object
                                 false 'ignored (list source line column position 1))
                      (test-case equal? to-test-stx exp-stx update-stx set-actuals-stx)))
                  (syntax-property #'(define-values () (values)) 
                                   'stepper-skip-completely
                                   #t))))
          
          #;(boolean? . -> . void?)
          ;; sets the test case to the proper result bassed on if it was correct
          (define/public (update pass?)
            (send result update (if pass? 'pass 'fail)))
          
          #;(-> void?)
          ;; resets the state of the test case
          ;; STATUS: Should I use an edit sequence of pb right here?
          (define/public (reset)
            ;; Hiding the actual on reset might be annoying
            #;(show-actual false)
            (send* actual
              (lock false)
              (erase)
              (lock true))
            (when enabled?
              (send result update 'unknown)))
          
          #;(boolean? . -> . void?)
          ;; enables or disables the test case
          (define/public (enable enable?)
            (unless (boolean=? enabled? enable?)
              (if enable?
                  (begin (set! enabled? true)
                         (send result update 'unknown))
                  (begin (set! enabled? false)
                         (reset)
                         (send result update 'disabled)))))
          
          #;(-> void)
          ;; tells the test-box to take the caret
          (define/public (take-caret)
            (send pb set-caret-owner
                  (send (send to-test get-admin) get-snip)
                  'display))
          
          #;((is-a?/c expand-program%) (listof any?) . -> . void?)
          ;; set the text in the actual field to the value given
          (define (set-actuals vals)
            (send (send (get-admin) get-editor) begin-edit-sequence)
            (send actual lock false)
            (print-to-text actual vals)
            (send actual lock true)
            (send (send (get-admin) get-editor) end-edit-sequence))
          
          ;;;;;;;;;;
          ;; Saving and Copying
          
          #;(-> (is-a?/c test-case-box%))
          ;; Called by drscheme to copy and paste this test-case
          (define/override (copy)
            (let ([new-to-test (new test-case:program-editor%)]
                  [new-expected (new test-case:program-editor%)])
              (send to-test copy-self-to new-to-test)
              (send expected copy-self-to new-expected)
              (new test-case-box%
                   (enabled? enabled?)
                   (actual-show? actual-show?)
                   (collapsed? collapsed?)
                   (to-test new-to-test)
                   (expected new-expected))))
          
          #;((is-a?/c editor-stream-out%) . -> . void?)
          ;; Writes this test case box to the given file.
          (define/override (write f)
            (send to-test write-to-file f)
            (send expected write-to-file f)
            (send f put (if enabled? 1 0))
            (send f put (if collapsed? 1 0))
            ;; Don't make actual persistant
            #;(send f put (if actual-show? 1 0)))
          
          
          #;((is-a?/c editor-stream-in%) . -> . void?)
          ;; Reads the state of the box in from the given stream
          (define/override (read-from-file f)
            (let ([enabled?-box (box 0)]
                  [collapsed?-box (box 0)]
                  ;; Don't make actual persistant
                  #;[actual-show?-box (box 0)])
              (send to-test read-from-file f)
              (send expected read-from-file f)
              (send f get enabled?-box)
              (send f get collapsed?-box)
              #;(send f get actual-show?)
              (set! enabled? (unbox enabled?-box))
              (set! collapsed?-box (unbox collapsed?-box))
              #;(set! actual-show?-box (unbox actual-show?-box))))
          
          ;;;;;;;;;;
          ;; Layout
          
          ;STATUS: BEFORE USING THIS CODE REWRITE IT TO USE STRING CONSTANTS
          ;(define/override (get-corner-bitmap)
          ;  (make-object bitmap% (icon "scheme-box.jpg")))
          ;(define/override (get-color) "purple")
          ;(define/override (get-menu)
          ;  (let ([the-menu (new popup-menu% (title "Test Box"))])
          ;    (new menu-item%
          ;         (label "Switch to Error Test Box")
          ;         (parent the-menu)
          ;         (callback (lambda (m e) (void))))
          ;    (new menu-item%
          ;         (label "Roll up Test Box")
          ;         (parent the-menu)
          ;         (callback (lambda (m e) (void))))
          ;    (new checkable-menu-item%
          ;         (label "Enable")
          ;         (parent the-menu)
          ;         (callback (lambda (m e) (void))))
          ;    the-menu))
          ;(define/override (get-position) 'top-right)

          #;(boolean? . -> . void)
          ;; Shows or hides the actual box
          (define (show-actual show?)
            (set! actual-show? show?)
            (send show-actual-button set-state
                  (boolean->show-actual-btn-state show?))
            (send actual-pane show show?))
          
          #;(boolean? . -> . void)
          ;; Toggles the test-case between a collapsed minimal state and one with entry boxes.
          (define (collapse bool)
            (set! collapsed? bool)
            (send collapse-button set-state
                  (boolean->collapse-btn-state bool))
            (send pb lock-alignment true)
            (send left show (not bool))
            (send right show (not bool))
            (send pb lock-alignment false))
            
          #;(boolean? . -> . (symbols 'on 'off))
          ;; converts a boolean to the value expected by the collapse button
          (define (boolean->collapse-btn-state bool)
            (if bool 'on 'off))
          
          #;(boolean? . -> . (symbols 'on 'off))
          ;; converts a boolean to the value expected by the show actual button
          (define (boolean->show-actual-btn-state bool)
            (if bool 'off 'on))
          
          ;;;;;;;;;;
          ;; Box layout
          
          (field
           [pb (new aligned-pasteboard%)]
           [main (new horizontal-alignment% (parent pb))]
           [left (new vertical-alignment%
                      (parent main)
                      (show? (not collapsed?)))]
           [right (new vertical-alignment%
                       (parent main)
                       (show? (not collapsed?)))]
           [button-pane (new vertical-alignment% (parent main))]
           [to-test-pane (new vertical-alignment% (parent left))]
           [expected-pane (new vertical-alignment% (parent right))]
           [actual-pane (new vertical-alignment%
                             (parent right)
                             (show? actual-show?))]
           [collapse-button
            (new turn-button-snip%
                 (state (boolean->collapse-btn-state collapsed?))
                 (turn-off (lambda (b e) (collapse true)))
                 (turn-on (lambda (b e) (collapse false))))]
           [show-actual-button
            (new turn-button-snip%
                 (state (boolean->show-actual-btn-state actual-show?))
                 (turn-off (lambda (b e) (show-actual false)))
                 (turn-on (lambda (b e) (show-actual true))))])
          
          (super-new (editor pb))
          
          #;((is-a?/c alignment<%>) string? (is-a?/c text%) . -> . (is-a?/c alignment<%>))
          ;; Inserts a label and a text field into the given alignment
          (define (labeled-field alignment label text)
            ;; I string-append here to give space after the label
            ;; They look a lot better without something right after them.
            (new snip-wrapper%
                 (snip (make-object string-snip% (string-append label "     ")))
                 (parent alignment))
            (new snip-wrapper%
                 (snip (new stretchable-editor-snip%
                            (editor text)
                            (stretchable-height false)))
                 (parent alignment)))
          
          (labeled-field to-test-pane (string-constant test-case-to-test) to-test)
          (labeled-field expected-pane (string-constant test-case-expected) expected)
          
          (new snip-wrapper%
               (snip (make-object string-snip% (string-constant test-case-actual)))
               (parent actual-pane))
          (new snip-wrapper%
               (snip (new (grey-editor-snip-mixin stretchable-editor-snip%)
                          (editor actual)
                          (stretchable-height false)))
               (parent actual-pane))
          
          (new snip-wrapper%
               (snip result)
               (parent button-pane))
          (new snip-wrapper%
               (snip collapse-button)
               (parent button-pane))
          (new snip-wrapper%
               (snip show-actual-button)
               (parent button-pane))
          
          (set-tabbing to-test expected)
          
          ;;;;;;;;;;
          ;; Snip class

          (inherit set-snipclass)
          (set-snipclass tcb-sc)))
      
      ;;;;;;;;;;
      ;; Snip class
      
      ;; A snip-class for the test case box
      (define tcb-sc (make-snipclass test-case-box% "test-case-box%"))
      ))
  
  #;((-> void?) (-> void?) (symbols 'up 'down) . -> . snip%)
  ;; a snip which acts as a toggle button for rolling a window up and down
  (define turn-button-snip%
    (class toggle-button-snip%
      (super-new
       (images-off (cons (icon "turn-down.gif") (icon "turn-down-click.gif")))
       (images-on (cons (icon "turn-up.gif") (icon "turn-up-click.gif"))))))
  
  ;; a snip which will display a pass/fail result
  (define result-snip%
    (class image-snip%
      (inherit load-file)
      (init-field [status 'unknown])
      ;; ((symbols 'pass 'fail 'unknown 'disabled) . -> . void?)
      ;; updates the image with the icon representing one of three results
      (define/public (update value)
        (load-file
         (test-icon
          (case value
            [(pass) "small-check-mark.jpeg"]
            [(fail) "small-cross.jpeg"]
            [(unknown) "small-empty.gif"]
            [(disabled) "small-no.gif"]))))
      
      (super-new)
      (update status)))
  
  #;(string? . -> . string?)
  ;; A path to the icon given a file name
  (define (icon str)
    (build-path (collection-path "icons") str))
  
  #;(string? . -> . string?)
  ;; A path to the icon in the test-suite given a file name
  (define (test-icon str)
    (build-path (collection-path "test-suite") "private" "icons" str))
  
  ;; a locked text hightlighted to show that it is inactive
  (define actual-text%
    (class (grey-editor-mixin
            (text:hide-caret/selection-mixin scheme:text%))
      (inherit hide-caret lock)
      (super-new)
      (hide-caret true)
      (lock true)))
  )
