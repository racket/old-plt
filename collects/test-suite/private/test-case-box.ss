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
   (lib "aligned-pasteboard.ss" "mrlib")
   (lib "framework.ss" "framework")
   (lib "readerr.ss" "syntax")
   (lib "string-constant.ss" "string-constants")
   (lib "snip-lib.ss" "mrlib" "private" "aligned-pasteboard")
   "fixed-width-label-snip.ss"
   "grey-editor.ss"
   "button-snip.ss"
   "test-case.ss"
   "tabbable-text.ss")
  
  (define-signature test-case-box^ (test-case-box%))
  (define test-case-box@
    (unit/sig test-case-box^
      (import drscheme:tool^)
      
      (define test-case-box%
        (class* (decorated-editor-snip-mixin aligned-editor-snip%) (readable-snip<%>)
          (inherit get-admin)
          
          (init-field
           [enabled? true]
           [error-box? false]
           [comment (new comment-text%)]
           [to-test (new to-test-text%)]
           [expected (new to-test-text%)])
          
          ;; read-one-special (integer? any? (union integer? false?) (union integer? false?)
          ;;                   (union integer? false?) . -> . any? integer? false?)
          ;; status: this contract is incorrectly written
          (define/public read-one-special
            (opt-lambda (index source (line false) (column false) (position false))
              ;; (syntax-object? (is-a?/c text%) . -> . syntax-object?)
              ;; a syntax object representing the text with the color of the given object
              (define (text->syntax-object text)
                (let ([port (open-input-text-editor text)])
                  (define (read-all-syntax)
                    (let* ([language-settings
                            (preferences:get
                             (drscheme:language-configuration:get-settings-preferences-symbol))]
                           [language
                            (drscheme:language-configuration:language-settings-language
                             language-settings)]
                           [settings
                            (drscheme:language-configuration:language-settings-settings
                             language-settings)])
                      (if (drscheme:language-configuration:language-settings? language-settings)
                          (let ([thunk (send language front-end/interaction
                                             (drscheme:language:make-text/pos
                                              text 0 (send text last-position))
                                             settings
                                             (drscheme:teachpack:new-teachpack-cache '()))])
                            (let loop ([stxs empty])
                              (let ([expr (thunk)])
                                (cond [(eof-object? expr) stxs]
                                      [else (loop (cons expr stxs))]))))
                          (error 'text->syntax-object "Invalid language settings"))))
                    (match (read-all-syntax)
                      [() (raise-read-error (string-constant test-case-empty-error)
                                            source line #f position 1)]
                      [(stx) stx]
                      [(stx next rest-stx ...)
                       (raise-read-error (string-constant test-case-too-many-expressions-error)
                                         text
                                         (syntax-line next)
                                         (syntax-column next)
                                         (syntax-position next)
                                         (syntax-span next))])))
                
              (values
               (if enabled?
                   (with-syntax ([to-test-stx (text->syntax-object to-test)]
                                 [exp-stx (text->syntax-object expected)]
                                 [update-stx (lambda (x) (update x))]
                                 [set-actuals-stx set-actuals])
                     (syntax/loc (datum->syntax-object false 'ignored (list source line column position 1))
                       (test-case equal? to-test-stx exp-stx update-stx set-actuals-stx)))
                   (syntax-property #'(define-values () (values)) 
                                    'stepper-skip-completely
                                    #t))
               1
               true)))
          
          ;; (boolean? . -> . void?)
          ;; sets the test case to the proper view
          (define/public (update pass?)
            (send result update (if pass? 'pass 'fail)))
          
          ;; (-> void?)
          ;; resets the state of the test case
          (define/public (reset)
            (send* actual
              (lock false)
              (erase)
              (lock true))
            (when enabled?
              (send result update 'unknown)))
          
          ;; enable (boolean? . -> . void?)
          ;; enables or disables the test case
          (define/public (enable enable?)
            (unless (boolean=? enabled? enable?)
              (if enable?
                  (begin (set! enabled? true)
                         (send result update 'unknown))
                  (begin (set! enabled? false)
                         (reset)
                         (send result update 'disabled)))))
          
          ;; set-actuals ((is-a?/c expand-program%) (listof any?) . -> . void?)
          ;; set the text in the actual field to the value given
          (define (set-actuals vals)
            (unless (empty? vals)
              (send* actual
                (lock false)
                (begin-edit-sequence)
                (erase))
              (send (send (get-admin) get-editor) begin-edit-sequence)
              (let ([port
                     (make-custom-output-port
                      false
                      (lambda (s start end block?)
                        (send actual insert s)
                        (string-length s))
                      void
                      void)])
                (print (first vals) port)
                (for-each
                 (lambda (val)
                   (newline port)
                   (print val port))
                 (rest vals)))
              (send (send (get-admin) get-editor) end-edit-sequence)
              (send* actual
                (end-edit-sequence)
                (lock true))))
          
          ;;;;;;;;;;
          ;; Saving and Copying
          
          ;; this is incomplete. I'll fix it when insertion works.
          (define/override (copy)
            (let ([new-comment (new comment-text%)]
                  [new-to-test (new to-test-text%)]
                  [new-expected (new to-test-text%)])
              (send comment copy-self-to new-comment)
              (send to-test copy-self-to new-to-test)
              (send expected copy-self-to new-expected)
              (new test-case-box%
                   (enabled? enabled?)
                   (error-box? error-box?)
                   (comment new-comment)
                   (to-test new-to-test)
                   (expected new-expected))))
          
          (define/override (write f)
            ;; must write the enabled? and error-box? fields
            (send comment write-to-file f)
            (send to-test write-to-file f)
            (send expected write-to-file f))
          
          (define/public (read-from-file f)
            ;; must read the enabled? and error-box? fields
            (send* comment
              (erase)
              (read-from-file f))
            (send* to-test
              (erase)
              (read-from-file f))
            (send* expected
              (erase)
              (read-from-file f)))
          
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
          
          ;; tells the test-box to take the caret
          (define/public (take-caret)
            (send editor set-caret-owner top-line 'global)
            (send (send top-line get-editor) set-caret-owner
                  (send (send comment get-admin) get-snip)))
          
          (define (hide-entries)
            (send* editor
              (lock false)
              (begin-edit-sequence)
              (release-snip to-test-line)
              (release-snip exp-line)
              (release-snip act-line)
              (end-edit-sequence)
              (lock true)))
          
          (define (show-entries)
            (send* editor
              (lock false)
              (begin-edit-sequence)
              (insert to-test-line false)
              (insert exp-line false)
              (insert act-line false)
              (end-edit-sequence)
              (lock true)))
          
          (field
           [editor (new vertical-pasteboard%)]
           [turn-button
            (new turn-button-snip%
                 (turn-down hide-entries)
                 (turn-up show-entries))]
           [result (new result-snip%
                        (status (if enabled? 'unknown 'disabled)))]
           [actual (new actual-text%)]
           [top-line (make-top-line turn-button comment result)]
           [to-test-line (make-line (string-constant test-case-to-test) to-test)]
           [exp-line (make-line (string-constant test-case-expected) expected)]
           [act-line (make-line (string-constant test-case-actual) actual
                                (grey-editor-snip-mixin editor-snip%))])
          
          (set-tabbing comment to-test expected)
          
          (send* editor
            (insert top-line)
            (insert to-test-line false)
            (insert exp-line false)
            (insert act-line false)
            (lock true))
          
          (super-new
           (editor editor)
           (stretchable-height false)
           (stretchable-width false))
          
          (inherit set-snipclass)
          (set-snipclass tcb-sc)))
      
      (define test-case-box-snipclass%
        (class snip-class%
          (define/override (read f)
            (let ([case (new test-case-box%)])
              (send case read-from-file f)
              case))
          (super-new)))
      
      (define tcb-sc (new test-case-box-snipclass%))
      (send tcb-sc set-classname "test-case-box%")
      (send tcb-sc set-version 1)
      (send (get-the-snip-class-list) add tcb-sc)
      
      ;; Notes: This code can be replaced by drscheme:unit:program-editor-mixin when I figure out how
      ;;        to make the results of the test case boxes be reset when (and only when) highlighting
      ;;        is being reset.
      (define (clear-results-program-editor-mixin %)
        (class %
          (inherit get-admin begin-edit-sequence end-edit-sequence)
          (rename [super-after-insert after-insert]
                  [super-after-delete after-delete])
          (define (get-frame)
            ;; gets the top most editor in the tree of snips and editors
            (define (editor-root ed)
              (let ([parent (editor-parent ed)])
                (cond
                  [(is-a? parent area<%>) parent]
                  [(is-a? parent snip%)
                   (editor-root (snip-parent parent))]
                  [else false])))
            
            ;; gets the canvas or snip that the pasteboard is displayed in
            ;; status: what if there is more than one canvas?
            (define (editor-parent ed)
              (let ([admin (send ed get-admin)])
                (cond
                  [(is-a? admin editor-snip-editor-admin<%>)
                   (send admin get-snip)]
                  [(is-a? admin editor-admin%)
                   (send ed get-canvas)]
                  [else false])))
            
            (let ([er (editor-root this)])
              (if er
                  (send er get-top-level-window)
                  false)))
          
          (define (alert-of-modify)
            (let ([frame (get-frame)])
              (when frame
                (send (send frame get-interactions-text) reset-highlighting)
                (send* (send frame get-definitions-text)
                  (set-modified true)
                  (reset-test-case-boxes)))))

          ;(rename [super-on-insert on-insert])
          ;(define/override (on-insert start len)
          ;  (begin-edit-sequence)
          ;  (super-on-insert start len)
          ;  (end-edit-sequence))

          (define/override (after-insert start len)
            (alert-of-modify)
            ;(begin-edit-sequence)
            (super-after-insert start len)
            ;(end-edit-sequence)
            )
          (define/override (after-delete start len)
            (alert-of-modify)
            (super-after-delete start len))
          (super-new)))
  
      (define comment-text% (tabbable-text-mixin (editor:keymap-mixin text:basic%)))
      (define to-test-text% (tabbable-text-mixin (clear-results-program-editor-mixin scheme:text%)))
      ))
  
  ;; the top line of the test-case
  ;; STATUS: the typing field should stretch to the width of the test-case
  (define (make-top-line turn-snip comment result-snip)
    (let ([pb (new horizontal-pasteboard%)])
      (send* pb
        (insert turn-snip false)
        (insert (new stretchable-editor-snip%
                     (min-width 100)
                     (editor comment))
                false)
        (insert result-snip false)
        (lock true))
      (new aligned-editor-snip%
           (with-border? false)
           (stretchable-height false)
           (top-margin 0)
           (bottom-margin 0)
           (left-margin 0)
           (right-margin 0)
           (editor pb))))
  
  ;; a line labeled with the given string and containing a given text
  (define make-line
    (opt-lambda (str text (snipclass editor-snip%))
      (let ([pb (new horizontal-pasteboard%)])
        (send* pb
          (insert (field-label str) false)
          (insert (text-field text snipclass) false)
          (lock true))
        (new aligned-editor-snip%
             (with-border? false)
             (top-margin 0)
             (bottom-margin 0)
             (right-margin 0)
             (left-margin 0)
             (editor pb)))))
  
  ;; ((-> void?) (-> void?) (symbols 'up 'down) . -> . snip%)
  ;; a snip which acts as a toggle button for rolling a window up and down
  (define turn-button-snip%
    (class toggle-button-snip%
      (init-field turn-down turn-up)
      (super-new
       (images1 (cons (icon "turn-down.gif") (icon "turn-down-click.gif")))
       (images2 (cons (icon "turn-up.gif") (icon "turn-up-click.gif")))
       (callback1 (lambda (b e) (turn-down)))
       (callback2 (lambda (b e) (turn-up))))))
  
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
  
  ;; a text field fit to be in a test-case (no borders or margins etc.)
  ;; STATUS: this should really return an stretchable-snip<%> not an editor-snip% of fixed size.
  (define text-field
    (opt-lambda (text (snipclass editor-snip%))
      (new (stretchable-editor-snip-mixin snipclass)
           (editor text))))
  
  ;; a snip to label a text case field
  ;; STATUS: This code breaks single point of control for the names of the text fields.
  (define (field-label str)
    (new (fixed-width-label-snip (list (string-constant test-case-to-test)
                                       (string-constant test-case-expected)
                                       (string-constant test-case-actual)))
         (label str)
         (top-margin 0)
         (bottom-margin 0)
         (left-margin 0)
         (right-margin 0)))
  
  (define (icon str)
    (build-path (collection-path "icons") str))
  
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
  
  ;;;;;;;;;;
  ;; tests

  ;(define f (new frame% (label "test") (width 200) (height 200)))
  ;(define e (new text%))
  ;(define c (new editor-canvas% (editor e) (parent f)))
  ;(define t (new test-case-box%))
  ;(send e insert t)
  ;(send f show #t)
  )