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
        (class* aligned-editor-snip% (readable-snip<%>)
          (inherit get-admin)
          
          (field
           [comment (new (tabbable-text-mixin (editor:keymap-mixin text:basic%)))]
           [call (new (tabbable-text-mixin (clear-results-program-editor-mixin scheme:text%)))]
           [expected (new (tabbable-text-mixin (clear-results-program-editor-mixin scheme:text%)))]
           [enabled? true])
          
          ;; read-one-special (integer? any? (union integer? false?) (union integer? false?)
          ;;                   (union integer? false?) . -> . any? integer? false?)
          ;; status: this contract is incorrectly written
          (define/public read-one-special
            (opt-lambda (index source (line false) (column false) (position false))
              ;; (syntax-object? (is-a?/c text%) . -> . syntax-object?)
              ;; a syntax object representing the text with the color of the given object
              (define (text->syntax-object text)
                (let ([port (open-input-text-editor text)])
                  (letrec ([read-all-syntax
                            (lambda ()
                              (let ([stx (read-syntax text port)])
                                (if (eof-object? stx)
                                    (begin (close-input-port port)
                                           empty)
                                    (cons stx (read-all-syntax)))))])
                    (match (read-all-syntax)
                      [() (raise-read-error "Empty test case" source line #f position 1)]
                      [(stx) stx]
                      [(stx next rest-stx ...)
                       (raise-read-error "Too many expressions in a test case."
                                         text
                                         (syntax-line next)
                                         (syntax-column next)
                                         (syntax-position next)
                                         (syntax-span next))]))))
              (values
               (if enabled?
                   (with-syntax ([call-stx (text->syntax-object call)]
                                 [exp-stx (text->syntax-object expected)]
                                 [record (lambda (bool) (update (if bool 'pass 'fail)))]
                                 [set-actuals set-actuals])
                     #'(test-case equal? call-stx exp-stx record set-actuals))
                   #'(define-values () (values)))
               1
               true)))
          
          ;; update ((symbols 'pass 'fail 'unknown) . -> . void?)
          ;; sets the test case to the proper view
          (define/public (update status)
            (send result update status)
            (cond [(symbol=? status 'unknown)
                   (send actual erase)]
                  [else (void)]))
          
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
          (define/override (copy) (new test-case-box%))
          
          (define/override (write f)
            (send comment write-to-file f)
            (send call write-to-file f)
            (send expected write-to-file f))
          
          (define/public (read-from-file f)
            (send* comment
              (erase)
              (read-from-file f))
            (send* call
              (erase)
              (read-from-file f))
            (send* expected
              (erase)
              (read-from-file f)))
          
          ;;;;;;;;;;
          ;; Layout
          
          (set-tabbing comment call expected)
          
          (define (hide-entries)
            (send* editor
              (begin-edit-sequence)
              (release-snip call-line)
              (release-snip exp-line)
              (release-snip act-line)
              (end-edit-sequence)))
          
          (define (show-entries)
            (send* editor
              (begin-edit-sequence)
              (insert call-line false)
              (insert exp-line false)
              (insert act-line false)
              (end-edit-sequence)))
          
          (field
           [editor (new vertical-pasteboard%)]
           [turn-button
            (new turn-button-snip%
                 (turn-down hide-entries)
                 (turn-up show-entries))]
           [enable-button
            (new enable-button-snip%
                 (enable (lambda () (set! enabled? true)))
                 (disable (lambda () (set! enabled? false))))]
           [result (new result-snip%)]
           [actual (new actual-text%)]
           [top-line (make-top-line turn-button enable-button comment result)]
           [call-line (make-line "Test" call)]
           [exp-line (make-line "Expected" expected)]
           [act-line (make-line "Actual" actual
                                (grey-editor-snip-mixin editor-snip%))])
          
          (send editor insert top-line)
          (show-entries)
          
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
          (inherit get-admin)
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
                  [else (error 'editor-root "No root ~s" parent)])))
            
            ;; gets the canvas or snip that the pasteboard is displayed in
            ;; status: what if there is more than one canvas?
            (define (editor-parent ed)
              (let ([admin (send ed get-admin)])
                (cond
                  [(is-a? admin editor-snip-editor-admin<%>)
                   (send admin get-snip)]
                  [(is-a? admin editor-admin%)
                   (send ed get-canvas)]
                  [else (error 'editor-parent "No parent")])))
            
            (send (editor-root this) get-top-level-window))
          
          (define (alert-of-modify)
            (let ([frame (get-frame)])
              (send (send frame get-interactions-text) reset-highlighting)
              (send* (send frame get-definitions-text)
                (set-modified true)
                (reset-test-case-boxes))))
          (define/override (after-insert start len)
            (alert-of-modify)
            (super-after-insert start len))
          (define/override (after-delete start len)
            (alert-of-modify)
            (super-after-delete start len))
          (super-new)))
      ))
  
  ;; the top line of the test-case
  (define (make-top-line turn-snip enable-snip comment result-snip)
    (let ([pb (new horizontal-pasteboard%)])
      (send* pb
        (insert turn-snip false)
        (insert enable-snip false)
        (insert (new editor-snip%
                     (editor comment)
                     (min-width 530))
                false)
        (insert result-snip false))
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
          (insert (text-field text snipclass) false))
        (new aligned-editor-snip%
             (with-border? false)
             (top-margin 0)
             (bottom-margin 0)
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
  
  ;; a snip which allows you to enable or disable a test case
  (define enable-button-snip%
    (class toggle-button-snip%
      (init-field enable disable)
      (super-new
       (images1 (cons (test-icon "checkbox_enabled_checked.gif")
                      (test-icon "checkbox_enabled_checked.gif")))
       (images2 (cons (test-icon "checkbox_enabled_notchecked.gif")
                      (test-icon "checkbox_enabled_notchecked.gif")))
       (callback1 (lambda (b e) (disable)))
       (callback2 (lambda (b e) (enable))))))
  
  ;; ((symbols 'pass 'fail 'unknown) . -> . result-snip%)
  ;; a snip which will display a pass/fail result
  (define result-snip%
    (class image-snip%
      (inherit load-file)
      (init-field [status 'unknown])
      ;; ((symbols pass fail unknown)) . -> . void?)
      ;; updates the image with the icon representing one of three results
      (define/public (update value)
        (load-file
         (test-icon
          (case value
            [(pass) "small-check-mark.jpeg"]
            [(fail) "small-cross.jpeg"]
            [(unknown) "empty.jpeg"]))))
      (super-new)
      (update status)))
  
  ;; a text field fit to be in a test-case (no borders or margins etc.)
  ;;STATUS: this should really return an aligned-snip<%> not an editor-snip% of fixed size.
  (define text-field
    (opt-lambda (text (snipclass editor-snip%))
      (new snipclass
           (editor text)
           (min-width 500))))
  
  ;; a snip to label a text case field
  ;; STATUS: This code breaks single point of control for the names of the text fields.
  (define (field-label str)
    (new (fixed-width-label-snip '("Test" "Expected" "Actual"))
         (label str)
         (top-margin 0)
         (bottom-margin 0)))
  
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
  
;  (define align? #t)
;  (define f (new frame% (label "test") (width 200) (height 200)))
;  (define e (new (if align? vertical-pasteboard% pasteboard%)))
;  (define c (new (if align? aligned-editor-canvas% editor-canvas%) (editor e) (parent f)))
;  (define t (new test-case-box%))
;  (send t resize 500 100)
;  (send e insert t)
;  (send f show #t)
  )