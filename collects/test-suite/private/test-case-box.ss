(module test-case-box mzscheme
  
  (provide test-case-box%)
  
  (require
   (lib "class.ss")
   (lib "mred.ss" "mred")
   (lib "etc.ss")
   (lib "aligned-pasteboard.ss" "mrlib")
   ;"aligned-pasteboard.ss"
   (lib "framework.ss" "framework"))
  
  ;; a text-case snip
  (define test-case-box%
    (class* aligned-editor-snip% (readable-snip<%> aligned-pasteboard-parent<%>)
     
      ;; read-one-special (integer? any? (union integer? false?) (union integer? false?) (union integer? false?) . -> .
      ;;                   any? integer? false?)
      ;; status: this contract is incorrectly written
      (define/public read-one-special
        (opt-lambda (index source (line false) (column false) (position false))
          (values
           (with-syntax ([call-stx (text->syntax-object false call)]
                         [exp-stx (text->syntax-object false exp)])
             (syntax (equal? call-stx exp-stx)))
           1
           true)))
      
      ;(define/override (get-color) "red")
      ;(define/override (make-editor) (new test-case-editor%))
      
      (field
       [editor (new vertical-pasteboard%)]
       [top-line (new horizontal-pasteboard%)]
       [turn-button
        (new turn-button-snip%
             (turn-down (lambda () (void)))
             (turn-up (lambda () (void))))]
       [comment (new text%)]
       [result (new result-snip%)]
       [call (new (class scheme:text%
                    (inherit get-max-width)
                    ;(rename [a after-insert])
                    ;(define/override (after-insert b c) (printf "~s~n" (get-max-width)) (a b c))
                    (super-new)))]
       [exp (new scheme:text%)]
       [act (new scheme:text%)])
      
      (send* top-line
        (insert turn-button false)
        (insert (new (aligned-snip-mixin editor-snip%)
                     (editor comment)
                     (with-border? false)
                     (stretchable-height false)
                     (top-margin 0)
                     (bottom-margin 0))
                false)
        (insert result false))
      
      (send editor insert
            (new aligned-editor-snip%
                   (editor top-line)
                   (with-border? false)
                   (stretchable-height false)
                   (top-margin 0)
                   (bottom-margin 0)
                   (left-margin 0)
                   (right-margin 0))
            false)
      
      (for-each
       (lambda (text)
         (send editor insert (text-field text) false))
       (list call exp))
      ;(send editor insert (new editor-snip% (editor act)) false)
      
      (super-new
       (editor editor)
       (stretchable-height false)
       (stretchable-width false))))
  
  ;; ((-> void?) (-> void?) (symbols 'up 'down) . -> . snip%)
  ;; a snip which acts as a toggle button for rolling a window up and down
  (define turn-button-snip%
    (class image-snip%
      (inherit load-file)
      (init-field turn-down turn-up [state 'up])
      (super-new)
      (load-file (build-path (collection-path "icons") "turn-up.gif"))))
  
  ;; ((symbols 'pass 'fail 'unknown) . -> . result-snip%)
  ;; a snip which will display a pass/fail result
  (define result-snip%
    (class image-snip%
      (inherit load-file)
      (init-field [status 'pass])
      ;; ((symbols pass fail unknown)) . -> . void?)
      ;; updates the image with the icon representing one of three results
      (define/public (update value)
        (load-file
         (build-path (collection-path "test-suite" "private" "icons")
                     (case value
                       [(pass) "small-check-mark.jpeg"]
                       [(fail) "small-cross.jpeg"]
                       [(unknown) "empty.jpeg"]))))
      (super-new)
      (update status)))
  
  ;; a text field fit to be in a test-case (no borders or margins etc.)
  (define (text-field text)
    (new (aligned-snip-mixin editor-snip%)
         (editor text)
         ;(with-border? false)
         (stretchable-height false)
         ;(top-margin 0)
         ;(bottom-margin 0)
         ))
  
  ;; (syntax-object? (is-a?/c text%) . -> . syntax-object?)
  ;; a syntax object representing the text with the color of the given object
  (define (text->syntax-object stx text)
    (datum->syntax-object stx (read (open-input-string (send text get-text)))))
  
  ;;;;;;;;;;
  ;; experiments
  
  ;(begin
  ;  (define align? #t)
  ;  (define f (new frame% (label "test") (width 200) (height 200)))
  ;  (define e (new (if align? vertical-pasteboard% pasteboard%)))
  ;  (define c (new (if align? aligned-editor-canvas% editor-canvas%) (editor e) (parent f)))
  ;  (define t (new test-case-box%))
  ;  (send e insert t)
  ;  (send f show #t))
  )