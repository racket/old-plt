(module example-box mzscheme
  
  (require
   (lib "mred.ss" "mred")
   (lib "class.ss")
   (lib "etc.ss")
   (lib "list.ss")
   (lib "embedded-gui.ss" "embedded-gui")
   (lib "match.ss")
   (lib "unitsig.ss")
   (lib "tool.ss" "drscheme")
   (lib "framework.ss" "framework")
   (lib "parser.ss" "profj")
   "box-helpers.ss")
  
  (provide example-box@
           example-box^)
  
  ;; This is wrong but it's a good enough prototype
  (define re:java-id (regexp "[A-Za-z_]+"))
  (define min-field-width 50)

  (define-signature example-box^ (example-box%))
 
  (define example-box@
    (unit/sig example-box^
      (import drscheme:tool^)
      
      (define example-box%
        (class* editor-snip% (readable-snip<%>)
          
          #;(any? (union integer? false?) (union integer? false?) (union integer? false?)
                  . -> .
                  any?)
          
          (define/public read-special
            (opt-lambda (source (line false) (column false) (position false))
              #;(((is-a?/c text%))
                 (natural-number? (union natural-number? false?))
                 . opt-> .
                 id?)
              ;; Make an id out of the given text
              ;; STATUS: I'm parsing the ID with a regexp that's probablly not
              ;; the correct Java variable regexp. Furthermore, I need to parse
              ;; it differently if it's a class name vs. field name.
              (define (text->java-id atext)
                (let ([str (send atext get-text)])
                  (match-let ([((m-start . m-end))
                               (regexp-match-positions
                                re:java-id str 0 false)])
                    (datum->syntax-object
                     false
                     (string->symbol (substring str m-start m-end))
                     (list atext
                           1
                           m-start
                           (add1 m-start)
                           (- m-end m-start))))))
              ;(lambda (level class-loc box-pos input-spec)
              (let ([level 'beginner] [class-loc #f] [box-pos #f] [input-spec #f])
                #`(begin #,@(send examples map-children
                                  (lambda (example)
                                    (with-syntax ([name (text->java-id
                                                         (send example get-name))]
                                                  [value (parse-interactions
                                                          (open-input-text-editor
                                                           (send example get-value))
                                                          (send example get-value)
                                                          level)])
                                      #'(define name value))))))))
          
          (field
           [pb (new aligned-pasteboard%)]
           [main (new vertical-alignment% (parent pb))]
           [header (new horizontal-alignment% (parent main))]
           [icon (new snip-wrapper% (parent header) (snip (make-object image-snip%)))])
           (new embedded-message% (parent header) (label "Examples"))
          (field
           [examples (new examples-field% (parent main))]
           [button-bar (new horizontal-alignment% (parent main))]
           [add-button (new embedded-text-button%
                            (parent button-bar)
                            (label "Add new example")
                            (callback (lambda (b e) (send examples add-new))))])
          
          (super-new (editor pb))
          (send examples add-new)))
  
      (define examples-field%
        (class vertical-alignment%
          (inherit-field head)
          (define/public (map-chidlren f)
            (send head map-to-list f))
          (define/public (add-new)
            (let* ([example (new example% (parent this))]
                   [previous (send example prev)])
              (when (is-a? previous example%)
                (set-tabbing (send previous get-value)
                             (send example get-type)))
              (set-tabbing (send example get-type)
                           (send example get-name)
                           (send example get-value))
              (send (send example get-value) set-ahead (lambda () (add-new))) ; eta
              (send (send example get-type) set-caret-owner false 'global)))
               
          (super-new)))
      
      (define example%
        (class horizontal-alignment%
          (inherit get-parent get-pasteboard)
          
          (define program-editor%
            (tabbable-text-mixin 
             ((drscheme:unit:get-program-editor-mixin)
              scheme:text%)))
          
          (field
           [type (new (single-line-text-mixin program-editor%))]
           [name (new (single-line-text-mixin program-editor%))]
           [value (new program-editor%)])
          
          (define/public (get-type) type)
          (define/public (get-name) name)
          (define/public (get-value) value)
          
          (super-new)
          (send (get-pasteboard) lock-alignment true)
          (new snip-wrapper%
               (parent this)
               (snip (new editor-snip%
                          (editor type)
                          (min-width min-field-width))))
          (new snip-wrapper%
               (parent this)
               (snip (new editor-snip%
                          (editor name)
                          (min-width min-field-width))))
          (new embedded-message% (parent this) (label " = "))
          (new snip-wrapper%
               (parent this)
               (snip (new editor-snip%
                          (editor value)
                          (min-width min-field-width))))
          (new embedded-message% (parent this) (label " ; "))
          (new horizontal-alignment% (parent this)) ; spacer
          (new embedded-text-button%
               (parent this)
               (label "Del")
               (callback (lambda (b e) (send (get-parent) delete-child this))))
          (send (get-pasteboard) lock-alignment false)
          ))
      ))
  )