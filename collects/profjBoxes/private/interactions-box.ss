(module interactions-box mzscheme
  
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
   (lib "parser.ss" "profj"))
  
  (provide interactions-box@
           interactions-box^)
  
  (define-signature interactions-box^ (interactions-box%))
  
  (define interactions-box@
    (unit/sig interactions-box^
      (import drscheme:tool^)
      
      (define interactions-box%
        (class* editor-snip% (readable-snip<%>)
          
          #;((integer? any?)
             ((union integer? false?) (union integer? false?) (union integer? false?))
             . opt-> .
             (values any? integer? false?))
          
          (define/public read-one-special
            (opt-lambda (index source (line false) (column false) (position false))
              (values
               ;(lambda (level class-loc box-pos input-spec)
               (let ([level 'beginner] [class-loc #f] [box-pos #f] [input-spec #f])
                 #`(begin
                     #,@(map (lambda (interaction)
                               (with-syntax ([display-output
                                              (lambda (value)
                                                (send interaction display-output value))])
                                 #`(display-output
                                    #,(parse-interactions
                                       (open-input-text-editor (send interaction get-input))
                                       (send interaction get-input)
                                       level))))
                             (send interactions get-children))))
               1
               true)))
          
          (field
           [pb (new aligned-pasteboard%)]
           [main (new vertical-alignment% (parent pb))]
           [header (new horizontal-alignment% (parent main))])
          
          (new horizontal-alignment% (parent header)) ; left spacer
          (new snip-wrapper% (snip (make-object string-snip% "Interactions")) (parent header))
          (new horizontal-alignment% (parent header)) ; right spacer
          
          (field [interactions (new interactions-field% (parent main))])
          
          (super-new (editor pb))
          (send interactions add-new)))
      
      (define interactions-field%
        (class vertical-alignment%
          (inherit get-pasteboard move-after)
          (define/public add-new
            (opt-lambda ((after false))
              (send (get-pasteboard) lock-alignment true)
              (let ([i (new interaction% (parent this))])
                (when after (void) #;(move-after i after))
                (send (get-pasteboard) lock-alignment false)
                (send (send i get-input) set-caret-owner false 'global))))
          (super-new)))
      
      (define interaction%
        (class horizontal-alignment%
          (inherit get-parent)
          
          (define program-editor%
            (tabbable-text-mixin 
             ((drscheme:unit:get-program-editor-mixin)
              scheme:text%)))
          
          (field [input-text (new program-editor%)]
                 [output-text (new text%)])
          
          #;(-> (is-a?/c text%))
          ;; The input of this interaction
          (define/public (get-input) input-text)
          
          #;(-> void?)
          ;; Resets the interaction to an inital state
          (define/public (reset)
            (send* output-text
              (lock false)
              (erase)
              (lock true))
            (send output show false))
          
          #;(string? . -> . void?)
          ;; Sets the output to the given value
          (define/public (display-output val)
            (send* output-text
              (lock false)
              (erase)
              (insert (format "~s" val))
              (lock true))
            (send output show true))
          
          (super-new)
          
          (new embedded-message% (label " > ") (parent this))
          (field [io (new vertical-alignment% (parent this))]
                 [input (new vertical-alignment% (parent io))]
                 [output (new vertical-alignment% (parent io) (show? false))])
          (new snip-wrapper%
               (snip (new stretchable-editor-snip%
                          (editor input-text)
                          (stretchable-height false)))
               (parent input))
          (new snip-wrapper%
               (snip (new stretchable-editor-snip%
                          (editor output-text)
                          (stretchable-height false)))
               (parent output))
          (new embedded-text-button%
               (parent this)
               (label "Ctrl + Enter")
               (callback (lambda (b e) (send (get-parent) add-new this))))
          ))
      ))
  )