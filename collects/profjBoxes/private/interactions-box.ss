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
          
          #;(any? (union integer? false?) (union integer? false?) (union integer? false?)
                  . -> .
                  any?)
          
          (define/public read-special
            (opt-lambda (index source (line false) (column false) (position false))
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
                            (filter
                             (lambda (x) (is-a? x interaction%))
                             (send interactions get-children)))))))
          
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
              (let (#;[l (new hline% (parent this))]
                    [i (new interaction% (parent this))])
                (when after
                  (move-after i after)
                  #;(move-after l after)
                  #;(move-after i l))
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
            (let ([blue-text (new style-delta%)])
              (send blue-text set-delta-foreground "blue")
              (send* output-text
                (lock false)
                (erase)
                (change-style blue-text 'start 'end #f)
                (insert (format "~s" val))
              (lock true))
              (send output show true)))
          
          (super-new)
          
          (field [io (new vertical-alignment% (parent this))]
                 [input (new horizontal-alignment% (parent io))])
          (new embedded-message% (label " > ") (parent input))
          (new snip-wrapper%
               (snip (new stretchable-editor-snip%
                          (editor input-text)
                          (stretchable-height false)
                          (with-border? false)
                          (min-width 100)))
               (parent input))
          (new embedded-text-button%
               (parent input)
               (label "Ctrl + Enter")
               (callback (lambda (b e) (send (get-parent) add-new this))))
          
          (field [output (new vertical-alignment% (parent io) (show? false))])
          (new snip-wrapper%
               (snip (new stretchable-editor-snip%
                          (editor output-text)
                          (stretchable-height false)
                          (with-border? false)))
               (parent output))
          ))
      ))
  )