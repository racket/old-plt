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
   (lib "readerr.ss" "syntax")
   (lib "parser.ss" "profj")
   (lib "text-syntax-object.ss" "test-suite" "private"))
  
  (provide interactions-box@
           interactions-box^)
  
  (define-signature interactions-box^ (interactions-box%))
  
  (define interactions-box@
    (unit/sig interactions-box^
      (import drscheme:tool^ text->syntax-object^)
      
      (define interactions-box%
        (class* editor-snip% (readable-snip<%>)
          
          #;(any? (union integer? false?) (union integer? false?) (union integer? false?) . -> . any?)
          (define/public read-special
            (opt-lambda (index source (line false) (column false) (position false))
              #;((is-a?/c text%) . -> . syntax-object?)
              (define (text->syntax-object text)
                (match (text->syntax-objects text)
                  [() (raise-read-error "Empty box"
                                        source line #f position 1)]
                  [(stx) stx]
                  [(stx next rest-stx ...)
                   (raise-read-error "Too many expressions"
                                     text
                                     (syntax-line next)
                                     (syntax-column next)
                                     (syntax-position next)
                                     (syntax-span next))]))
              ;(lambda (level class-loc box-pos input-spec)
              (let ([level 'beginner] [class-loc #f] [box-pos #f] [input-spec #f])
                #`(begin
                    #,@(send interactions map-children
                             (lambda (interaction)
                               (if (is-a? interaction interaction%)
                                   (with-syntax ([display-output
                                                  (lambda (value)
                                                    (send interaction display-output value))])
                                     #`(display-output
                                        #,(text->syntax-object (send interaction get-input))
                                        ;#,(parse-interactions
                                        ;   (open-input-text-editor (send interaction get-input))
                                        ;   (send interaction get-input)
                                        ;   level)
                                        ))
                                   #'(void))))))))
          
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
          (inherit get-pasteboard)
          (inherit-field head)
          (define/public (map-children f)
            (send head map-to-list f))
          (define/public add-new
            (opt-lambda ((after false))
              (send (get-pasteboard) lock-alignment true)
              (let (#;[l (new hline% (parent this))]
                    [i (new interaction% (parent this) (after after))])
                (send (get-pasteboard) lock-alignment false)
                (send (send i get-input) set-caret-owner false 'global))))
          (super-new)))
      
      (define interaction%
        (class horizontal-alignment%
          (inherit get-parent)
          
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
          
          (define program-editor%
            ((drscheme:unit:get-program-editor-mixin)
             (interaction-text this)))
          
          (field [input-text (new program-editor%)]
                 [output-text (new text%)])
          
          (define/public (make-new) (ctrl-enter #f #f))
          (define (ctrl-enter b e) (send (get-parent) add-new this))
          
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
               (callback ctrl-enter))
          
          (field [output (new vertical-alignment% (parent io) (show? false))])
          (new snip-wrapper%
               (snip (new stretchable-editor-snip%
                          (editor output-text)
                          (stretchable-height false)
                          (with-border? false)))
               (parent output))
          ))
      
      (define (interaction-text interaction)
        (class scheme:text%
          
          (define (goto inter)
            (when (is-a? inter interaction%)
              (let ([text (send inter get-input)])
                (send text set-caret-owner false 'global))))
      
          (field [movement-keymap (make-object keymap%)])
          
          (send* movement-keymap
            (add-function "goto-next-interaction"
                          (lambda (ignored event)
                            (goto (send interaction next))))
            (map-function ":c:right" "goto-next-interaction")
            (add-function "goto-prev-interaction"
                          (lambda (ignored event)
                            (goto (send interaction prev))))
            (map-function ":c:left" "goto-prev-interaction")
            (add-function "make-new"
                          (lambda (ignored event)
                            (send interaction make-new)))
            (map-function ":c:return" "make-new"))
          
          #;(-> (listof keymap%))
          ;; the list of keymaps associated with this text
          (define/override (get-keymaps)
            (cons movement-keymap (super get-keymaps)))
          (super-new)
          ))
      ))
  )