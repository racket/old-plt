#|

saving doesn't work right -- need a snipclass for helper%
(can re-use editor-snip's snipclass?)

|#

(module helper mzscheme
  
  (require
   (lib "class.ss")
   (lib "mred.ss" "mred")
   (lib "etc.ss")
   (lib "aligned-pasteboard.ss" "mrlib")
   (lib "unitsig.ss")
   (lib "tool.ss" "drscheme")
   "signatures.ss"
   "interfaces.ss"
   (lib "framework.ss" "framework"))
  
  (provide helper@)
  
  (define helper@
    (unit/sig helper^
      (import)
      (define helper%
        (class* aligned-editor-snip% (test-suite:item<%>)
          (inherit next get-admin)
          
	  (define/public (show-test b) (void))
	  (define/public (reset) (void))
	  (define/public (lock b) (send prg-text lock b))
          
          ;; these methods are not in the test-suite:item<%> interface -- should they be?
          (define/public (focus-first) (void))
          (define/public (clear-highlighting) 
            (let* ([list (send prg-text get-style-list)]
                   [style (send list find-named-style "Standard")])
              (when style
                (send prg-text change-style
                      style 
                      0
                      (send prg-text last-position)
                      #f))))
          
          ;; execute ((is-a?/c expand-program%) ((union (id-s?/c snip%) false?) . -> . void?) . -> . void?)
          ;; execute the test case
          (define/public (execute expander continue) ; =drscheme-eventspace=
            (send expander eval-text/multiple prg-text
                  (lambda () ; =drscheme-eventspace=
                    (let ([next-case (next)])
                      (if next-case
                          (send next-case execute expander continue)
                          (continue))))))
          
          ;; write ((is-a?/c editor-stream-out%) . -> . void?)
          ;; write the snip out to the stream
          (define/override (write f)
            (send prg-text write-to-file f))
          
          (define/public (get-prg-text) prg-text)
          
          (super-new
           (editor (new vertical-pasteboard%))
           (stretchable-height false))
          
          (inherit get-editor)
          (field [prg-text (new scheme:text%)])
          (send (get-editor) insert 
                (new editor-snip% 
                     (editor prg-text)
                     (left-margin 0)
                     (right-margin 0)
                     (top-margin 0)
                     (bottom-margin 0)
                     (with-border? #f))
                false)
          
          
          (inherit set-snipclass)
          (set-snipclass hsc)))
      
      (define case-snip-class%
        (class snip-class%
          (define/override (read f)
            (let ([hlp (instantiate helper% ())])
              (send (send hlp get-prg-text) read-from-file f)
              hlp))
          (super-instantiate ())))
  
      (define hsc (instantiate case-snip-class% ()))
      (send hsc set-classname "drscheme:test-suite:helper%")
      (send hsc set-version 1)
      (send (get-the-snip-class-list) add hsc))))
