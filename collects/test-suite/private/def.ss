(module def mzscheme
  
  (require
   (lib "class.ss")
   (lib "mred.ss" "mred")
   (lib "etc.ss")
   (lib "framework.ss" "framework")
   (lib "aligned-pasteboard.ss" "mrlib")
   (lib "unitsig.ss")
   (lib "tool.ss" "drscheme")
   "signatures.ss"
   "interfaces.ss"
   "test-text.ss")
  
  (provide def@)
  
  (define def@
    (unit/sig def^
      (import)
      (define def%
        (class* aligned-editor-snip% (test-suite:item<%>)
          (inherit get-editor)
          ;; show-test (boolean? . -> . void?)
          ;; how/hide the test in the display
          (define/public (show-test show?)
            (void))
          
          ;; reset (-> void?)
          ;; resets the result of the test case
          (define/public (reset)
            (void))
          
          ;; lock (boolean? . -> . void?)
          ;; lock or unlock the test case for modification
          (define/public (lock lock?)
            (void))

          ;; execute ((is-a?/c expand-program%) ((union (id-s?/c snip%) false?) . -> . void?) . -> . void?)
          ;; execute the item
          (define/public (execute expander next)
            (send expander expand-text (get-editor)
                  (lambda (def-syntax)
                    (send expander eval-stx def-syntax next))))
          
          (super-instantiate ()
            (editor (instantiate def-text% ())))
          ))
      
      (define def-snip-class%
        (class snip-class%
          ;; read ((is-a?/c editor-stream-in%) . -> . void?)
          ;; read a snip from the stream
          (define/override (read f)
            (let ([t (instantiate def-text% ())])
              (send t read-from-file t)
              (instantiate def% ()
                (editor t))))
          (super-instantiate ())
          ))
  
      (define dsc (instantiate def-snip-class% ()))
      (send dsc set-classname "def%")
      (send dsc set-version 1)
      (send (get-the-snip-class-list) add dsc)
      ))   
  )
