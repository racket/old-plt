(module tool mzscheme
  (require (lib "tool.ss" "drscheme")
           (lib "unitsig.ss")
           (lib "class.ss"))

  (provide tool@)
  
  (define (mixin %)
    (class %
      (define tokens #f)
      (define/public (get-tokens) tokens)
      (define/public (set-tokens t) (set! tokens t))
      (super-instantiate ())))
  
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)

      (drscheme:get/extend:extend-interactions-text mixin)
      (drscheme:get/extend:extend-definitions-text mixin)
      
      (define (phase1) (void))
      (define (phase2) (void)))))
      

