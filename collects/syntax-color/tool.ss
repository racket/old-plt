(module tool mzscheme
  (require (lib "tool.ss" "drscheme")
           (lib "unitsig.ss")
           (lib "class.ss")
           "colorer.ss")

  (provide tool@)
  
  (define (mixin-definition %)
    (class %
      (define/public (get-prompt-position) 0)
      (define/public (do-eval) (void))
      (define/public (insert-prompt) (void))
      (define/public (initialize-console) (void))
      (define/public (reset-console) (void))
      (super-instantiate ())))
      
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)

      (drscheme:get/extend:extend-interactions-text
       (lambda (%) (text-mixin %)))
      (drscheme:get/extend:extend-definitions-text
       (lambda (%) (text-mixin (mixin-definition %))))
      
      (define (phase1) (void))
      (define (phase2) (void)))))
      

