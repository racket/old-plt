(module tool mzscheme
  (require (lib "tool.ss" "drscheme")
           (lib "unitsig.ss")
           (lib "class.ss")
           "tokenized-text.ss")

  (provide tool@)
  
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)

      (drscheme:get/extend:extend-interactions-text
       (lambda (%) (tokenized-interactions-text-mixin %)))
      (drscheme:get/extend:extend-definitions-text
       (lambda (%) (tokenized-text-mixin %)))
      
      (define (phase1) (void))
      (define (phase2) (void)))))