(module tab-completion-tool mzscheme
  (require  (lib "tool.ss" "drscheme")
            (lib "unitsig.ss")
            "tab-completion-definitions-text-mixin.ss")
  
  (provide tool@)
  
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      
      (drscheme:get/extend:extend-definitions-text tab-completion-definitions-text-mixin)
      
      (define (phase1)
        (void))
      
      (define (phase2)
        (void)))))
      
