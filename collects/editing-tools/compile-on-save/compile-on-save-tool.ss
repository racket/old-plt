(module compile-on-save-tool mzscheme
  (require  (lib "tool.ss" "drscheme")
            (lib "unitsig.ss")
            "compile-on-save-definitions-text-mixin.ss")
  
  (provide tool@)
  
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      
      (drscheme:get/extend:extend-definitions-text compile-on-save-definitions-text-mixin)
      
      (define (phase1)
        (void))
      
      (define (phase2)
        (void)))))
      
