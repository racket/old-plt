(module online-error-checking-tool mzscheme
  (require  (lib "tool.ss" "drscheme")
            (lib "unitsig.ss")
            "online-error-checking-definitions-text-mixin.ss")
  
  (provide tool@)
  
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      
      (drscheme:get/extend:extend-definitions-text
       (lambda (dt%)
         (online-error-checking-definitions-text-mixin dt%
                                             drscheme:eval:expand-program
                                             drscheme:language:make-text/pos)))
      
      (define (phase1)
        (void))
      
      (define (phase2)
        (void)))))
      
