#cs
(module tool mzscheme
  (require (lib "tool.ss" "drscheme")
           (lib "mred.ss" "mred")
           (lib "unitsig.ss") 
           (lib "etc.ss")
           (lib "class.ss")
	   (lib "string-constant.ss" "string-constants"))

  (provide tool@)

  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)

      (define (phase1) (void))
      (define (phase2) (void))))
  )
