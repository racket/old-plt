
#|

empty tool, just so that we can put the icon in
the about window and the splash screen. The 
language specification in the info.ss is really
everything.

|#

(module eopl-tool mzscheme
  (require (lib "unitsig.ss")
	   (lib "tool.ss" "drscheme"))
  
  (provide tool@)

  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      (define (phase1) (void))
      (define (phase2) (void)))))

