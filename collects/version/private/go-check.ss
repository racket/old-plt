(module go-check mzscheme

  (require (lib "unitsig.ss"))

  (require "checksigs.ss")
  (require "runcheck.ss")

  (provide go-check)

  (define (go-check frame defs@)
    (let ([check-frame@ 
	   (unit/sig 
	    check-frame^
	    (import)
	    (define check-frame frame))])
      (invoke-unit/sig
       (compound-unit/sig
	(import)
	(link
         [CFRAME : check-frame^ (check-frame@)]
	 [DEFS : defs^ (defs@)]
	 [RUNCHECK : empty^ (runcheck@ (CFRAME) (DEFS))])
	(export))))))

