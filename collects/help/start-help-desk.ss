(unit/sig help:start-help-desk^
  (import mzlib:function^
	  mzlib:string^
	  mzlib:file^
	  mzlib:url^
	  mred^
	  framework^)

  (define new-help-frame #f)

  (include "startup-url.ss")

  (define (start-help-desk)
    (unless new-help-frame
      (set! new-help-frame
	    (invoke-unit/sig
	     (require-library "helpr.ss" "help")
	     mzlib:function^
	     mzlib:string^
	     mzlib:file^
	     mzlib:url^
	     mred^
	     framework^)))
    (new-help-frame startup-url)))



      

