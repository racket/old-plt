(unit/sig help:start-help-desk^
  (import mzlib:function^
	  mzlib:string^
	  mzlib:file^
	  mzlib:url^
	  mred^)

  (include "startup-url.ss")

  (define (start-help-desk)
    (parameterize ([exit-handler void])
      (invoke-unit/sig
       (require-library "helpr.ss" "help")
       help:option^
       mzlib:function^
       mzlib:string^
       mzlib:file^
       mzlib:url^
       mred^))))

