(unit/sig help:start-help-desk^
  (import mzlib:function^
	  mzlib:string^
	  mzlib:file^
	  mzlib:url^
	  mred^
	  framework^)

  (define new-help-frame #f)
  (define open-url-from-user #f)

  (include "startup-url.ss")

  (define (start-help-desk new-drscheme)
    (define frame-mixin (lambda (%) %))
    (unless new-help-frame
      (set!-values (new-help-frame
		    open-url-from-user)
		   (invoke-unit/sig
		    (require-library "helpr.ss" "help")
		    mzlib:function^
		    mzlib:string^
		    mzlib:file^
		    mzlib:url^
		    mred^
		    framework^
		    (frame-mixin))))
    (new-help-frame startup-url)))


      

