  (unit/sig ()
    (import [mred : mred^]
            mzlib:core^
            mzlib:print-convert^
            (drscheme : drscheme:export^)
            zodiac:system^
            plt:parameters^)

    (mred:add-version-spec 's 1)

    (define filename (build-path mred:constants:plt-home-directory 
				 "mrspidey"
				 "drspidey.ss"))

    (define invoke-spidey
      (lambda (frame)
	(mred:show-busy-cursor
	 (lambda ()
	   (load/use-compiled (build-path filename))
	   (set! invoke-spidey
		 (invoke-open-unit/sig
		  (global-defined-value 'tool@) 
		  mrspidey
		  (mred : mred^)
		  mzlib:core^
		  mzlib:print-convert^
		  (drscheme : drscheme:export^)
		  zodiac:system^
		  plt:parameters^))
	   (invoke-spidey frame)))))

    (define spidey-bitmap
      (drscheme:unit:make-bitmap
       (build-path mred:constants:plt-home-directory
		   "icons"
		   "mrspidey.bmp")
       "Analyze"))

    (define spidey-frame%
      (letrec ()
	(class (drscheme:parameters:current-frame%) args
	  (inherit button-panel)
	  (sequence (apply super-init args))
	  (private
	    [button (make-object mred:button%
				 button-panel
				 (lambda (button evt) (invoke-spidey this))
				 spidey-bitmap)])
	  (sequence
	    (send button-panel change-children
		  (lambda (l)
		    (cons button (function@:remq button l))))))))

    (drscheme:parameters:current-frame% spidey-frame%))
