(define tool@
  (unit/sig ()
    (import [mred : mred^]
	    [mzlib : mzlib:core^]
	    [print-convert : mzlib:print-convert^]
	    [drscheme : drscheme:export^]
	    [zodiac : zodiac:system^]
	    [params : plt:parameters^])
    (lambda (f)
      (mred:message-box (format
			 "The analyzer is not available in DrScheme version ~a."
			 (mred:version))
			"Unavailable"))))

