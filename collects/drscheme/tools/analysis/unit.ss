  (unit/sig ()
    (import [wx : wx^]
	    [mred : mred^]
            mzlib:core^
            mzlib:print-convert^
            (drscheme : drscheme:export^)
	    drscheme:zodiac^)

    (mred:add-version-spec 's 1)

    (define invoke-spidey
      (lambda (frame)
	(mred:show-busy-cursor
	 (lambda ()
	   (let* ([f [parameterize ([wx:current-eventspace [wx:make-eventspace]])
		       (make-object mred:frame% null "Spidey")]]
		  [p (make-object mred:vertical-panel% f)]
		  [m (make-object mred:canvas-message% p "Please wait, loading the Analysis.")])
	     (send f show #t)
	     (require-library "drspidey.ss" "mrspidey")
	     (send f show #f))
	   (set! invoke-spidey
		 (invoke-open-unit/sig
		  (global-defined-value 'tool@) 
		  mrspidey
		  (wx : wx^)
		  (mred : mred^)
		  mzlib:core^
		  mzlib:print-convert^
		  (drscheme : drscheme:export^)
		  zodiac:system^))
	   (invoke-spidey frame)))))

    (define spidey-bitmap
      (drscheme:unit:make-bitmap
       (build-path (collection-path "icons") "mrspidey.bmp")
       "Analyze"))

    (drscheme:get/extend:extend-unit-frame%
     (lambda (super%)
       (class super% args
	 (inherit button-panel)
	 (sequence (apply super-init args))
	 (public
	   [analyze-button (make-object mred:button%
				button-panel
				(lambda (button evt) (invoke-spidey this))
				spidey-bitmap)])
	 (sequence
	   (send button-panel change-children
		 (lambda (l)
		   (cons analyze-button (function@:remq analyze-button l)))))))))
