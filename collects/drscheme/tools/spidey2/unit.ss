  (unit/sig ()
    (import [mred : mred^]
            mzlib:core^
	    [fw : framework^]
            mzlib:print-convert^
            (drscheme : drscheme:export^)
	    [zodiac : zodiac:system^])
    
    (include (begin-elaboration-time (build-path (collection-path "newspidey") "spidey2r.ss"))))

    (define (open-gui drs-frame)
      (define thnk drscheme:load-handler:process-text)
      (void))

    (define spidey-bitmap
      (drscheme:unit:make-bitmap
       "Analyze2"
       (build-path (collection-path "icons") "mrspidey.bmp")))

    (drscheme:get/extend:extend-unit-frame
     (lambda (super%)
       (class super% args
	 (inherit button-panel)
	 (sequence (apply super-init args))
	  (rename [super-disable-evaluation disable-evaluation]
		  [super-enable-evaluation enable-evaluation])
	  (override
	    [enable-evaluation
	     (lambda ()
	       (send analyze-button enable #t)
	       (super-enable-evaluation))]
	    [disable-evaluation
	     (lambda ()
	       (send analyze-button enable #f)
	       (super-disable-evaluation))])
	 (public
	   [analyze-button (make-object mred:button%
				(spidey-bitmap this)
				button-panel
				(lambda (button evt) (invoke-spidey this)))])
	 (sequence
	   (send button-panel change-children
		 (lambda (l)
		   (cons analyze-button (function:remq analyze-button l)))))))))
