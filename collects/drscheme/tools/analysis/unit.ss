  (unit/sig ()
    (import [mred : mred-interfaces^]
            mzlib:core^
	    [fw : framework^]
            mzlib:print-convert^
            (drscheme : drscheme:export^)
	    drscheme:zodiac^)
    
    (define invoke-spidey
      (lambda (frame)
	(fw:gui-utils:show-busy-cursor
	 (lambda ()
	   (let* ([e (mred:make-eventspace)]
		  [f (parameterize ([mred:current-eventspace e])
		       (mred:begin-busy-cursor)
		       (make-object mred:dialog% "Spidey"))]
		  [p (make-object mred:vertical-panel% f)]
		  [m (make-object mred:message% "Please wait, loading the Analysis." p)])
	     (send p stretchable-height #f)
	     (send p stretchable-width #f)
	     (thread (lambda () (send f show #t)))
	     (parameterize ([mred:current-eventspace e])
	       (mred:flush-display) (mred:yield)
	       (mred:flush-display) (mred:yield)
	       (mred:flush-display) (mred:yield)
	       (mred:flush-display) (mred:yield)
	       (mred:flush-display) (mred:yield)
	       (mred:flush-display) (mred:yield)
	       (mred:flush-display) (mred:yield))
	     (require-library "drspidey.ss" "mrspidey")
	     (send f show #f)
	     (set! invoke-spidey
		   (invoke-open-unit/sig
		    (global-defined-value 'tool@) 
		    mrspidey
		    (fw : framework^)
		    (mred : mred^)
		    mzlib:core^
		    mzlib:print-convert^
		    (drscheme : drscheme:export^)
		    zodiac:system^))
	     (parameterize ([mred:current-eventspace e])
	       (mred:end-busy-cursor))
	     (invoke-spidey frame))))))

    (define spidey-bitmap
      (drscheme:unit:make-bitmap
       "Analyze"
       (build-path (collection-path "icons") "mrspidey.bmp")))

    (drscheme:get/extend:extend-unit-frame%
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
