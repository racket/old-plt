  (unit/sig ()
    (import [mred : mred^]
            mzlib:core^
            mzlib:print-convert^
            (drscheme : drscheme:export^)
            zodiac:system^
            plt:parameters^)

    (define filename (build-path mred:constants:plt-home-directory 
				 "mrspidey"
				 "drspidey.ss"))

    (define spidey-frame%
      (class (drscheme:parameters:current-frame%) args
	(inherit button-panel)
	(sequence (apply super-init args))
	(private
	  [button (make-object mred:button%
		    button-panel
		    (letrec ([t 
			      (lambda ()
				(mred:show-busy-cursor
				 (lambda ()
				   (load/use-compiled (build-path filename))
				   (set! t
					 (let ([f (invoke-unit/sig
						   (global-defined-value 'tool@) 
						   (mred : mred^)
						   mzlib:core^
						   mzlib:print-convert^
						   (drscheme : drscheme:export^)
						   zodiac:system^
						   plt:parameters^)])
					   (lambda ()
					     (f this))))))
				(t))])
		      (lambda (button evt) 
			(t)))
		    (drscheme:unit:make-bitmap
		     (build-path mred:constants:plt-home-directory
				 "icons"
				 "mrspidey.bmp")
		     "Analyze"))])
	(sequence
	  (send button-panel change-children
		(lambda (l)
		  (cons button (function@:remq button l)))))))

    (drscheme:parameters:current-frame% spidey-frame%))
