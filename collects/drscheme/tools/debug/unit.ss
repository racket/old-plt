  (unit/sig ()
    (import [mred : mred^]
            mzlib:core^
            mzlib:print-convert^
            (drscheme : drscheme:export^)
            zodiac:system^
            plt:parameters^)

    (define frame #f)

    (drscheme:parameters:current-frame%
     (class (drscheme:parameters:current-frame%) args
       (inherit button-panel)
       (sequence (apply super-init args))
       (private
	 [button (make-object mred:button%
			      button-panel
			      (lambda (button evt)
				(if frame
				    (send frame show #t)
				    (set! frame (make-object mred:console-frame%))))
			      (drscheme:unit:make-bitmap
			       (build-path mred:constants:plt-home-directory
					   "icons"
					   "bb-small.bmp")
			       "Console"))])
       (sequence
	 (send button-panel change-children
	       (lambda (l)
		 (cons button (function@:remq button l))))))))
