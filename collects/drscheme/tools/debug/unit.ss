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
	 [button (make-object
		  mred:button%
		  button-panel
		  (lambda (button evt)
		    (if frame
			(send frame show #t)
			(set! frame (make-object mred:console-frame%))))
		  (make-object wx:bitmap% 
			       (if (<= (wx:display-depth) 1)
				   (build-path mred:constants:plt-home-directory
					       "icons"
					       "bb-sm-bw.bmp")
				   (build-path mred:constants:plt-home-directory
					       "icons"
					       "bb-small.bmp"))
			       wx:const-bitmap-type-bmp))])
       (sequence
	 (send button-panel change-children
	       (lambda (l)
		 (cons button (function@:remq button l))))))))
