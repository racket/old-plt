  (unit/sig ()
    (import [mred : mred^]
            mzlib:core^
            mzlib:print-convert^
            (drscheme : drscheme:export^)
            zodiac:system^
            plt:parameters^)

    (define rep-thread #f)

    (drscheme:parameters:current-frame%
     (class (drscheme:parameters:current-frame%) args
       (inherit button-panel)
       (sequence (apply super-init args))
       (private
	 [button (make-object
		  mred:button%
		  button-panel
		  (lambda (button evt)
		    (if (and rep-thread
			     (thread-running? rep-thread))
			(mred:message-box "already created a rep in the xterm")
			(set! rep-thread (thread 
					  (lambda ()
					    (read-eval-print-loop)
					    (printf "~nREP finished~n"))))))
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
