(unit/sig ()
  (import [wx : wx^]
	  [mred : mred^]
	  mzlib:core^
	  mzlib:print-convert^
	  (drscheme : drscheme:export^)
	  drscheme:zodiac^)
  
  (define rep-thread #f)
  
  (drscheme:get/extend:extend-unit-frame%
   (lambda (super%)
     (class super% args
       (inherit button-panel)
       (sequence (apply super-init args))
       (private
	 [bitmap (make-object wx:bitmap% 
			      (if (<= (wx:display-depth) 1)
				  (build-path (collection-path "icons")
					      "bb-sm-bw.bmp")
				  (build-path (collection-path "icons")
					      "bb-small.bmp"))
			      wx:const-bitmap-type-bmp)]
	 [button (make-object
		  mred:button%
		  button-panel
		  (lambda (button evt)			      
		    (if (and rep-thread
			     (thread-running? rep-thread))
			(begin 
			  (kill-thread rep-thread)
			  (set! rep-thread #f)
			  (printf "REPL killed~n"))
			(set! rep-thread 
			      (thread 
			       (lambda ()
				 (load "~/.mzschemerc")
				 (read-eval-print-loop)
				 (printf "~nREPL finished~n"))))))
		  (if (send bitmap ok?)
		      bitmap
		      "Console"))])
       (sequence
	 (send button-panel change-children
	       (lambda (l)
		 (cons button (function@:remq button l)))))))))
