(module frames mzscheme

  (provide home-frames)
	
  (define (home-frames)
    `(FRAMESET ((ROWS "100%,*")
		(COLS "100%")
		(BORDER "0"))
	       (NOFRAMES
		(H2
		 "Your Web browser does not support frames."
		 (P)
		 "You can either:"
		 (UL
		  (LI "Disable frames in the "
		      (A ((HREF "/servlets/hd-config.ss"))
			 "Help Desk configuration") ", or")
		  (LI "Upgrade to a browser that does support frames. "
		      "The Mozilla browser is a good choice, available at "
		      (A ((HREF "http://www.mozilla.org/")) "http://www.mozilla.org/")
		      "."))))
	       (FRAME ((NAME "outer")
		       (SRC "/servlets/index.ss") 
		       (FRAMEBORDER "no"))))))
