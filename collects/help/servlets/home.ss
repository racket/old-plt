(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server"))

(require "private/hd-css.ss")

(unit/sig ()
  (import servlet^)

 `(HTML
   (HEAD ,hd-css
	(TITLE "PLT Help Desk")
	(LINK ((REL "icon") (HREF "/help/servlets/plticon.ico") (TYPE "image/ico")))
	(LINK ((REL "SHORTCUT ICON") (HREF "/help/servlets/plticon.ico"))))
   ; the * pretends we have a second frame, working around a bug
   ;  in Navigator 4.x
   (FRAMESET ((ROWS "100%,*")
	      (COLS "100%")
	      (BORDER "0"))
 	     (NOFRAMES
	      (H2
	       "Your Web browser does not support frames, which are "
	       "required to use PLT Help Desk."
	       (P)
	       "Please upgrade to a browser that does support frames. "
	       "The Mozilla browser is a good choice, available at "
	       (A ((HREF "http://www.mozilla.org/")) "http://www.mozilla.org/")
	       "."))
	     (FRAME ((NAME "outer")
		     (SRC "/servlets/index.ss") 
		     (FRAMEBORDER "no"))))))
