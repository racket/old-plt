(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server"))

(require "private/hd-css.ss"
         "private/frames.ss"
         "private/no-frames.ss"
         "private/util.ss")

(unit/sig ()
  (import servlet^)

 `(HTML
   (HEAD ,hd-css
	(TITLE "PLT Help Desk")
	(LINK ((REL "icon") (HREF "/help/servlets/plticon.ico") 
	       (TYPE "image/ico")))
	(LINK ((REL "SHORTCUT ICON") (HREF "/help/servlets/plticon.ico"))))
   ,(if (use-frames?)
       (home-frames)
       (home-no-frames))))
