(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server"))

(require "private/util.ss")
(require "private/headelts.ss")

(unit/sig ()
  (import servlet^)

 `(HTML 
   (TITLE "DrScheme")	
   (HEAD ,hd-css
	 ,@hd-links)
   (BODY 
    (H1 "DrScheme") 
    "DrScheme is PLT's flagship programming environment. "
    "See " (A ((HREF "/servlets/scheme/how.ss")) "Software & Components") 
    " for a guide to the full suite of PLT tools." 
    (UL  
     (LI  (B  (A ((HREF "/doc/tour/")) "Tour")) ": An introduction to DrScheme")
     (LI  (B  ,(manual-entry "drscheme" 
			     "graphical interface"
			     "Interface Essentials"))
	  ": Quick-start jump into the user manual")
     (LI  (B  (A ((HREF "/servlets/scheme/what.ss")) 
		 "Languages")) 
	  ": Languages supported by DrScheme")
     (LI  (B  ,(main-manual-page "drscheme")) ": The complete user manual")))))


