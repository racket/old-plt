(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server"))

(require "private/hd-css.ss")

(unit/sig ()
  (import servlet^)

  `(HTML 
    (HEAD ,hd-css
	  (TITLE "External Resources"))
    (BODY 
     (H1  "External Resources")
     (P)
     "DrScheme is created by "
     (A ((HREF "http://www.plt-scheme.org/")) "PLT") 
     ", the Programming Languages Team "
     "based at Northeastern University, the University of Utah, and "
     "Brown University. "
     "Here are some links related to our activities."
     (P)
     (UL  
      (LI  (B (A ((HREF "resources/teachscheme.ss"))
		 "TeachScheme! Workshops"))
	   ": Free summer program")
      (LI  (B (A ((HREF "resources/libext.ss")) 
		 "Libraries")) 
	   ": From PLT and contributors")
      (LI  (B (A ((HREF "resources/maillist.ss")) 
		 "Mailing Lists")) ": How to subscribe"))
     (P)
     "Also, the Schemers.org Web site provides links for "
     "many Scheme resources, including books, implementations, "
     "and libraries: " (A ((HREF "http://www.schemers.org/"))
			  "http://www.schemers.org/") ".")))
