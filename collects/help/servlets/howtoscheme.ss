(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server"))

(require "private/headelts.ss")

(unit/sig ()
  (import servlet^)
  `(HTML 
    (TITLE "Software")
    (HEAD ,hd-css
	  ,@hd-links)
    (BODY 
     (H1  "Software")  
     (UL  
      (LI  (B  (A ((HREF "howtodrscheme.ss")) "DrScheme")) 
	   ": The programming environment")
      (LI  (B  (A ((HREF "/servlets/scheme/what.ss")) "Languages")) 
	   ": The family of languages supported by PLT Software")
      (LI  (B  (A ((HREF "/servlets/scheme/how.ss")) "Software & Components")) 
	   ": The full suite of PLT tools " 
	   (BR) 'nbsp 'nbsp 'nbsp 'nbsp
	   (FONT ((SIZE "-2")) 
		 (A ((HREF "/servlets/scheme/how.ss#installed-components")) "Installed Components") ", ..."))
      (LI  (B  (A ((href "/servlets/scheme/doc.ss")) "Documentation")) ": Organization and manuals       " 
	   (BR) 'nbsp 'nbsp 'nbsp 'nbsp
	   (FONT ((SIZE "-2")) 
		 (A ((HREF "/servlets/manuals.ss")) "Manuals") ", ...") )
      (LI  (B  (A ((HREF "scheme/misc.ss")) "Hints")) 
	   ": How to do things in Scheme " )
      (LI  (B 
	    ,(manual-entry "drscheme"
			   "frequently asked questions"
			   "FAQ"))
	   ": Frequently asked questions")
      (LI  
       (B (A ((HREF "releaseinfo.ss")) "Release Information")) 
       ": License, notes, and known bugs")))))
