(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
	 (lib "string.ss"))

(require "../private/headelts.ss")

(unit/sig ()
  (import servlet^)
  
  `(HTML
    (HEAD ,hd-css
          ,@hd-links
	  (TITLE "Downloadable Patches"))
    (H1 "Downloadable Patches")
    (A ((NAME="patches") (VALUE "Downloadable patches")))
    "The following Web page may contain downloadable patches to fix serious bugs in "
    "version " ,(version) " of the PLT software:"
    (P)
    'nbsp 'nbsp
    ,(let ([url (format "http://download.plt-scheme.org/patches/~a/" (version))])
       `(A ((HREF ,url)
	    (TARGET "_top")) ,url))))

