(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
	 (lib "string.ss"))

(require "../private/hd-css.ss")

(unit/sig ()
  (import servlet^)
  
  `(HTML
    (TITLE "Downloadable Patches")
    (HEAD ,hd-css)
    (H1 "Downloadable Patches")
    (A ((NAME="patches") (VALUE "Downloadable patches")))
    "The following Web page may contain downloadable patches to fix serious bugs in "
    "version " ,(version) " of the PLT software:"
    (P)
    'nbsp 'nbsp
    ,(let ([url (format "http://download.plt-scheme.org/patches/~a/" (version))])
       `(A ((HREF ,url)) ,url))))


