(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server"))

(require "../../private/util.ss")
(require "../../private/hd-css.ss")

(unit/sig ()
  (import servlet^)

 `(HTML 
   (TITLE "How to use ActiveX components")
   (HEAD ,hd-css)
   (BODY 
    (H1  "How to use ActiveX components")
    (A ((NAME "com") (VALUE "COM")))
    (A ((NAME "activex") (VALUE "ActiveX")))
    "If you run Windows, you can use MysterX, a library for "
    "controlling COM and ActiveX components within DrScheme, "
    "MzScheme, or MrEd.  MysterX is available from "
    (PRE 
     'nbsp 'nbsp  (A ((HREF "http://www.plt-scheme.org/software/mysterx/"))
		     "http://www.plt-scheme.org/software/mysterx/"))
    (P)
    ,(collection-doc-link "mysterx"
			  "The MysterX collection"))))
