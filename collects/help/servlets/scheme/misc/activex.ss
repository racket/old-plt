(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
         (lib "servlet-helpers.ss" "web-server"))

(require "../../private/util.ss")
(require "../../private/headelts.ss")

(unit/sig ()
  (import servlet^)

  (report-errors-to-browser send/finish)

 `(HTML 
   (HEAD ,hd-css
         ,@hd-links
         (TITLE "How to use ActiveX components"))
   (BODY 
    (H1  "How to use ActiveX components")
    (A ((NAME "com") (VALUE "COM")))
    (A ((NAME "activex") (VALUE "ActiveX")))
    "If you run Windows, you can use MysterX, a library for "
    "controlling COM and ActiveX components within DrScheme, "
    "MzScheme, or MrEd.  MysterX is available from "
    (PRE 
     'nbsp 'nbsp  (A ((HREF "http://www.plt-scheme.org/software/mysterx/")
		      (TARGET "_top")) "http://www.plt-scheme.org/software/mysterx/"))
    (P)
    ,(collection-doc-link "mysterx"
			  "The MysterX collection"))))
