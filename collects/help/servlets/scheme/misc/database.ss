(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server"))

(require "../../private/hd-css.ss")

(unit/sig ()
  (import servlet^)

 `(HTML 
   (TITLE "How to connect to databases")
   (HEAD ,hd-css)
   (BODY 
    (H1  "How to connect to databases") 
    (A ((NAME "db") (VALUE "Database connections")))
    "SrPersist (\"Sister Persist\") is an ODBC interface for "
    "DrScheme and MzScheme. "
    "Download SrPersist from "
    (PRE  
     " " 
     (A ((HREF "http://www.plt-scheme.org/software/srpersist/")) 
	"http://www.plt-scheme.org/software/srpersist/") ". ")
    "ODBC is a very low-level interface. "
    "Francisco Solsona has built a higher-level interface, "
    "SchemeQL, that uses SrPersist.  See "
    (PRE 
     " " 
     (A ((HREF "http://schematics.sourceforge.net/schemeql.html"))
	"http://schematics.sourceforge.net/schemeql.html"))
    " for more details.")))


