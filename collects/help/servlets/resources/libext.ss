(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server"))

(require "../private/hd-css.ss")

(unit/sig ()
  (import servlet^)

  `(HTML 
    (HEAD ,hd-css
          (TITLE "Libraries"))
    (BODY 
     (H1  "Libraries")
     (A ((NAME "libraries") (VALUE "extensions")))  
     (A ((NAME "mrspidey") (VALUE "mrspidey")))
     (A ((NAME "static debugger") (VALUE "static debugger")))
     (A ((NAME "mysterx") (VALUE "mysterx")))
     (A ((NAME "mzcom") (VALUE "mzcom")))
     (A ((NAME "COM") (VALUE "COM")))
     (A ((NAME "srpersist") (VALUE "srpersist")))
     (A ((NAME "ODBC") (VALUE "ODBC")))
     (A ((NAME "databases") (VALUE "databases")))
     "Many libraries and extensions are available for PLT software. "
     "See the " 
     (A ((HREF "http://www.cs.utah.edu/plt/develop/")) 
	"PLT libraries and extensions")
     " page for a comprehensive listing." 
     (P)
     "If you write a PLT library or extension, we would like to "
     "hear about it!  Please send a message about it to "
     "Matthew Flatt at " 
     (TT "mflatt@cs.utah.edu") " so we can list it. "
     "Thanks for your efforts!")))
