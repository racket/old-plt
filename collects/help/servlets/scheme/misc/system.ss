(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server"))

(require "../../private/hd-css.ss")

(unit/sig ()
  (import servlet^)

 `(HTML
   (TITLE "How to call low-level system routines") 
   (HEAD ,hd-css)
   (BODY
    (H1 "How to call low-level system routines") 
    (A ((NAME "os") (VALUE "Low-level operating system calls")))
    "To call low-level system routines, you must write "
    "an extension to MzScheme using the C programming language. "
    "See " 
    (A ((HREF "/doc/insidemz/"))
       "Inside PLT MzScheme") " for details.")))

