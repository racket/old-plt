(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server"))

(require "../../private/hd-css.ss")

(unit/sig ()
  (import servlet^)

 `(HTML 
   (TITLE "How to build a stand-alone executable")
   (HEAD ,hd-css)
   (BODY 
    (H1 "How to build a stand-alone executable")
    (A ((NAME "exec") (VALUE "Standalone executables")))
    (A ((name "exec2") (VALUE "Stand-alone executables")))
    "The mzc compiler can be used to produce stand-alone "
    "executables. "
    "See " 
    (A ((HREF "/doc/mzc/")) 
       "PLT " (TT  "mzc") ": MzScheme Compiler Manual")
    " for more information.")))

