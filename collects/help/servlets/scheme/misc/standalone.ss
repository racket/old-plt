(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server"))

(require "../../private/headelts.ss")

(unit/sig ()
  (import servlet^)

 `(HTML 
   (HEAD ,hd-css
	 ,@hd-links
         (TITLE "How to build a stand-alone executable"))
   (BODY 
    (H1 "How to build a stand-alone executable")
    (A ((NAME "exec") (VALUE "Standalone executables")))
    (A ((name "exec2") (VALUE "Stand-alone executables")))
    "The mzc compiler can be used to produce stand-alone "
    "executables. "
    "See " 
    ,(main-manual-page "mzc")
    " for more information.")))

