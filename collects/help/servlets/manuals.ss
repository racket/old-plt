(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
	 (lib "manuals.ss" "help" "private"))

(unit/sig ()
  (import servlet^)

  (list
   "text/html"	 
   (find-manuals)))







