((reference-library "appl.ss" "system")
 (unit/sig (console)
   (import [mred : mred^]
	   [I : mred:application-imports^])
   (define console (make-object mred:console-frame%))))
