((reference-library "appl.ss" "system")
 (unit/sig (console)
   (import [mred : mred^]
	   [I : mred:application-imports^])
   (define console
     (apply (global-defined-value 'mred:startup)
	    (vector->list I:argv)))))
