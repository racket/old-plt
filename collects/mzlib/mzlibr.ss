(compound-unit/sig
   (import)
   (link [core@ : mzlib:core^ ((require-reference-unit/sig "corer.ss"))]
	 [trigger@ : mzlib:trigger^ ((require-reference-unit/sig "triggerr.ss"))]
	 [zmath@ : mzlib:zmath^ ((require-reference-unit/sig "zmathr.ss"))]
	 [convert@ : mzlib:print-convert^
		   ((require-reference-unit/sig "pconverr.ss") 
		    (core@ string@) 
		    (core@ function@))]
	 [date@ : mzlib:date^ ((require-reference-unit/sig "dater.ss")
			       (core@ function@))]
	 [inflate@ : mzlib:inflate^ ((require-reference-unit/sig "inflater.ss"))])
   (export (open core@)
	   (unit trigger@)
	   (unit zmath@)
	   (unit convert@)
	   (unit date@)
	   (unit inflate@)))

	 