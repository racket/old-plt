(compound-unit/sig
   (import)
   (link [core@ : mzlib:core^ ((require-reference-unit/sig "corer.ss"))]
	 [trigger@ : mzlib:trigger^ ((require-reference-unit/sig "triggerr.ss"))]
	 [zmath@ : mzlib:zmath^ ((require-reference-unit/sig "zmathr.ss"))]
	 [convert@ : mzlib:print-convert^
		   ((require-reference-unit/sig "pconverr.ss") 
		    (core@ string@) 
		    (core@ function@) 
		    hooks@)]
	 [hooks@ : mzlib:print-convert-hooks^ ((require-reference-unit/sig pchookr.ss"))]
	 [date@ : mzlib:date^ ((require-reference-unit/sig "dater.ss")
			       (core@ function@))])
   (export (open core@)
	   (unit trigger@)
	   (unit zmath@)
	   (unit convert@)
	   (unit hooks@)
	   (unit date@)))

	 