(compound-unit/sig
   (import)
   (link [core@ : mzlib:core^ ((reference-library-unit/sig "corer.ss"))]
	 [trigger@ : mzlib:trigger^ ((reference-library-unit/sig "triggerr.ss"))]
	 [zmath@ : mzlib:zmath^ ((reference-library-unit/sig "zmathr.ss"))]
	 [convert@ : mzlib:print-convert^
		   ((reference-library-unit/sig "pconverr.ss") 
		    (core@ string@) 
		    (core@ function@))]
	 [date@ : mzlib:date^ ((reference-library-unit/sig "dater.ss")
			       (core@ function@))]
	 [inflate@ : mzlib:inflate^ ((reference-library-unit/sig "inflater.ss"))]
	 [command-line@ : mzlib:command-line^ ((reference-library-unit/sig "cmdliner.ss"))])
   (export (open core@)
	   (unit trigger@)
	   (unit zmath@)
	   (unit convert@)
	   (unit date@)
	   (unit inflate@)
	   (unit command-line@)))

	 