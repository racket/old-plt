
  (compound-unit/sig
    (import )
    (link [core : mzlib:core^ ((reference-library-unit/sig "corer.ss"))]
	  [rice : ricedefs^ ((reference-unit/sig "ricedefr.ss"))])
    (export (open (core pretty-print@))
	    (open (core file@))
	    (open (core function@))
	    (open (core compat@))
	    (open (core string@))
	    (open rice)))



