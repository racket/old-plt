
  (compound-unit/sig
    (import [params : plt:parameters^])
    (link [core : mzlib:core^ ((reference-library-unit/sig "corer.ss"))]
	  [rice : ricedefs^ ((reference-unit/sig "ricedefu.ss"))])
    (export (open (core pretty-print@))
	    (open (core file@))
	    (open (core function@))
	    (open (core compat@))
	    (open (core string@))
	    (open rice)))



