(compound-unit/sig (import [params : plt:userspace:params^])
  (link [core : mzlib:core^ ((require-library-unit/sig "corer.ss"))]
	[rice : ricedefs^ ((require-unit/sig "ricedefr.ss") params)])
  (export (open (core pretty-print@))
	  (open (core file@))
	  (open (core function@))
	  (open (core compat@))
	  (open (core string@))
	  (open rice)))



