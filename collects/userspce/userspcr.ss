(compound-unit/sig (import [params : plt:userspace:params^])
  (link [core : mzlib:core^ ((require-library "corer.ss"))]
	[compat : mzlib:compat^ ((require-library "compatr.ss") (core function))]
	[rice : ricedefs^ ((require-relative-library "ricedefr.ss") params)])
  (export (open (core pretty-print))
	  (open (core file))
	  (open (core function))
	  (open compat)
	  (open (core string))
	  (open rice)))



