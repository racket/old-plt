(compound-unit/sig (import [params : plt:userspace:params^])
  (link [core : mzlib:core^ ((require-library "corer.ss"))]
	[compat : mzlib:compat^ ((require-library "compatr.ss") (core function))]
	[mred : mred^ (mred@)]
	[rice : ricedefs^ ((require-library "ricedefr.ss" "userspce")
			   params)]
	[graphics : graphics^ ((require-library "graphicr.ss" "graphics")
			       (core file) mred)]
	[turtle : turtle^ ((require-library "turtlmr.ss" "graphics")
			   mred (core function))])
  (export (open (core pretty-print))
	  (open (core file))
	  (open (core function))
	  (open (core string))
	  (open compat)
	  (open rice)
	  (open graphics)
	  (open turtle)
	  (open mred)))
