(compound-unit/sig (import [params : plt:userspace:params^])
  (link [core : mzlib:core^ ((require-library-unit/sig "corer.ss"))]
	[compat : mzlib:compat^ ((require-library-unit/sig "compatr.ss") (core function))]
	[mred : mred^ (mred@)]
	[rice : ricedefs^ ((require-library-unit/sig "ricedefr.ss" "userspce")
			   params)]
	[graphics : graphics^ ((require-library-unit/sig "graphicr.ss" "graphics")
			       (core file) mred)]
	[turtle : turtle^ ((require-library-unit/sig "turtlmr.ss" "graphics")
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
