(compound-unit/sig (import [params : plt:userspace:params^])
  (link [core : mzlib:core^ ((require-library-unit/sig "corer.ss"))]
	[mred : mred^ ((require-library-unit/sig "link.ss" "mred") core)]
	[wx : wx^ (wx@)]
	[rice : ricedefs^ ((require-library-unit/sig "ricedefr.ss" "userspce")
			   params)]
	[graphics : graphics^ ((require-library-unit/sig "graphicr.ss" "graphics")
			       wx (core file@) mred)]
	[turtle : turtle^ ((require-library-unit/sig "turtlmr.ss" "graphics")
			   wx 
			   (core function@))])
  (export (open (core pretty-print@))
	  (open (core file@))
	  (open (core function@))
	  (open (core compat@))
	  (open (core string@))
	  (open rice)
	  (open graphics)
	  (open turtle)
	  (unit mred)
	  (unit wx)))
