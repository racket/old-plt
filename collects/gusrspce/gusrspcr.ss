(compound-unit/sig (import [params : plt:userspace:params^])
  (link [core : mzlib:core^ ((reference-library-unit/sig "corer.ss"))]
	[mred : mred^ ((reference-library-unit/sig "link.ss" "mred") core)]
	[wx : wx^ (wx@)]
	[rice : ricedefs^ ((reference-library-unit/sig "ricedefr.ss" "userspce")
			   params)]
	[graphics : graphics^ ((reference-library-unit/sig "graphicr.ss" "graphics")
			       wx (core file@) mred)]
	[turtle : turtle^ ((reference-library-unit/sig "turtlmr.ss" "graphics")
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
