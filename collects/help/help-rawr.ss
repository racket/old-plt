(compound-unit/sig
 (import)
 (link [F : mzlib:function^ ((require-library "functior.ss"))]
       [SEARCH : help:search^ ((require-relative-library "search.ss")
			       HELP F)]
       [HELP : help:help^
	     ((require-relative-library "help-raw-main.ss")
	      SEARCH)])
 (export))
