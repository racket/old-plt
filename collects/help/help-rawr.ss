(compound-unit/sig
 (import [A : (argv)])
 (link [F : mzlib:function^ ((require-library "functior.ss"))]
       [SEARCH : help:search^ ((require-relative-library "search.ss")
			       HELP F)]
       [C : mzlib:command-line^ ((require-library "cmdliner.ss"))]
       [HELP : help:help^
	     ((require-relative-library "help-raw-main.ss")
	      A C SEARCH)])
 (export))
