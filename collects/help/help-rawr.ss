(compound-unit/sig
 (import [A : (argv)])
 (link [F : mzlib:function^ ((require-library "functior.ss"))]
       [SEARCH : help:search^ ((require-relative-library "search.ss") F)]
       [C : mzlib:command-line^ ((require-library "cmdliner.ss"))]
       [HELP : () ((require-relative-library "help-raw-main.ss")
		   A C SEARCH)])
 (export))
