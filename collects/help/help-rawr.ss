(compound-unit/sig
 (import [A : (argv)])
 (link [F : mzlib:function^ ((require-library "functior.ss"))]
       [doc-pos : help:doc-position^ ((unit/sig help:doc-position^
					(import)
					(define (user-defined-doc-position x) #f)))]
       [SEARCH : help:search^ ((require-relative-library "search.ss") F)]
       [C : mzlib:command-line^ ((require-library "cmdliner.ss"))]
       [HELP : () ((require-relative-library "help-raw-main.ss")
		   A C SEARCH)])
 (export))
