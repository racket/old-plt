(require-library "core.ss")
(plt:require-library "userspcs.ss")
(plt:require-library "ricedefu.ss")
(plt:require-library "sparams.ss")

(define plt:userspace@
  (compound-unit/sig
    (import [params : plt:parameters^])
    (link [core : mzlib:core^ (mzlib:core@)]
	  [rice : ricedefs^ (ricedefs@ params)])
    (export (open (core pretty-print@))
	    (open (core file@))
	    (open (core function@))
	    (open (core compat@))
	    (open (core string@))
	    (open rice))))



