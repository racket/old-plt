(require-library "core.ss")
(plt:require-library "userspcs.ss")
(plt:require-library "ricedefu.ss")
(plt:require-library "graphicu.ss")
(plt:require-library "sparams.ss")
(plt:require-library "turtleu.ss")

(define plt:userspace@
  (compound-unit/sig
    (import [params : plt:parameters^])
    (link [core : mzlib:core^ (mzlib:core@)]
	  [rice : ricedefs^ (ricedefs@ params)])
    (export (open core)
	    (open rice))))



