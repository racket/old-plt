(require-library "functiou.ss")
(require-library "compatu.ss")
(require-library "stringu.ss")
(plt:require-library "userspcs.ss")
(plt:require-library "ricedefu.ss")
(plt:require-library "graphicu.ss")
(plt:require-library "sparams.ss")
(plt:require-library "turtleu.ss")

(define plt:userspace@
  (compound-unit/sig
    (import [params : plt:parameters^])
    (link [function : mzlib:function^ (mzlib:function@)]
	  [compat : mzlib:compat^ (mzlib:compat@ function)]
	  [string : mzlib:string^ (mzlib:string@)]
	  [rice : ricedefs^ (ricedefs@ params)]
	  [graphics : graphics^ (graphics@)]
	  [turtle : turtle^ (turtle@ function)])
    (export (open function)
	    (open compat)
	    (open string)
	    (open rice)
	    (open graphics)
	    (open turtle))))

