(require-library "core.ss")
(require-library "trigger.ss")
(plt:require-library "gusrspcs.ss")
(plt:require-library "ricedefu.ss")
(plt:require-library "graphicu.ss")
(plt:require-library "sparams.ss")
(plt:require-library "turtleu.ss")

(define plt:userspace@
  (compound-unit/sig
    (import [params : plt:parameters^])
    (link [core : mzlib:core^ (mzlib:core@)]
	  [trigger : mzlib:trigger^ (mzlib:trigger@)]
	  [appliction : mred:application^ 
		      ((unit/sig mred:application^
			 (import)
			 (define app-name "DrScheme")
			 (define console
			   (make-object 
			    (class-asi wx:frame% (public [show void]))
			    '() "dummy console" -1 -1 100 100))
			 (send console show #t)
			 (define eval-string void)))]
	  [mred : mred^ (mred@ core trigger appliction)]
	  [rice : ricedefs^ (ricedefs@)]
	  [graphics : graphics^ (graphics@)]
	  [turtle : turtle^ (turtle@ (core function@))])
    (export (open (core pretty-print@))
	    (open (core file@))
	    (open (core function@))
	    (open (core compat@))
	    (open (core string@))
	    (open rice)
	    (open graphics)
	    (open turtle)
	    (unit mred))))