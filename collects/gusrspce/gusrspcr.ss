  (compound-unit/sig
    (import [params : plt:parameters^])
    (link [core : mzlib:core^ ((reference-library-unit/sig "corer.ss"))]
	  [trigger : mzlib:trigger^ ((reference-library-unit/sig "triggerr.ss"))]
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
	  [mred : mred^ ((reference-library-unit/sig "link.ss" "mred")
			 core trigger appliction)]
	  [rice : ricedefs^ ((reference-unit/sig "ricedefu.ss"))]
	  [graphics : graphics^ ((reference-library-unit/sig "graphicu.ss" "graphics"))]
	  [turtle : turtle^ ((reference-library-unit/sig "turtleu.ss" "graphics")
			     (core function@))])
    (export (open (core pretty-print@))
	    (open (core file@))
	    (open (core function@))
	    (open (core compat@))
	    (open (core string@))
	    (open rice)
	    (open graphics)
	    (open turtle)
	    (unit mred)))