;; this file expects mred@ to be defined at the toplevel and be the result of
;; (reference-unit/sig (build-path (getenv "PLTHOME") "mred" "system" "link.ss"))
;; [ roughly ]
;; if mred@ is not defined, it will re-load the library

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
	  [mred : mred^ ((with-handlers ([void
					  (lambda (exn)
					    (reference-unit/sig
					     (begin-elaboration-time
					      (normalize-path
					       (build-path 'up "mred" "system" "link.ss")))))])
			   mred@) 
			 core trigger appliction)]
	  [rice : ricedefs^ ((reference-unit/sig "ricedefu.ss"))]
	  [graphics : graphics^ ((reference-unit/sig "graphicu.ss"))]
	  [turtle : turtle^ ((reference-unit/sig "turtleu.ss")
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