  (compound-unit/sig
    (import [params : plt:parameters^]
	    [export : drscheme:export^])
    (link [core : mzlib:core^ ((reference-library-unit/sig "corer.ss"))]
	  [trigger : mzlib:trigger^ ((reference-library-unit/sig "triggerr.ss"))]
	  [appliction : mred:application^ 
		      ((unit/sig mred:application^
			 (import)
			 (define app-name "DrScheme")
			 (define console #f)
			 (define eval-string void)
			 (define startup void)))]
	  [mred : mred^ ((reference-library-unit/sig "link.ss" "mred")
			 core trigger appliction)]
	  [rice : ricedefs^ ((reference-library-unit/sig "ricedefr.ss" "userspce"))]
	  [graphics : graphics^ ((reference-library-unit/sig "graphicr.ss" "graphics"))]
	  [create-window : turtle:create-window^
	     ((unit/sig turtle:create-window^
		(import [drscheme : drscheme:export^])
		(define (create-turtle-window % title width height)
		  (with-parameterization drscheme:rep:system-parameterization
		    (lambda ()
		      (make-object % title width height)))))
	      export)]
	  [turtle : turtle^ ((reference-library-unit/sig "turtlmr.ss" "graphics")
			     (core function@)
			     create-window)])
    (export (open (core pretty-print@))
	    (open (core file@))
	    (open (core function@))
	    (open (core compat@))
	    (open (core string@))
	    (open rice)
	    (open graphics)
	    (open turtle)
	    (unit mred)))