(compound-unit/sig (import [init : drscheme:init^]
			   [params : plt:userspace:params^])
  (link [core : mzlib:core^ ((reference-library-unit/sig "corer.ss"))]
	[appliction : mred:application^ 
		    ((unit/sig mred:application^
		       (import)
		       (define app-name "DrScheme")
		       (define console #f)
		       (define eval-string void)))]
	[mred : mred^ ((reference-library-unit/sig "link.ss" "mred")
		       core appliction)]
	[rice : ricedefs^ ((reference-library-unit/sig "ricedefr.ss" "userspce")
			   params)]
	[graphics : graphics^ ((reference-library-unit/sig "graphicr.ss" "graphics")
			       (core file@))]
	[create-window : turtle:create-window^
		       ((unit/sig turtle:create-window^
			  (import [drscheme:init : drscheme:init^])
			  (define (create-turtle-window % title width height)
			    (with-parameterization drscheme:init:system-parameterization
			      (lambda ()
				(make-object % title width height)))))
			init)]
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
