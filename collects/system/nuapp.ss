(compound-unit/sig (import [I : mred:application-imports^])
  (link
   [mred : ((open mred^) (open mred:application^) (open (startup)))
	 ((compound-unit/sig (import [I : mred:application-imports^])
	    (link
	     [core : mzlib:core^ ((reference-library-unit/sig "corer.ss"))]
	     [mred : mred^ ((reference-library-unit/sig "link.ss" "mred") 
			    core app)]
	     [app : mred:application^
		  ((unit/sig mred:application^
		     (import mred:application-imports^
			     [mred : mred^])
		     (define app-name "MrEd")
		     (define eval-string (lambda (s)
					   (if console
					       (let ([ce (send console get-edit)])
						 (send ce eval-and-display s)
						 (send ce insert-prompt)
						 #t)
					       #f)))
		     (define console (make-object mred:console-frame%))
		     (for-each mred:edit-file (vector->list argv))
		     console)
		   I
		   mred)]
	     [dummy : (startup)
		    ((unit/sig (startup)
		       (import)
		       (define startup (lambda () (void)))))])
	    (export (open mred)
		    (open app)
		    (open dummy)))
	  I)])
  (export (unit mred)))


