(compound-unit/sig (import [I : mred:application-imports^])
  (link
   [mred : ((open mred^) (open mred:application^))
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
		     (for-each mred:edit-file (vector->list argv)))
		   I
		   mred)])
	    (export (open mred)
		    (open app)))
	  I)])
  (export (unit mred)))


