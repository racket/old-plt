(compound-unit/sig
 (import [I : mred:application-imports^])
  (link [mred : ((open mred^) (open mred:application^))
	      ((compound-unit/sig (import [I : mred:application-imports^])
		 (link [mzlib : mzlib:core^ ((reference-library-unit/sig "corer.ss"))]
		       [mred : mred^ ((reference-library-unit/sig "link.ss" "mred")
				      mzlib app)]
		       [app : mred:application^
			    ((unit/sig mred:application^
			       (import [mred : mred^]
				       mred:application-imports^)
			       (define app-name "MrEd")
			       (define console (make-object mred:console-frame%))
			       (define eval-string (lambda (s)
						     (let ([ce (send console get-edit)])
						       (send ce eval-and-display s)
						       (send ce insert-prompt)
						       #t)))
			       (for-each mred:edit-file (reverse (vector->list argv))))
			     mred
			     I)])
		 (export (open mred)
			 (open app)))
	       I)])
 (export (unit mred)))
