(begin-elaboration-time
 (require-library "coreflats.ss")
 (require-library "turtles.ss" "graphics")
 (require-library "invoke.ss"))

(let ([u (compound-unit/sig
	  (import)
	  (link [core : mzlib:core-flat^ ((require-library "coreflatr.ss"))]
		[turtles : turtle^ ((require-library "turtler.ss" "graphics")
				    (core : mzlib:function^))]
		[posn : ((struct posn (x y)))
		      ((unit/sig ((struct posn (x y)))
			 (import)
			 (define-struct posn (x y))))])
	  (export
	   (open core)
	   (open turtles)
	   (open posn)))])
  (lambda ()
    (global-define-values/invoke-unit/sig ((open mzlib:core-flat^)
					   (open turtle^)
					   (open ((struct posn (x y)))))
					  u)))
