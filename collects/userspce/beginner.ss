(begin-elaboration-time
 (require-library "coreflats.ss")
 (require-library "invoke.ss"))

(let ([u (compound-unit/sig
	  (import)
	  (link [cf : mzlib:core-flat^ ((require-library "coreflatr.ss"))]
		[posn : ((struct posn (x y) -setters))
		      ((unit/sig ((struct posn (x y) -setters))
			 (import)
			 (define-struct posn (x y))))])
	  (export (open cf)
		  (open posn)))])
  (lambda ()
    (global-define-values/invoke-unit/sig ((open mzlib:core-flat^) (open ((struct posn (x y) -setters)))) u)))
