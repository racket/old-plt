(require-relative-library "sigs.ss")

(define mysterx@
  (compound-unit/sig
   (import)
   (link [core : mzlib:core^ ((require-library "corer.ss"))]
	 [mxprims : mysterx:prims^ ((require-relative-library "prims.ss"))]
	 [mysterx : mysterx:mysterx^ 
		  ((require-relative-library "mysterxe.ss") 
		   (core function) (core string)
		   mxprims)])
   (export
    (open mysterx))))
