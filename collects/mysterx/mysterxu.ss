;;; mysterxu.ss

(require-library "sigs.ss" "mysterx")

(define mysterx@
  (compound-unit/sig
   (import)
   (link [core : mzlib:core^ ((require-library "corer.ss"))]
	 [mxprims : mysterx:prims^ ((require-library "prims.ss" "mysterx"))]
	 [mysterx : mysterx:mysterx^ 
		  ((require-library "mysterxe.ss" "mysterx") 
		   (core function) (core string)
		   mxprims)])
   (export
    (open mysterx))))
