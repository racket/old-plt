;; mysterx.ss

(current-require-relative-collection '("mysterx"))

(require-library "macro.ss")
(require-library "cores.ss")
(require-relative-library "sigs.ss")

(invoke-open-unit/sig 
  (compound-unit/sig ; mysterx:mysterx^
   (import)
   (link [core : mzlib:core^ ((require-library "corer.ss"))]
	 [mxprims : mysterx:prims^ ((require-relative-library "prims.ss"))]
	 [mysterx : mysterx:mysterx^ 
		  ((require-relative-library "mysterxu.ss") 
		   (core function) (core string)
		   mxprims)])
   (export
    (open mysterx))))
