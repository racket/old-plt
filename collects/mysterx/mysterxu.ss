;;; mysterxu.ss

(require-library "cores.ss")
(require-library "macro.ss")
(require-library "functio.ss")

(require-library "urlu.ss" "net")

(require-library "sigs.ss" "mysterx")

(define mysterx@
  (compound-unit/sig
   (import)
   (link [core : mzlib:core^ ((require-library "corer.ss"))]
	 [mxprims : mysterx:prims^ ((require-library "prims.ss" "mysterx"))]
         [util : mysterx:util^ ((require-library "util.ss" "mysterx") 
				(core function))]
         [properties : mysterx:properties^ 
		((require-library "properties.ss" "mysterx") 
		 (core function))]
         [style : mysterx:style^
		((require-library "style.ss" "mysterx") 
		 (core string) properties util)]
         [filter : mysterx:filter^ 
		((require-library "filter.ss" "mysterx") 
		 (core string)
		 properties util)] 
	 [url : mzlib:url^
	      (mzlib:url@ (core file))]
	 [mysterx : mysterx:mysterx^ 
		  ((require-library "mysterxe.ss" "mysterx") 
		   (core function) (core string) url mxprims
		   style filter properties util)])
   (export
    (open mysterx))))

