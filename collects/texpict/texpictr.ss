
(compound-unit/sig
 (import)
 (link [COMMON : ((open texpict-common^)
		  (open texpict-internal^))
	       ((require-relative-library "commonr.ss")
		(TEXPICTX : texpict-common-setup^))]
       [TEXPICTX : ((open texpict-extra^)
		    (open texpict-common-setup^))
		 ((require-relative-library "texpictxr.ss")
		  COMMON)])
 (export (open (COMMON : texpict-common^))
	 (open (TEXPICTX : texpict-extra^))))
