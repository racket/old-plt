
(compound-unit/sig
 (import (MRED : mred^))
 (link [COMMON : ((open texpict-common^)
		  (open texpict-internal^))
	       ((require-relative-library "commonr.ss")
		(MRPICTX : texpict-common-setup^))]
       [MRPICTX : ((open mrpict-extra^)
		   (open texpict-common-setup^))
		((require-relative-library "mrpictxr.ss")
		 MRED
		 COMMON)])
 (export (open (COMMON : texpict-common^))
	 (open (MRPICTX : mrpict-extra^))))
