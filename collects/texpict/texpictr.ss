
(compound-unit/sig
 (import)
 (link
  [mzlib:function : mzlib:function^ ((require-library "functior.ss"))]
  [common : ((open texpict-common^)
	     (open texpict-internal^))
	  ((require-relative-library "commonr.ss")
	   (texpictx : texpict-common-setup^))]
  [texpictx : ((open texpict-extra^)
	       (open texpict-common-setup^))
	    ((require-relative-library "texpictxr.ss")
	     common
	     mzlib:function)])
 (export (open (common : texpict-common^))
	 (open (texpictx : texpict-extra^))))
