(compound-unit/sig
  (import [m : mzlib:function^])
  (link [mred : mred^ (mred@)]
	[t : turtle^ ((require-library "turtlmr.ss" "graphics")
		      mred m)])
  (export (open t)))
   
