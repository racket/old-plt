(compound-unit/sig
  (import [M : mzlib:function^])
  (link [mred : mred^ (mred@)]
	[T : turtle^ ((require-library-unit/sig "turtlmr.ss" "graphics")
		      mred M)])
  (export (open T)))
   
