(compound-unit/sig
  (import [M : mzlib:function^])
  (link [mred : mred^ (mred@)]
	[T : turtle^ ((require-relative-library "turtlmr.ss" "graphics")
		      mred M)])
  (export (open T)))
   
