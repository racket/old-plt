(compound-unit/sig
  (import [M : mzlib:function^])
  (link [wx : wx^ (wx@)]
	[T : turtle^ ((require-library-unit/sig "turtlmr.ss" "graphics")
		      wx M)])
  (export (open T)))
   
