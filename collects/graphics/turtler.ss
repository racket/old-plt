(compound-unit/sig
  (import [M : mzlib:function^])
  (link [wx : wx^ (wx@)]
	[T : turtle^ ((reference-library-unit/sig "turtlmr.ss" "graphics")
		      wx M)])
  (export (open T)))
   
