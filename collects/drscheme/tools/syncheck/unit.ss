(compound-unit/sig
  (import [mred : mred^]
	  [mzlib : mzlib:core^]
	  [fw : framework^]
	  [print-convert : mzlib:print-convert^]
	  [drscheme : drscheme:export^]
	  [zodiac : zodiac:system^])
  (link
   [prims : drscheme:syncheck:prims^ ((require-relative-library "prims.ss"))]
   [arrow : drscheme:draw-arrow^ ((require-library "arrow.ss" "drscheme") mred)]
   [main : () ((require-relative-library "main.ss") 
               mred mzlib fw print-convert drscheme zodiac prims arrow)])
  (export))