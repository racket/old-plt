(compound-unit/sig
  (import [mred : mred^]
	  [mzlib : mzlib:core^]
	  [fw : framework^]
	  [print-convert : mzlib:print-convert^]
	  [drscheme : drscheme:export^]
	  [zodiac : zodiac:system^])
  (link
   [prims : drscheme:syncheck:prims^ ((require-relative-library "prims.ss"))]
   [main : () ((require-relative-library "main.ss") 
               mred mzlib fw print-convert drscheme zodiac prims)])
  (export))