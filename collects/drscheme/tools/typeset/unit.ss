(compound-unit/sig
  (import [mred : mred^]
	  [core : mzlib:core^]
	  [fw : framework^]
	  [pc : mzlib:print-convert^]
	  [drscheme : drscheme:export^]
	  [zodiac : zodiac:system^])

  (link
   [main : () ((require-library "tool.ss" "typeset")
	       mred
	       fw
	       drscheme
	       zodiac)])
  (export))
