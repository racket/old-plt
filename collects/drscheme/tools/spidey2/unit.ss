(compound-unit/sig
  (import [mred : mred^]
	  [mzlib : mzlib:core^]
	  [fw : framework^]
	  [pc : mzlib:print-convert^]
	  (drscheme : drscheme:export^)
	  [zodiac : zodiac:system^])
  
  (link [arrow : drscheme:draw-arrow^ ((require-library "arrow.ss" "drscheme")
                                       mred)]
        [g : () ((require-relative-library "gui.ss")
                 mred mzlib fw pc drscheme arrow zodiac)])
  (export))