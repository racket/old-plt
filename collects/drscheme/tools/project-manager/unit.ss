(compound-unit/sig
  (import [mred : mred^]
	  [core : mzlib:core^]
	  [fw : framework^]
	  [pc : mzlib:print-convert^]
	  [drscheme : drscheme:export^]
	  [zodiac : zodiac:system^])

  (link
   [hierlist : hierlist^ ((require-library "hierlistr.ss" "hierlist") (core function) mred)]
   [main : () ((require-library "main.ss" "drscheme" "tools" "project-manager")
	       mred
	       core
               pc
	       fw
	       drscheme
	       zodiac
	       hierlist)])
  (export))
