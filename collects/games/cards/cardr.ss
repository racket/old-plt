(compound-unit/sig
  (import (mred : mred^)
	  (F : mzlib:function^))
  (link [constants : cards:constants^ ((load-relative "constants.ss") mred)]
	[region : cards:region-local^ ((load-relative "region.ss"))]
	[util : cards:util^ ((load-relative "utils.ss"))]
	[sc : cards:snipclass^ ((load-relative "snipclass.ss") mred)]
	[cc : cards:card-class^ ((load-relative "card-class.ss") mred sc region)]
	[mc : cards:make-cards^ ((load-relative "make-cards.ss") mred cc)]
	[cl : cards:classes^ ((load-relative "classes.ss") mred util region constants F mc)]
	[main : cards:main^ ((load-relative "main.ss") mred cl mc)])
  (export
   (open main)
   (open util)
   (open (region : cards:region^))))
