(compound-unit/sig
  (import (wx : wx^)
	  (mred : mred^)
	  (F : mzlib:function^))
  (link [constants : cards:constants^ ((load-relative "constants.ss") wx)]
	[region : cards:region-local^ ((load-relative "region.ss"))]
	[util : cards:util^ ((load-relative "utils.ss"))]
	[sc : cards:snipclass^ ((load-relative "snipclass.ss") wx)]
	[cc : cards:card-class^ ((load-relative "card-class.ss") wx mred sc)]
	[mc : cards:make-cards^ ((load-relative "make-cards.ss") wx mred cc)]
	[cl : cards:classes^ ((load-relative "classes.ss") wx mred util region constants F mc)]
	[main : cards:main^ ((load-relative "main.ss") wx mred cl mc)])
  (export
   (open main)
   (open util)
   (open (region : cards:region^))))