(compound-unit/sig (import [mred : mred^]
			   [mzlib : mzlib:core^]
			   [app : drscheme:app^]
			   [basis : drscheme:basis^]
			   [edit : drscheme:edit^]
			   [language : drscheme:language^]
			   [rep : drscheme:rep^]
			   [setup : drscheme:setup^]
			   [zodiac : zodiac:system^])
  (link [frame : drscheme:frame^
	       ((reference-unit/sig "frame.ss")
		mred mzlib basis
		setup unit compound-unit zodiac)]
	[unit : drscheme:unit^
	  ((reference-unit/sig "unit.ss")
	   mred mzlib app setup compound-unit frame edit rep
	   language parameters)]
	[compound-unit : drscheme:compound-unit^
	  ((reference-unit/sig "cunit.ss")
	   mred mzlib unit frame)]
	[parameters : drscheme:parameters^
		    ((reference-unit/sig "params.ss") mred unit rep)])
  (export (unit frame)
	  (unit unit)
	  (unit compound-unit)
	  (unit parameters)))