(compound-unit/sig (import [mred : mred^]
			   [mzlib : mzlib:core^]
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
	   mred mzlib setup compound-unit frame edit rep
	   language)]
	[compound-unit : drscheme:compound-unit^
	  ((reference-unit/sig "cunit.ss")
	   mred mzlib unit frame)])
  (export (unit frame drscheme:frame)
	  (unit unit drscheme:unit)
	  (unit compound-unit drscheme:compound-unit)))