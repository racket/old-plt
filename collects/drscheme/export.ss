(compound-unit/sig (import [mred : mred^]
			   [mzlib : mzlib:core^]
			   [print-convert : mzlib:print-convert^]
			   [app : drscheme:app^]
			   [basis : drscheme:basis^]
			   [edit : drscheme:edit^]
			   [language : drscheme:language^]
			   [setup : drscheme:setup^]
			   [snip : drscheme:snip^]
			   [init : drscheme:init^]
			   [interface : drscheme:interface^]
			   [aries : plt:aries^]
			   [zodiac : drscheme:zodiac^])
  (link [rep : drscheme:rep^
	     ((reference-unit/sig "rep.ss")
	      mred mzlib print-convert aries zodiac
	      interface init snip language app basis edit)]
	[frame : drscheme:frame^
	       ((reference-unit/sig "frame.ss")
		mred mzlib basis
		setup unit parameters
		compound-unit app zodiac)]
	[unit : drscheme:unit^
	  ((reference-unit/sig "unit.ss")
	   mred mzlib app setup compound-unit frame edit rep
	   language parameters)]
	[compound-unit : drscheme:compound-unit^
	  ((reference-unit/sig "cunit.ss")
	   mred mzlib unit frame)]
	[parameters : drscheme:parameters^
		    ((reference-unit/sig "params.ss") mred unit frame rep)])
  (export (unit frame)
	  (unit unit)
	  (unit compound-unit)
	  (unit parameters)
	  (unit rep)))