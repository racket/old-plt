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
			   [face : drscheme:face^]
			   [aries : plt:aries^]
			   [zodiac : drscheme:zodiac^])
  (link [rep : drscheme:rep^
	     ((reference-unit/sig "rep.ss")
	      mred mzlib print-convert aries zodiac
	      interface init snip language app basis edit)]
	[frame : drscheme:frame^
	       ((reference-unit/sig "frame.ss")
		mred mzlib basis
		setup unit
		compound-unit app zodiac)]
	[unit : drscheme:unit^
	  ((reference-unit/sig "unit.ss")
	   mred mzlib app setup compound-unit frame edit rep
	   language get/extend face)]
	[compound-unit : drscheme:compound-unit^
	  ((reference-unit/sig "cunit.ss")
	   mred mzlib unit frame face)]
	[signature : drscheme:signature^ ((reference-unit/sig "sig.ss")
					  mred)]
	[program : drscheme:program^ ((reference-unit/sig "prog.ss"))]
	[get/extend : drscheme:get/extend^
		    ((reference-unit/sig "params.ss") 
		     mred unit frame rep mzlib)])
  (export (unit frame)
	  (unit unit)
	  (unit compound-unit)
	  (unit signature)
	  (unit program)
	  (unit get/extend)
	  (unit rep)))