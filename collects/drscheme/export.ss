(compound-unit/sig (import [wx : wx^]
			   [mred : mred^]
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
	     ((require-relative-library-unit/sig "rep.ss")
	      wx mred mzlib print-convert aries zodiac
	      interface init snip language app basis edit)]
	[frame : drscheme:frame^
	       ((require-relative-library-unit/sig "frame.ss")
		wx mred mzlib basis
		setup unit
		compound-unit* app zodiac)]
	[unit : drscheme:unit^
	  ((require-relative-library-unit/sig "unit.ss")
	   wx mred mzlib app setup compound-unit* frame edit rep
	   language get/extend face)]
	[compound-unit* : drscheme:compound-unit^
	  ((require-relative-library-unit/sig "cunit.ss")
	   wx mred mzlib unit frame face)]
	[signature : drscheme:signature^ ((require-relative-library-unit/sig "sig.ss")
					  mred)]
	[program : drscheme:program^ ((require-relative-library-unit/sig "prog.ss"))]
	[get/extend : drscheme:get/extend^
		    ((require-relative-library-unit/sig "params.ss") 
		     wx mred unit frame rep mzlib)])
  (export (unit frame)
	  (unit unit)
	  (unit compound-unit* compound-unit)
	  (unit signature)
	  (unit program)
	  (unit get/extend)
	  (unit rep)))