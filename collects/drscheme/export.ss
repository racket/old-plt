(compound-unit/sig (import [mred : mred^]
			   [mzlib : mzlib:core^]
			   [framework : framework^]
			   [print-convert : mzlib:print-convert^]
			   [app : drscheme:app^]
			   [edit : drscheme:edit^]
			   [language : drscheme:language^]
			   [snip : drscheme:snip^]
			   [init : drscheme:init^]
			   [interface : drscheme:interface^]
			   [face : drscheme:face^]
			   [aries : plt:aries^]
			   [zodiac : drscheme:zodiac^])
  (link [basis-import : userspace:basis-import^ ((unit/sig userspace:basis-import^
						   (import)
						   (define in-mzscheme? #f)))]
	[basis : userspace:basis^
	       ((require-library-unit/sig "basis.ss" "userspce")
		basis-import
		zodiac
		interface
		aries
		print-convert
		(mzlib pretty-print)
		(mzlib function))]
	[rep : drscheme:rep^
	     ((require-relative-library "rep.ss")
	      framework mred mzlib print-convert zodiac
	      interface init snip language app basis edit)]
	[frame : drscheme:frame^
	       ((require-relative-library "frame.ss")
		mred mzlib framework
		unit compound-unit* app zodiac)]
	[unit : drscheme:unit^
	  ((require-relative-library "unit.ss")
	   mred mzlib framework app compound-unit* frame edit rep
	   language get/extend face)]
	[compound-unit* : drscheme:compound-unit^
	  ((require-relative-library "cunit.ss")
	   mred mzlib unit frame face)]
	[signature : drscheme:signature^ ((require-relative-library "sig.ss")
					  mred)]
	[program : drscheme:program^ ((require-relative-library "prog.ss"))]
	[get/extend : drscheme:get/extend^
		    ((require-relative-library "params.ss") 
		     mred unit frame rep mzlib)])
  (export (unit basis)
	  (unit frame)
	  (unit unit)
	  (unit compound-unit* compound-unit)
	  (unit signature)
	  (unit program)
	  (unit get/extend)
	  (unit rep)))