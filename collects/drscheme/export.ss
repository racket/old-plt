(compound-unit/sig (import [mred : mred-interfaces^]
			   [mzlib : mzlib:core^]
			   [framework : framework^]
			   [print-convert : mzlib:print-convert^]
			   [app : drscheme:app^]
			   [text : drscheme:text^]
			   [language : drscheme:language^]
			   [snip : drscheme:snip^]
			   [init : drscheme:init^]
			   [interface : drscheme:interface^]
			   [graph : drscheme:graph^]
			   [help-desk : help:drscheme-interface^]
			   [aries : plt:aries^]
			   [zodiac : drscheme:zodiac^])
  (link [basis-import : userspace:basis-import^ ((unit/sig userspace:basis-import^
						   (import)
						   (define in-mzscheme? #f)))]
	[params : plt:userspace:params^ ((require-library "paramr.ss" "userspce"))]
	[basis : userspace:basis^
	       ((require-library "basis.ss" "userspce")
		basis-import
		params
		zodiac
		interface
		aries
		print-convert
		(mzlib pretty-print)
		(mzlib function))]
	[rep : drscheme:rep^
	     ((require-relative-library "rep.ss")
	      mred mzlib framework print-convert zodiac
	      interface init snip language app frame basis text
              help-desk)]
	[frame : drscheme:frame^
	       ((require-relative-library "frame.ss")
		mred mzlib framework
		unit app
		help-desk zodiac)]
	[unit : drscheme:unit^
	  ((require-relative-library "unit.ss")
	   mred mzlib framework app frame text rep
	   language get/extend graph)]
	[program : drscheme:program^ ((require-relative-library "prog.ss"))]
	[get/extend : drscheme:get/extend^
		    ((require-relative-library "params.ss") 
		     mred unit frame rep mzlib)])
  (export (unit basis)
	  (unit frame)
	  (unit unit)
	  (unit program)
	  (unit get/extend)
	  (unit rep)))