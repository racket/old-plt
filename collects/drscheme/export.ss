(compound-unit/sig (import [mred : mred^]
			   [mzlib : mzlib:core^]
			   [mzlib:date : mzlib:date^]
			   [framework : framework^]
			   [print-convert : mzlib:print-convert^]
			   [app : drscheme:app^]
			   [text : drscheme:text^]
			   [snip : drscheme:snip^]
			   [init : drscheme:init^]
			   [graph : drscheme:graph^]
			   [aries : plt:aries^]
			   [zodiac : zodiac:system^])
  (link [interface : drscheme:interface^
          ((require-library "interface.ss" "userspce") aries zodiac)]
        [url : mzlib:url^ ((require-library "urlr.ss" "net") (mzlib file))]
	[basis-import : plt:basis-import^ ((unit/sig plt:basis-import^
					     (import mred^)
					     (define (invalid-teachpack s)
					       (message-box "Invalid Teachpack" s))
					     (define in-mzscheme? #f))
					   mred)]
        [params : plt:userspace:params^ ((require-library "paramr.ss" "userspce"))]
	[basis : plt:basis^
	       ((require-library "basis.ss" "userspce")
		basis-import
		params
		zodiac
		interface
		aries
		print-convert
		(mzlib pretty-print)
		(mzlib function))]
	[load-handler : drscheme:load-handler^
                      ((require-library "load-handler.ss" "drscheme")
		       mred zodiac basis 
		       (framework gui-utils))]

        [rep : drscheme:rep^
	     ((require-relative-library "rep.ss")
	      mred mzlib framework print-convert zodiac
	      interface init snip language app frame unit
	      basis text load-handler
              help-desk)]

	[frame : drscheme:frame^
	       ((require-relative-library "frame.ss")
		mred mzlib mzlib:date framework
		unit app
		help-desk zodiac)]
        [dynext-compiler : dynext:compiler^ ((require-library "compiler.ss" "dynext"))]
        [dynext-linker : dynext:link^ ((require-library "linkr.ss" "dynext"))]
	[launcher : launcher-maker^ ((require-library "launcherr.ss" "launcher")
                                     (mzlib file)
                                     dynext-compiler
                                     dynext-linker)]
        [unit : drscheme:unit^
	  ((require-relative-library "unit.ss")
	   mred mzlib mzlib:date 
           framework
           launcher basis
           app frame text rep
	   language get/extend graph)]
	[program : drscheme:program^ ((require-relative-library "prog.ss"))]
	[get/extend : drscheme:get/extend^
		    ((require-relative-library "params.ss") 
		     mred unit frame rep mzlib)]
	[language : drscheme:language^
		  ((require-relative-library "language.ss")
		   mred framework
		   unit
		   zodiac
		   basis
		   (mzlib function) 
		   (mzlib file)
		   print-convert)]

	[help-info : help:get-info^ ((require-relative-library "help-info.ss")
                                     framework
                                     basis
                                     language)]
        [help-desk : help:drscheme-interface^
		   ((require-library "start-help-desk.ss" "help")
		    (mzlib function)
		    (mzlib string)
		    (mzlib file)
		    url
		    (mred : mred^)
		    framework
		    frame
		    language
		    basis)])

	
  (export (unit help-info)
          (unit interface)
          (unit basis)
	  (unit frame)
	  (unit unit)
	  (unit program)
	  (unit get/extend)
	  (unit load-handler)
	  (unit rep)
	  (unit help-desk)
	  (unit language)))
