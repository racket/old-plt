(compound-unit/sig (import [top-level : (program argv get-dropped-files)])
  (link [mred : mred^ (mred@)]
	[mzlib : mzlib:core^ ((require-library "corer.ss"))]
	[mzlib:date : mzlib:date^ ((require-library "dater.ss") (mzlib function))]
	[init : drscheme:init^ ((require-relative-library "init.ss") mred)]
	[framework : framework^ ((require-library "frameworkr.ss" "framework") mzlib mred)]
	[print-convert : mzlib:print-convert^
		       ((require-library "pconverr.ss")
			(mzlib string)
			(mzlib function))]
	[text : drscheme:text^ ((require-relative-library "edit.ss")
				mzlib:date framework zodiac)]
	[snip : drscheme:snip^ ((require-relative-library "snip.ss") mred)]
	[graph : drscheme:graph^ ((require-relative-library "graph.ss")
                                  mred framework (mzlib string) (mzlib function))]
        [export* : drscheme:export^ ((require-relative-library "export.ss")
				     mred mzlib mzlib:date framework print-convert app
				     text snip
				     init graph
				     cogen zodiac)]

	[prefs : drscheme:prefs^ ((require-relative-library "prefs.ss")
				  (export* language)
				  mred framework
				  (export* basis))]
	[zodiac : zodiac:system^
		  ((require-library "link2.ss" "zodiac")
		   ((export* interface) : zodiac:interface^)
		   (mzlib pretty-print)
		   (mzlib file))]

        [cogen : plt:aries^
               ((require-library-unit/sig "link.ss" "stepper-graphical")
                mzlib
                framework
                print-convert
                mred
                export*
                zodiac
                ((export* interface) : zodiac:interface^))]
	[app : drscheme:app^ ((require-relative-library "app.ss")
			      mred
			      mzlib
			      framework
			      (export* unit)
			      (export* frame)
			      (export* help-desk))]


	[tool : () 
	      ((require-relative-library "tool.ss")
	       mred mzlib framework
	       print-convert 
	       zodiac
	       export*)]

	[main : drscheme:main^ ((require-relative-library "main.ss")
				top-level
                                mred
				framework
				(mzlib pretty-print)
				print-convert
				app
				(export* unit)
				(export* get/extend)
				(export* basis)
				(mzlib function)
                                (mzlib file))])
  (export (unit init)
	  (unit mzlib)
	  (unit mred)
	  (open framework)
	  (unit print-convert)
	  (unit prefs drscheme:prefs)
	  (unit cogen drscheme:cogen)
	  (unit zodiac zodiac)
	  (unit text drscheme:text)
	  (unit snip drscheme:snip)
	  (unit export* drscheme:export)
	  (unit tool drscheme:tool)
	  (unit app drscheme:app)
	  (unit main drscheme:main)))