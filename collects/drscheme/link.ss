(compound-unit/sig (import [i : (program argv)])
  (link [mred : mred^ (mred@)]
	[mzlib : mzlib:core^ ((require-library "corer.ss"))]
	[init : drscheme:init^ ((require-relative-library "init.ss") mred)]
	[framework : framework^ ((require-library "frameworkr.ss" "framework") mzlib mred)]
	[print-convert : mzlib:print-convert^
		       ((require-library "pconverr.ss")
			(mzlib string)
			(mzlib function))]
	[prefs : drscheme:prefs^ ((require-relative-library "prefs.ss")
				  mred framework
				  (export* basis))]
	[text : drscheme:text^ ((require-relative-library "edit.ss") framework aries zodiac)]
	[snip : drscheme:snip^ ((require-relative-library "snip.ss") mred)]
	[graph : drscheme:graph^ ((require-relative-library "graph.ss") mred framework (mzlib string) (mzlib function))]
        [export* : drscheme:export^ ((require-relative-library "export.ss")
				     mred mzlib framework print-convert app
				     text language snip
				     init graph intro
				     aries zodiac)]
	[zodiac : zodiac:system^
		  ((require-library "link.ss" "zodiac")
		   ((export* interface) : zodiac:interface^)
		   (mzlib pretty-print)
		   (mzlib file))]
	[aries : plt:aries^ ((require-library "ariesr.ss" "cogen")
			     zodiac
			     ((export* interface) : zodiac:interface^))]
	[language : drscheme:language^
		  ((require-relative-library "language.ss")
		   mred framework
		   (export* unit)
		   aries zodiac
		   (export* basis)
		   (mzlib function) 
		   (mzlib file)
		   print-convert)]
	[tool : () 
	      ((require-relative-library "tool.ss")
	       mred mzlib framework
	       print-convert 
	       zodiac
	       export*)]
	[app : drscheme:app^ ((require-relative-library "app.ss")
			      mred
			      mzlib
			      framework
			      (export* unit)
			      (export* frame)
			      (export* help-desk))]

	[intro : drscheme:intro^ ((require-relative-library "intro.ss")
				  mred
                                  framework
                                  (export* help-desk))]

	[main : drscheme:main^ ((require-relative-library "main.ss")
				i
				framework
				(mzlib pretty-print)
				print-convert
				intro
				(export* unit)
				(export* get/extend)
				(export* basis)
				(mzlib function))])
  (export (unit init)
	  (unit mzlib)
	  (unit mred)
	  (open framework)
	  (unit print-convert)
	  (unit prefs drscheme:prefs)
	  (unit aries drscheme:aries)
	  (unit language drscheme:language)
	  (unit zodiac zodiac)
	  (unit text drscheme:text)
	  (unit snip drscheme:snip)
	  (unit export* drscheme:export)
	  (unit tool drscheme:tool)
	  (unit app drscheme:app)
	  (unit main drscheme:main)))