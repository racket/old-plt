(compound-unit/sig (import [I : (program argv)])
  (link [mred : mred-interfaces^ (mred-interfaces@)]
	[mzlib : mzlib:core^ ((require-library-unit/sig "corer.ss"))]
	[init : drscheme:init^ ((require-relative-library "init.ss") mred)]
	[framework : framework^ ((require-library "frameworkr.ss" "framework") mzlib mred)]
	[print-convert : mzlib:print-convert^
		       ((require-library-unit/sig "pconverr.ss")
			(mzlib string)
			(mzlib function))]
	[face : drscheme:face^ ((require-relative-library "face.ss") mred)]
	[prefs : drscheme:prefs^ ((require-relative-library "prefs.ss") mred framework)]
	[aries : plt:aries^ ((require-library-unit/sig "ariesr.ss" "cogen")
			     zodiac
			     (interface : zodiac:interface^))]
	[interface : drscheme:interface^
		   ((require-library-unit/sig "interface.ss" "userspce") zodiac)]
	[zodiac : zodiac:system^
		  ((require-library-unit/sig "link.ss" "zodiac")
		   (interface : zodiac:interface^)
		   (mzlib pretty-print)
		   (mzlib file))]
	[edit : drscheme:edit^ ((require-relative-library "edit.ss") framework aries zodiac)]
	[snip : drscheme:snip^ ((require-relative-library "snip.ss") mred)]
	[graph : drscheme:graph^ ((require-relative-library "graph.ss") mred framework (mzlib string) (mzlib function))]
	[export* : drscheme:export^ ((require-relative-library "export.ss")
				     mred mzlib framework print-convert app
				     edit language snip
				     init interface face graph
				     aries zodiac)]
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
			      framework)]
	[main : drscheme:main^ ((require-relative-library "main.ss")
				I
				framework
				(mzlib pretty-print)
				print-convert
				(export* unit)
				(export* compound-unit)
				(export* get/extend))])
  (export (unit init)
	  (unit mzlib)
	  (unit mred)
	  (unit print-convert)
	  (unit prefs drscheme:prefs)
	  (unit aries drscheme:aries)
	  (unit language drscheme:language)
	  (unit interface drscheme:interface)
	  (unit zodiac zodiac)
	  (unit edit drscheme:edit)
	  (unit snip drscheme:snip)
	  (unit export* drscheme:export)
	  (unit tool drscheme:tool)
	  (unit app drscheme:app)
	  (unit main drscheme:main)
	  (unit face drscheme:face)))