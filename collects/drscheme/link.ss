(compound-unit/sig (import [I : mred:application-imports^])
  (link [init : drscheme:init^ ((reference-unit/sig "init.ss") mred)]
	[mzlib : mzlib:core^ ((reference-library-unit/sig "corer.ss"))]
	[mred : mred^ ((reference-library-unit/sig "link.ss" "mred") mzlib)]
	[print-convert : mzlib:print-convert^
		       ((reference-library-unit/sig "pconverr.ss")
			(mzlib string@)
			(mzlib function@))]
	[face : drscheme:face^ ((reference-unit/sig "face.ss") mred)]
	[prefs : drscheme:prefs^ ((reference-unit/sig "prefs.ss") mred)]
	[basis : drscheme:basis^
	       ((reference-unit/sig "basis.ss")
		init language mred drzodiac)]
	[aries : plt:aries^ ((reference-library-unit/sig "ariesr.ss" "cogen")
			     (drzodiac : zodiac:system^)
			     (interface : zodiac:interface^))]
	[language : drscheme:language^
		  ((reference-unit/sig "language.ss")
		   mred basis aries drzodiac
		   (mzlib function@) print-convert)]
	[interface : drscheme:interface^
		   ((reference-unit/sig "intrface.ss") 
		    drzodiac
		    init
		    mred)]
	[drzodiac : drscheme:zodiac^
		  ((reference-unit/sig "zlink.ss")
		   mred
		   basis
		   interface
		   (mzlib pretty-print@)
		   (mzlib file@))]
	[edit : drscheme:edit^ ((reference-unit/sig "edit.ss") 
				mred aries drzodiac)]
	[setup : drscheme:setup^ ((reference-unit/sig "setup.ss") mred mzlib)]
	[snip : drscheme:snip^ ((reference-unit/sig "snip.ss") mred)]
	[export : drscheme:export^ ((reference-unit/sig "export.ss")
				    mred mzlib print-convert app
				    basis edit language setup snip
				    init interface face
				    aries drzodiac)]
	[tool : () 
	      ((reference-unit/sig "tool.ss")
	       mred mzlib print-convert 
	       drzodiac
	       export)]
	[app : drscheme:app^ ((reference-unit/sig "app.ss")
			      mred
			      mzlib)]
	[main : drscheme:main^ ((reference-unit/sig "main.ss")
				I
				mred
				print-convert
				(export unit)
				(export compound-unit)
				(export get/extend))])
  (export (unit init)
	  (unit mzlib)
	  (unit mred)
	  (unit print-convert)
	  (unit prefs drscheme:prefs)
	  (unit basis drscheme:basis)
	  (unit aries drscheme:aries)
	  (unit language drscheme:language)
	  (unit interface drscheme:interface)
	  (unit drzodiac zodiac)
	  (unit edit drscheme:edit)
	  (unit setup drscheme:setup)
	  (unit snip drscheme:snip)
	  (unit export drscheme:export)
	  (unit tool drscheme:tool)
	  (unit app drscheme:app)
	  (unit main drscheme:main)
	  (unit face drscheme:face)))