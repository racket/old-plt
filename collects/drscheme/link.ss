(compound-unit/sig (import [I : mred:application-imports^])
  (link [wx : wx^ (wx@)]
	[init : drscheme:init^ ((require-unit/sig "init.ss") wx mred)]
	[mzlib : mzlib:core^ ((require-library-unit/sig "corer.ss"))]
	[mred : mred^ ((require-library-unit/sig "link.ss" "mred") mzlib)]
	[print-convert : mzlib:print-convert^
		       ((require-library-unit/sig "pconverr.ss")
			(mzlib string@)
			(mzlib function@))]
	[face : drscheme:face^ ((require-unit/sig "face.ss") mred)]
	[prefs : drscheme:prefs^ ((require-unit/sig "prefs.ss") mred)]
	[basis : drscheme:basis^
	       ((require-unit/sig "basis.ss")
		wx init language mred drzodiac)]
	[aries : plt:aries^ ((require-library-unit/sig "ariesr.ss" "cogen")
			     (drzodiac : zodiac:system^)
			     (interface : zodiac:interface^))]
	[language : drscheme:language^
		  ((require-unit/sig "language.ss")
		   wx mred 
		   basis
		   (export* unit)
		   aries drzodiac
		   (mzlib function@) print-convert)]
	[interface : drscheme:interface^
		   ((require-unit/sig "intrface.ss") 
		    drzodiac
		    init
		    mred)]
	[drzodiac : drscheme:zodiac^
		  ((require-unit/sig "zlink.ss")
		   mred
		   basis
		   interface
		   (mzlib pretty-print@)
		   (mzlib file@))]
	[edit : drscheme:edit^ ((require-unit/sig "edit.ss") 
				mred aries drzodiac)]
	[setup : drscheme:setup^ ((require-unit/sig "setup.ss") wx mred mzlib)]
	[snip : drscheme:snip^ ((require-unit/sig "snip.ss") wx mred)]
	[export* : drscheme:export^ ((require-unit/sig "export.ss")
				    wx mred mzlib print-convert app
				    basis edit language setup snip
				    init interface face
				    aries drzodiac)]
	[tool : () 
	      ((require-unit/sig "tool.ss")
	       wx mred mzlib print-convert 
	       drzodiac
	       export*)]
	[app : drscheme:app^ ((require-unit/sig "app.ss")
			      wx
			      mred
			      mzlib)]
	[main : drscheme:main^ ((require-unit/sig "main.ss")
				wx I
				mred
				(mzlib pretty-print@)
				print-convert
				(export* unit)
				(export* compound-unit)
				(export* get/extend))])
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
	  (unit export* drscheme:export)
	  (unit tool drscheme:tool)
	  (unit app drscheme:app)
	  (unit main drscheme:main)
	  (unit face drscheme:face)))