(compound-unit/sig (import [I : mred:application-imports^])
  (link [wx : wx^ (wx@ ; (require-library-unit/sig "fakewx.ss" "mred")
		   )]
	[init : drscheme:init^ ((require-relative-library "init.ss") wx mred)]
	[mzlib : mzlib:core^ ((require-library-unit/sig "corer.ss"))]
	[mred : mred^ ((require-library-unit/sig "link.ss" "mred") mzlib)]
	[print-convert : mzlib:print-convert^
		       ((require-library-unit/sig "pconverr.ss")
			(mzlib string@)
			(mzlib function@))]
	[face : drscheme:face^ ((require-relative-library "face.ss") mred)]
	[prefs : drscheme:prefs^ ((require-relative-library "prefs.ss") mred)]
	[aries : plt:aries^ ((require-library-unit/sig "ariesr.ss" "cogen")
			     (drzodiac : zodiac:system^)
			     (interface : zodiac:interface^))]
	[interface : drscheme:interface^
		   ((require-library-unit/sig "interface.ss" "userspce") drzodiac)]
	[drzodiac : drscheme:zodiac^
		  ((require-library-unit/sig "zlink.ss" "userspce")
		   (export* basis)
		   (interface : zodiac:interface^)
		   (mzlib pretty-print@)
		   (mzlib file@))]
	[edit : drscheme:edit^ ((require-relative-library "edit.ss") 
				mred aries drzodiac)]
	[setup : drscheme:setup^ ((require-relative-library "setup.ss") wx mred mzlib)]
	[snip : drscheme:snip^ ((require-relative-library "snip.ss") wx mred)]
	[export* : drscheme:export^ ((require-relative-library "export.ss")
				    wx mred mzlib print-convert app
				    edit language setup snip
				    init interface face
				    aries drzodiac)]
	[language : drscheme:language^
		  ((require-relative-library "language.ss")
		   wx mred 
		   (export* unit)
		   aries drzodiac
		   (export* basis)
		   (mzlib function@) 
		   (mzlib file@)
		   print-convert)]
	[tool : () 
	      ((require-relative-library "tool.ss")
	       wx mred mzlib print-convert 
	       drzodiac
	       export*)]
	[app : drscheme:app^ ((require-relative-library "app.ss")
			      wx
			      mred
			      mzlib)]
	[main : drscheme:main^ ((require-relative-library "main.ss")
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