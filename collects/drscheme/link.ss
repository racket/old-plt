(compound-unit/sig (import [mred : mred^]
			   [mzlib : mzlib:core^])
  (link [hooks : mzlib:print-convert-hooks^ ((reference-unit/sig "phooks.ss"))]
	[print-convert : mzlib:print-convert^
		       ((reference-library-unit/sig "pconverr.ss")
			(mzlib string@)
			(mzlib function@)
			hooks)]
	[interface : drscheme:interface^
		   ((reference-unit/sig "intrface.ss") zodiac mred)]
	[basis : drscheme:basis^
	       ((reference-unit/sig "basis.ss") (language : plt:parameters^) mred zodiac)]
	[language : drscheme:language^
		  ((reference-unit/sig "language.ss")
		   mred basis (mzlib function@)
		   print-convert)]
	[zodiac : zodiac:system^ ((reference-unit/sig (begin-construction-time
						       (build-path plt:home-directory
								   "zodiac"
								   "link.ss")))
				  (interface : zodiac:interface^)
				  (language : plt:parameters^)
				  (mzlib pretty-print@))]
	[aries : plt:aries^ ((reference-library-unit/sig "ariesu.ss" "cogen")
			     zodiac
			     (interface : zodiac:interface^))]
	[edit : drscheme:edit^ ((reference-unit/sig "edit.ss") mred aries zodiac)]
	[setup : drscheme:setup^ ((reference-unit/sig "setup.ss") mred mzlib)]
	[snip : drscheme:snip^ ((reference-unit/sig "snip.ss") mred)]
	[rep : drscheme:rep^
	     ((reference-unit/sig "rep.ss")
	      mred mzlib print-convert aries zodiac
	      interface snip language app basis edit)]
	[export : drscheme:export^ ((reference-unit/sig "export.ss")
				    mred mzlib app
				    basis edit language rep setup
				    zodiac)]
	[tool : () 
	      ((reference-unit/sig "tool.ss")
	       mred mzlib print-convert zodiac (language : plt:parameters^)
	       export)]
	[app : drscheme:app^ ((reference-unit/sig "app.ss")
			      (export unit) 
			      (export frame)
			      (export parameters)
			      mred mzlib)])
  (export (open app)))