(lambda (myapp-unt)
  (compound-unit/sig (import [I : mred:application-imports^])
    (link [mzlib : mzlib:core^ ((reference-library-unit/sig "corer.ss"))]
	  [wx : wx^ (wx@)]
	  [mred : mred^
		((reference-library-unit/sig "linkwx.ss" "mred") wx mzlib)]
	  [myapp : mred:application^ (myapp-unit wx mred I)])
    (export (unit myapp))))
