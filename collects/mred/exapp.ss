(lambda (myapp-unt)
  (compound-unit/sig (import [I : mred:application-imports^])
    (link [mzlib : mzlib:core^ ((require-library-unit/sig "corer.ss"))]
	  [wx : wx^ (wx@)]
	  [mred : mred^
		((require-library-unit/sig "linkwx.ss" "mred") wx mzlib)]
	  [myapp : mred:application^ (myapp-unit wx mred I)])
    (export (unit myapp))))
