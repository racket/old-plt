(compound-unit/sig 
   (import [I : mred:application-imports^])
   (link [mzlib : mzlib:core^ 
            ((require-library-unit/sig "corer.ss"))]
	 [wx : wx^ (wx@)]
         [mred : mred^
            ((require-library-unit/sig "linkwx.ss" "mred") mzlib wx)]
         [myapp : ()
            ((unit/sig ()
	      (import)
	      (require-library "mzlib.ss")
	      (require-library "demo.ss" "mrdemo")))])
   (export (unit mred) (unit wx)))

