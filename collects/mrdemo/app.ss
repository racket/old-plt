(compound-unit/sig 
   (import [I : mred:application-imports^])
   (link [mzlib : mzlib:core^ 
            ((reference-library-unit/sig "corer.ss"))]
	 [wx : wx^ (wx@)]
         [mred : mred^
            ((reference-library-unit/sig "linkwx.ss" "mred") mzlib wx)]
         [myapp : ()
            ((unit/sig ()
	      (import)
	      (require-library "mzlib.ss")
	      (require-library "demo.ss" "mrdemo")))])
   (export (unit mred) (unit wx)))

