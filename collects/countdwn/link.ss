(compound-unit/sig
 (import)
 (link [C : mzlib:core^ ((require-library-unit/sig "corer.ss"))]
       [T : mzlib:date^ ((require-library-unit/sig "dater.ss") (C function))]
       [M : mred^ (mred@)]
       [R : countdown^ ((require-unit/sig "remind.ss") T 
			(C thread) (C function) (C pretty-print)
			M)]
       [B : before^ ((require-unit/sig "before.ss") R M)]
       [D : during^ ((require-unit/sig "during.ss") 
		     B
		     T
		     (C function)
		     (C string)
		     (C pretty-print)
		     M)]
       [F : () ((require-unit/sig "after.ss") B M)])
 (export))
