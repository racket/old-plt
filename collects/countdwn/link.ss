(compound-unit/sig
 (import [I : mred:application-imports^])
 (link [W : wx^ (wx@)]
       [C : mzlib:core^ ((require-library-unit/sig "corer.ss"))]
       [T : mzlib:date^ ((require-library-unit/sig "dater.ss") (C function@))]
       [M : mred^ ((require-library-unit/sig "linkwx.ss" "mred") C W)]
       [R : countdown^ ((require-unit/sig "remind.ss") W T 
			(C thread@) (C function@) (C pretty-print@)
			M)]
       [B : before^ ((require-unit/sig "before.ss") R M)]
       [D : during^ ((require-unit/sig "during.ss") 
		     W
		     B
		     T
		     (C function@)
		     (C string@)
		     (C pretty-print@)
		     M)]
       [F : () ((require-unit/sig "after.ss") B)])
 (export))
