(compound-unit/sig
 (import [I : mred:application-imports^])
 (link [C : mzlib:core^ ((reference-library-unit/sig "corer.ss"))]
       [T : mzlib:date^ ((reference-library-unit/sig "dater.ss") (C function@))]
       [M : mred^ ((reference-library-unit/sig "link.ss" "mred") C)]
       [R : countdown^ ((reference-unit/sig "remind.ss") T 
			(C thread@) (C function@) (C pretty-print@)
			M)]
       [B : before^ ((reference-unit/sig "before.ss") R M)]
       [D : during^ ((reference-unit/sig "during.ss") 
		     B
		     T
		     (C function@)
		     (C string@)
		     (C pretty-print@)
		     M)]
       [F : () ((reference-unit/sig "after.ss") B)])
 (export))
