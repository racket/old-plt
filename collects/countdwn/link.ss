(compound-unit/sig
 (import [I : mred:application-imports^])
 (link [C : mzlib:core^ ((reference-library-unit/sig "corer.ss"))]
       [T : mzlib:date^ ((reference-library-unit/sig "dater.ss") (C function@))]
       [A : mred:application^ ((unit/sig mred:application^
				 (import)
				 (define app-name "Countdown")
				 (define eval-string void)
				 (define console #f)))]
       [M : mred^ ((reference-library-unit/sig "link.ss" "mred") C A)]
       [R : countdown^ ((reference-unit/sig "remind.ss") T 
			(C thread@) (C function@) (C pretty-print@)
			M)]
       [B : before^ ((reference-unit/sig "before.ss") R M)]
       [D : during^ ((reference-unit/sig "during.ss") B)]
       [U : () ((reference-unit/sig "user.ss") D)]
       [F : () ((reference-unit/sig "after.ss") B)])
 (export))
