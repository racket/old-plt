; stepper-test.ss

(require-library "sig.ss" "stepper")
(require-library "sigs.ss" "zodiac")

(define dummy-interface@
  (unit/sig zodiac:interface^
    (import)
    (define static-error 
      (lambda args
	(raise (cons 'static-error args))))
    (define internal-error
      (lambda args
	(raise (cons 'internal-error args))))
    ))		    
    
(define stepper-test@
  (compound-unit/sig
    (import)
    (link [FUNCTION : mzlib:function^ ((require-library-unit/sig "functior.ss"))]
	  [ZODIAC-INTERFACE : zodiac:interface^ (dummy-interface@)]
	  [PRETTY : mzlib:pretty-print^ ((require-library-unit/sig "prettyr.ss"))]
	  [MZLIB-STRING : mzlib:string^ ((require-library-unit/sig "stringr.ss"))]
	  [MZLIB-FILE : mzlib:file^ ((require-library-unit/sig "filer.ss")
				     MZLIB-STRING
				     FUNCTION)]
	  [ZODIAC : zodiac:system^ ((require-library-unit/sig "link.ss" "zodiac")
				    ZODIAC-INTERFACE
				    PRETTY
				    MZLIB-FILE)]
	  [STEPPER : stepper^ ((require-library-unit/sig "stepperr.ss" "stepper")
			       FUNCTION
			       ZODIAC)])
    (export (unit STEPPER))))

(invoke-open-unit/sig stepper-test@)

	  