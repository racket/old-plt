; stepper-test.ss

(require-library "sig.ss" "stepper")
(require-library "zsigs.ss" "zodiac")
(require-library "sigs.ss" "zodiac")

; zodiac:default-interface@ is COPIED (gasp!) from
; zodiac/invoke.ss

(define zodiac:default-interface@
  (unit/sig zodiac:interface^
    (import)
    (define default-error-handler
      (lambda (keyword)
	(lambda (where fmt-spec . args)
	  (printf "Error at: ~s~n" where)
	  (apply error keyword fmt-spec args))))
    (define internal-error
      (default-error-handler 'internal-error))
    (define static-error
      (default-error-handler 'syntax-error))))
    
(define stepper-test@
  (compound-unit/sig
    (import)
    (link [FUNCTION : mzlib:function^ ((require-library-unit/sig "functior.ss"))]
	  [ZODIAC-INTERFACE : zodiac:interface^ (zodiac:default-interface@)]
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

	  