; stepper-test.ss

(require-library "errortrace.ss" "errortrace")

(require-library "sig.ss" "stepper")
(require-library "zsigs.ss" "zodiac")
(require-library "sigs.ss" "zodiac")

(define stepper:default-error@
  (unit/sig stepper:error^
    (import)
    (define default-error-handler
      (lambda (keyword)
	(lambda (where fmt-spec . args)
	  (printf "Error at: ~s~n" where)
	  (apply error keyword fmt-spec args))))
    (define internal-error
      (default-error-handler 'internal-error))
    (define static-error
      (default-error-handler 'syntax-error))
    (define dynamic-error
      (default-error-handler 'runtime-syntax-error))))

    
(define stepper-test@
  (compound-unit/sig
    (import)
    (link [FUNCTION : mzlib:function^ ((require-library-unit/sig "functior.ss"))]
	  [ERROR : stepper:error^ (stepper:default-error@)]
	  [PRETTY : mzlib:pretty-print^ ((require-library-unit/sig "prettyr.ss"))]
	  [MZLIB-STRING : mzlib:string^ ((require-library-unit/sig "stringr.ss"))]
	  [MZLIB-FILE : mzlib:file^ ((require-library-unit/sig "filer.ss")
				     MZLIB-STRING
				     FUNCTION)]
	  [ZODIAC : zodiac:system^ ((require-library-unit/sig "link.ss" "zodiac")
				    (ERROR : zodiac:interface^)
				    PRETTY
				    MZLIB-FILE)]
	  [SHARED : stepper:shared^ ((require-library-unit/sig "sharedr.ss" "stepper")
				     ZODIAC
                                     ERROR)]
	  [ANNOTATE : stepper:annotate^
		    ((require-library-unit/sig "annotater.ss" "stepper")
		     ZODIAC
		     FUNCTION
		     ERROR
		     SHARED
		     RECONSTRUCT)]
	  [RECONSTRUCT : stepper:reconstruct^ 
		       ((require-library-unit/sig "reconstructr.ss" "stepper")
			ZODIAC
                        FUNCTION
			ERROR
			SHARED)]
          [STEPPER : stepper:stepper^
                   ((require-library-unit/sig "stepperr.ss" "stepper")
                    PRETTY
                    ANNOTATE
                    RECONSTRUCT)])
    (export (unit STEPPER))))

(invoke-open-unit/sig stepper-test@)

;(define lookup (stepper:stepper-start "((lambda (x) x) (let ([x 3] [y 3]) (+ x y)))"))

(define (s) (stepper:stepper-step))

;(stepper:stepper-start 
;  "(define a 3)
;  (define b 4)
;  (define c (+ 4 3))
;  3")

(stepper:stepper-start
 "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
 (fact 4)")
   
;(stepper:stepper-start
; "(define (fact n) (if (= n 0) 1 (* n (- n 1))))
; (fact 4)")  
             
                                   
                     
                     

