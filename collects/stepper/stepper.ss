; stepper.ss

(compound-unit/sig 
  (import (core : mzlib:core^)
          (print-convert : mzlib:print-convert^)
          (mred : mred^)
          (drscheme : drscheme:export^)
          (zodiac : drscheme:zodiac^))
  (link [ERROR : stepper:error^ ((unit/sig stepper:error^
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
                                     (default-error-handler 'runtime-syntax-error))))]
        [PRETTY : mzlib:pretty-print^ ((require-library-unit/sig "prettyr.ss"))]
        [SHARED : stepper:shared^ ((require-library-unit/sig "sharedr.ss" "stepper")
                                   zodiac
                                   error)]
        [ANNOTATE : stepper:annotate^
                  ((require-library-unit/sig "annotater.ss" "stepper")
                   zodiac
                   (core function)
                   error
                   shared)]
        [RECONSTRUCT : stepper:reconstruct^ 
                     ((require-library-unit/sig "reconstructr.ss" "stepper")
                      zodiac
                      (core function)
                      error
                      print-convert
                      (drscheme basis)
                      shared)]
        [STEPPER : ()
                 ((require-library-unit/sig "stepperr.ss" "stepper")
                  pretty
                  mred
                  (drscheme basis)
                  annotate
                  reconstruct)])                   
      (export))
