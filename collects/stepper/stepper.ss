; stepper.ss

(compound-unit/sig 
  (import (core : mzlib:core^)
          (framework : framework^)
          (print-convert : mzlib:print-convert^)
          (mred : mred^)
          (drscheme : drscheme:export^)
          (zodiac : zodiac:system^)
          (zcp : stepper:zodiac-client-procs^))
  (link [error : stepper:error^ ((unit/sig stepper:error^
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
        [pretty : mzlib:pretty-print^ ((require-library-unit/sig "prettyr.ss"))]
        [shared : stepper:shared^ ((require-library-unit/sig "sharedr.ss" "stepper")
                                   zodiac
                                   error
                                   zcp)]
        [annotate : stepper:annotate^
                  ((require-library-unit/sig "annotater.ss" "stepper")
                   zodiac
                   (core function)
                   error
                   stepper
                   shared
                   zcp)]
        [reconstruct : stepper:reconstruct^ 
                     ((require-library-unit/sig "reconstructr.ss" "stepper")
                      zodiac
                      (core function)
                      error
                       (drscheme basis)
                      stepper
                      shared)]
        [stepper : stepper:settings^
                 ((require-library-unit/sig "stepperr.ss" "stepper")
                  zodiac
                  pretty
                  mred
                  drscheme
                  print-convert
                  error
                  annotate
                  reconstruct
                  framework
                  shared)])       
      (export))
