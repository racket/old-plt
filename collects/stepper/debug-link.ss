(compound-unit/sig 
  (import (core : mzlib:core^)
          (drscheme : drscheme:export^)
          (zodiac : zodiac:system^)
          (mred : mred^))
  (link [error : stepper:error^ ((unit/sig stepper:error^
                                   (import)
                                   (define default-error-handler
                                     (lambda (keyword)
                                       (lambda (where fmt-spec . args)
					; (printf "Error at: ~s~n" where)
					 (printf "error: ~s~n" (list where fmt-spec args))
                                         (apply error keyword fmt-spec args))))
                                   (define internal-error
                                     (default-error-handler 'internal-error))
                                   (define static-error
                                     (default-error-handler 'syntax-error))
                                   (define dynamic-error
                                     (default-error-handler 'runtime-syntax-error))))]
        [marks : stepper:marks^
               ((require-library-unit/sig "marks.ss" "stepper")
                zodiac
                zcp
                (core function))]
        [utils : cogen-utils^ ((require-library-unit/sig "cogen-utilsr.ss" "cogen")
                               zodiac
                               (error : (internal-error static-error)))] 
        [zcp : stepper:zodiac-client-procs^
             ((require-library-unit/sig "debug-zcp.ss" "stepper")
              zodiac)]
        [shared : stepper:shared^ ((require-library-unit/sig "sharedr.ss" "stepper")
                                   zodiac
                                   error
                                   zcp)]
        [fake-stepper : stepper:model^
                      ((require-library-unit/sig "fake-model.ss" "stepper"))]
        [annotate : stepper:annotate^
                  ((require-library-unit/sig "annotater.ss" "stepper")
                   zodiac
                   (core function)
                   error
                   utils
                   fake-stepper
                   shared
                   zcp)]
        [debug-wrapper : plt:aries^
                          ((require-library-unit/sig "debug-wrapper.ss" "stepper")
                           drscheme
                           zodiac
                           mred
                           marks
                           annotate)])       
  (export (open debug-wrapper)))