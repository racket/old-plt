(compound-unit/sig 
  (import (core : mzlib:core^)
          (zodiac : zodiac:system^))
  (link [pretty : mzlib:pretty-print^ ((require-library-unit/sig "prettyr.ss"))]
        [error : stepper:error^ ((unit/sig stepper:error^
                                   (import)
                                   (define default-error-handler
                                     (lambda (keyword)
                                       (lambda (where fmt-spec . args)
                                         ; (printf "Error at: ~s~n" where)
                                         (apply error keyword fmt-spec args))))
                                   (define internal-error
                                     (default-error-handler 'internal-error))
                                   (define static-error
                                     (default-error-handler 'syntax-error))
                                   (define dynamic-error
                                     (default-error-handler 'runtime-syntax-error))))]
        [client-procs : stepper:client-procs^
                      ((require-library-unit/sig "client-procs.ss" "stepper")
                       zodiac)]
        [marks : stepper:marks^
               ((require-library-unit/sig "marks.ss" "stepper")
                zodiac
                client-procs
                (core function))]
        [utils : stepper:cogen-utils^ 
               ((require-library-unit/sig "cogen-utilsr.ss" "cogen")
                zodiac
                (error : (internal-error static-error)))]        
        [shared : stepper:shared^ ((require-library-unit/sig "sharedr.ss" "stepper")
                                   zodiac
                                   error
                                   client-procs)]
        [fake-stepper : stepper:model^
                     ((require-library-unit/sig "fake-model.ss" "stepper"))]
        [annotate : stepper:annotate^
                  ((require-library-unit/sig "annotater.ss" "stepper")
                   zodiac
                   (core function)
                   error
                   utils
                   marks
                   fake-stepper
                   shared
                   client-procs)]
        [debug-wrapper : plt:aries-no-break^
                          ((require-library-unit/sig "debug-wrapper.ss" "stepper")
                           zodiac
                           utils
                           marks
                           annotate)]
        [break : (break)
               ((unit/sig (break) (import) (define break (lambda () #f))))])       
      (export (open debug-wrapper) (open break)))