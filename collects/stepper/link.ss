; link.ss

(compound-unit/sig 
  (import (core : mzlib:core^)
          (framework : framework^)
          (print-convert : mzlib:print-convert^)
          (mred : mred^)
          (drscheme : drscheme:export^)
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
                      ((require-library-unit/sig "client-procs.ss")
                       zodiac)]
        [marks : stepper:marks^
               ((require-library-unit/sig "marks.ss" "stepper")
                zodiac
                client-procs
                (core function))]
        [utils : cogen-utils^ ((require-library-unit/sig "cogen-utilsr.ss" "cogen")
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
                   fake-stepper
                   shared
                   client-procs)]
        [debug-wrapper : plt:aries^
                          ((require-library-unit/sig "debug-wrapper.ss" "stepper")
                           drscheme
                           zodiac
                           mred
                           utils
                           marks
                           annotate)]
        [stepper-view-controller : (stepper-go)
                 ((require-library-unit/sig "stepper-view-controller.ss" "stepper")
                  core
                  error
                  zodiac
                  client-procs
                  pretty
                  mred
                  drscheme
                  print-convert
                  framework
                  shared)]
        [stepper-setup : (invoke-stepper)
                       ((require-library-unit/sig "stepper-setup.ss")
                        mred
                        framework
                        drscheme
                        stepper-view-controller)]       
      (export (open debug-wrapper) (open stepper-setup)))
