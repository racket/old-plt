; link.ss

(compound-unit/sig 
  (import (core : mzlib:core^)
          (framework : framework^)
          (print-convert : mzlib:print-convert^)
          (mred : mred^)
          (drscheme : drscheme:export^)
          (zodiac : zodiac:system^)
          (error : zodiac:interface^))
  (link [pretty : mzlib:pretty-print^ ((require-library-unit/sig "prettyr.ss"))]
        [client-procs : stepper:client-procs^
                      ((require-library-unit/sig "client-procs.ss" "stepper")
                       zodiac)]
        [marks : stepper:marks^
               ((require-library-unit/sig "marks.ss" "stepper")
                zodiac
                error
                client-procs
                (core function))]
        [utils : stepper:cogen-utils^ 
               ((require-library-unit/sig "utils.ss" "stepper")
                zodiac
                error)]        
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
               ((require-library-unit/sig "break-graphical.ss" "stepper-graphical")
                core
                mred
                marks
                print-convert
                pretty
                zodiac
                error
                framework
                drscheme)]
        [mred-extensions : stepper:mred-extensions^
                 ((require-library-unit/sig "mred-extensions.ss" "stepper-graphical")
                  mred
                  framework
                  pretty
                  (shared : (highlight-placeholder)))]
        [stepper-view-controller : (stepper-go)
                 ((require-library-unit/sig "view-controller.ss" "stepper-graphical")
                  core
                  error
                  zodiac
                  client-procs
                  mred
                  drscheme
                  print-convert
                  framework
                  shared
                  utils
                  marks
                  mred-extensions)]
        [stepper-startup : ()
                         ((require-library-unit/sig "startup.ss" "stepper-graphical")
                          core
                          mred
                          framework
                          drscheme
                          error
                          stepper-view-controller)])       
      (export (open debug-wrapper) (open break)))
