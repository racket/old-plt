; stepper-instance

(compound-unit/sig 
  (import (model-input : stepper:model-input^)
          (core : mzlib:core^)
          (error : stepper:error^)
          (print-convert : mzlib:print-convert^)
          (drscheme : drscheme:export^)
          (zodiac : zodiac:system^)
          (zcp : stepper:zodiac-client-procs^)
          (shared : stepper:shared^)
          (mred : mred^))
  (link [utils : cogen-utils^ ((require-library-unit/sig "cogen-utilsr.ss" "cogen")
                               zodiac
                               (error : (internal-error static-error)))] 
        [marks : stepper:marks^
               ((require-library-unit/sig "marks.ss" "stepper")
                zodiac
                zcp
                (core function))]
        [annotate : stepper:annotate^
                  ((require-library-unit/sig "annotater.ss" "stepper")
                   zodiac
                   (core function)
                   error
                   utils
                   stepper
                   shared
                   zcp)]
        [reconstruct : stepper:reconstruct^ 
                     ((require-library-unit/sig "reconstructr.ss" "stepper")
                      zodiac
                      (core function)
                      error
                      utils
                      (drscheme basis)
                      marks
                      stepper
                      shared)]
        [stepper : stepper:model^
                 ((require-library-unit/sig "stepper-model.ss" "stepper")
                  model-input
                  mred
                  zodiac
                  drscheme
                  print-convert
                  error
                  annotate
                  reconstruct
                  shared)])       
      (export))