; stepper-instance

(compound-unit/sig 
  (import (model-input : stepper:model-input^)
          (core : mzlib:core^)
          (error : stepper:error^)
          (print-convert : mzlib:print-convert^)
          (drscheme : drscheme:export^)
          (zodiac : zodiac:system^)
          (zcp : stepper:client-procs^)
          (shared : stepper:shared^)
          (mred : mred^)
          (utils : stepper:cogen-utils^)
          (marks : stepper:marks^)
          (annotate : stepper:annotate^))
  (link [reconstruct : stepper:reconstruct^ 
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
