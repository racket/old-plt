; stepper.ss 

(require-library "letplsrc.ss")

(unit/sig stepper^
  (import mzlib:function^
	  [z : zodiac:system^])
  
  (define (stepper-start string)
    (z:read (open-input-string string)))
  
  (define (stepper-step)
    null)
  
  (define (stepper-stop)
    null)
  
  )
  
	  
  
  