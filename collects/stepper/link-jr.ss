(module link-jr mzscheme
  (require (lib "unitsig.ss")
	   "sig.ss"
	   "client-procs.ss"
	   "marks.ss"
	   "utils.ss"
	   "shared.ss"
	   "fake-model.ss"
	   "fake-break.ss"
	   "annotater.ss"
	   "debug-wrapper.ss")

  (provide link-jr@)

  (define link-jr@
    (compound-unit/sig 
      (import (zodiac : zodiac^))
      (link [client-procs : stepper:client-procs^ (client-procs@ zodiac)]
	    [marks : stepper:marks^ (marks@ zodiac client-procs)]
	    [utils : stepper:cogen-utils^ (utils@ zodiac)]        
	    [shared : stepper:shared^ (shared@ zodiac client-procs)]
	    [fake-stepper : stepper:model^ (fake-model@)]
	    [fake-break : (break) (fake-break@)]
	    [annotate : stepper:annotate^
		      (annotater@ zodiac utils marks fake-stepper shared client-procs)]
	    [debug-wrapper : plt:aries-no-break^
			   (debug-wrapper@ zodiac utils marks annotate)])       
      (export (open debug-wrapper) (open fake-break)))))
