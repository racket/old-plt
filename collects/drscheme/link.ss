(define-signature drscheme:parameters^
  (case-sensitive?
   allow-set!-on-undefined?
   allow-internal-defines?
   allow-improper-lists?
   unmatched-cond/case-is-error?))

(define drscheme:comp210-parameters@
  (unit/sig drscheme:parameters^
    (import)
    (define case-sensitive? #t)
    (define allow-set!-on-undefined? #f)
    (define allow-internal-defines? #f)
    (define allow-improper-lists? #f)
    (define unmatched-cond/case-is-error? #t)))

(define drscheme@
  (compound-unit/sig
    (import [mred : mred^]
	    [mzlib : mzlib:core^]
	    [zodiac : zodiac:system^]
	    [print-convert : mzlib:print-convert^])
    (link [setup : drscheme:setup^ (drscheme:setup@ mred mzlib)]
	  [load/link-tool : drscheme:load/link-tool^ (drscheme:load/link-tool@)]
	  [spawn : drscheme:spawn^
	   (drscheme:spawn@ mred mzlib zodiac)]
	  [edit : drscheme:edit^ (drscheme:edit@ mred print-convert spawn)]
	  [frame : drscheme:frame^
	    (drscheme:frame@ mred mzlib spawn setup project load/link-tool)]
	  [aries : drscheme:aries^ (drscheme:aries@ mred mzlib zodiac edit frame)]
	  [project : drscheme:project^ (drscheme:project@ mred mzlib aries spawn)])
    (export (open project))))

(define mred:make-invokable-unit
  (lambda ()
    (unit/sig->unit
     (compound-unit/sig (import ())
       (link [mzlib : mzlib:core^ (mzlib:core@)]
	     [hooks : mzlib:print-convert-hooks^ (mzlib:print-convert-hooks@)]
	     [print-convert : mzlib:print-convert^
               (mzlib:print-convert@ (mzlib string@) (mzlib function@) hooks)]
	     [trigger : mzlib:trigger^ (mzlib:trigger@)]
	     [mred : mred^ 
	       (mred@ mzlib trigger (drscheme : mred:application^))]
	     [zodiac : zodiac:system^ (zodiac:system@)]
	     [drscheme : drscheme^
	       (drscheme@ mred mzlib zodiac print-convert)])
       (export (open mred)
	       (open mzlib)
	       (open print-convert)
	       (open (drscheme : drscheme:export^))
	       (open zodiac))))))