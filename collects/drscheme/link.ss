(define drscheme:baby-parameters@
  (unit/sig mzlib:parameters^
    (import)
    (define allow-one-armed-if? #f)
    (define case-sensitive? #t)
    (define allow-set!-on-undefined? #f)
    (define allow-internal-defines? #f)
    (define allow-improper-lists? #f)
    (define allow-improper-lists-in-lambda? #f)
    (define unmatched-cond/case-is-error? #t)))

(define drscheme:adult-parameters@
  (unit/sig mzlib:parameters^
    (import)
    (define allow-one-armed-if? #t)
    (define case-sensitive? #t)
    (define allow-set!-on-undefined? #f)
    (define allow-internal-defines? #f)
    (define allow-improper-lists? #f)
    (define allow-improper-lists-in-lambda? #t)
    (define unmatched-cond/case-is-error? #t)))

(define drscheme:adult-parameters@
  (unit/sig mzlib:parameters^
    (import)
    (define allow-one-armed-if? #t)
    (define case-sensitive? #t)
    (define allow-set!-on-undefined? #f)
    (define allow-internal-defines? #f)
    (define allow-improper-lists? #f)
    (define allow-improper-lists-in-lambda? #t)
    (define unmatched-cond/case-is-error? #t)))

(define drscheme:our-parameters@
  (unit/sig mzlib:parameters^
    (import)
    (define allow-one-armed-if? #t)
    (define case-sensitive? #t)
    (define allow-set!-on-undefined? #f)
    (define allow-internal-defines? #t)
    (define allow-improper-lists? #t)
    (define allow-improper-lists-in-lambda? #t)
    (define unmatched-cond/case-is-error? #t)))

(define drscheme@
  (compound-unit/sig
    (import [mred : mred^]
	    [mzlib : mzlib:core^]
	    [zodiac : zodiac:system^]
	    [interface : zodiac:interface^]
	    [print-convert : mzlib:print-convert^])
    (link [setup : drscheme:setup^ (drscheme:setup@ mred mzlib)]
	  [load/link-tool : drscheme:load/link-tool^ (drscheme:load/link-tool@)]
	  [spawn : drscheme:spawn^ (drscheme:spawn@ mred mzlib aries zodiac)]
	  [edit : drscheme:edit^ (drscheme:edit@ mred print-convert spawn)]
	  [frame : drscheme:frame^
	    (drscheme:frame@ mred mzlib spawn setup project load/link-tool)]
	  [aries : drscheme:aries^ (drscheme:aries@ mred mzlib zodiac interface edit frame)]
	  [project : drscheme:project^ (drscheme:project@ mred mzlib aries edit spawn)])
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
	     [interface : zodiac:interface^ (drscheme:zodiac-interface@ zodiac)]
	     [params : mzlib:parameters^ (drscheme:baby-parameters@)]
	     [zodiac : zodiac:system^ (zodiac:system@ interface params)]
	     [drscheme : drscheme^
	       (drscheme@ mred mzlib zodiac interface print-convert)])
       (export (open mred)
	       (open mzlib)
	       (open print-convert)
	       (open (drscheme : drscheme:export^))
	       (open zodiac))))))