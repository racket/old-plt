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


(define drscheme:tool@
  (unit/sig drscheme:tool^
    (import mred^ mzlib:core^ mzlib:print-convert^ zodiac:system^ drscheme:export^)

    (define-struct tool (name file))

    (define tools 
      (list (make-tool "Stepper" (build-path "donkey" "donkey.ss"))
	    (make-tool "Syntax Checker" (build-path "drscheme" "mrslatex.ss"))
	    (make-tool "Analyzer" (build-path "mrspidey" "mrspidey.ss"))

;;          this is the example tool.
;	    (make-tool "Toy" (build-path "drscheme" "toy.ss"))

	    ))


    (define unit-with-signature->unit (global-defined-value 'unit-with-signature->unit))

    (define load/invoke-tool
      (lambda (tool)
	(load/cd (tool-file tool))
	(invoke-unit/sig (global-defined-value 'tool@) 
			 mred^
			 mzlib:core^
			 mzlib:print-convert^
			 drscheme:export^
			 zodiac:system^)))))

(define drscheme@
  (compound-unit/sig
    (import [mred : mred^]
	    [mzlib : mzlib:core^]
	    [zodiac : zodiac:system^]
	    [interface : zodiac:interface^]
	    [print-convert : mzlib:print-convert^]
	    [params : mzlib:parameters^])
    (link [setup : drscheme:setup^ (drscheme:setup@ mred mzlib)]
	  [tool : drscheme:tool^ 
	    (drscheme:tool@ mred mzlib print-convert zodiac (project : drscheme:export^))]
	  [spawn : drscheme:spawn^ (drscheme:spawn@ mred mzlib print-convert params aries zodiac)]
	  [edit : drscheme:edit^ (drscheme:edit@ mred print-convert spawn)]
	  [frame : drscheme:frame^
	    (drscheme:frame@ mred mzlib setup project tool)]
	  [aries : drscheme:aries^ (drscheme:aries@ mred mzlib zodiac interface edit frame)]
	  [project : drscheme:project^ (drscheme:project@ mred mzlib aries edit spawn)])
    (export (open project))))

(define drscheme:userspace@
  (unit/sig->unit
   (compound-unit/sig
    (import)
    (link [function : mzlib:function^ (mzlib:function@)]
	  [compat : mzlib:compat^ (mzlib:compat@ function)]
	  [string : mzlib:string^ (mzlib:string@)]
	  [rice : ricedefs^ (ricedefs@)]
	  [graphics : graphics^ (graphics@)])
    (export (open function)
	    (open compat)
	    (open string)
	    (open rice)
	    (open graphics)))))

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
	       (drscheme@ mred mzlib zodiac interface print-convert params)])
       (export (open mred)
	       (open mzlib)
	       (open print-convert)
	       (open (drscheme : drscheme:export^))
	       (open zodiac))))))
