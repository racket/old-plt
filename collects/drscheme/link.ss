(define drscheme:tools
  (list (list "Stepper" (build-path "donkey" "donkey"))
	(list "Syntax Checker" (build-path "drscheme" "mrslatex"))
	(list "Analyzer" (build-path "drscheme" "spidstub"))

;;          this is the example tool.
;	    (list "Toy" (build-path "drscheme" "toy.ss"))

	))

(define drscheme:tool@
  (unit/sig drscheme:tool^
    (import mred^ mzlib:core^ mzlib:print-convert^ 
	    zodiac:system^ drscheme:export^ plt:parameters^)

    (define-struct tool (name file))

    (define tools (map (lambda (x) (apply make-tool x))
		       (global-defined-value 'drscheme:tools)))

    (define unit-with-signature->unit (global-defined-value 'unit-with-signature->unit))

    (define load/invoke-tool
      (lambda (tool)
	(file@:load-recent (build-path plt-home-directory (tool-file tool)))
	(invoke-unit/sig (global-defined-value 'tool@) 
			 mred^
			 mzlib:core^
			 mzlib:print-convert^
			 drscheme:export^
			 zodiac:system^
			 plt:parameters^)))))

(define drscheme@
  (compound-unit/sig
    (import [mred : mred^]
	    [mzlib : mzlib:core^]
	    [zodiac : zodiac:system^]
	    [interface : zodiac:interface^]
	    [print-convert : mzlib:print-convert^]
	    [params : plt:parameters^])
    (link [setup : drscheme:setup^ (drscheme:setup@ mred mzlib)]
	  [tool : drscheme:tool^ 
	    (drscheme:tool@ mred mzlib print-convert zodiac (project : drscheme:export^) params)]
	  [spawn : drscheme:spawn^ (drscheme:spawn@ mred mzlib print-convert params aries zodiac)]
	  [edit : drscheme:edit^ (drscheme:edit@ mred print-convert spawn)]
	  [frame : drscheme:frame^
	    (drscheme:frame@ mred mzlib setup project tool)]
	  [aries : drscheme:aries^ (drscheme:aries@ mred mzlib print-convert zodiac interface edit frame)]
	  [project : drscheme:project^ (drscheme:project@ mred mzlib aries edit spawn)])
    (export (open setup)
	    (open tool)
	    (open spawn)
	    (open frame)
	    (open aries)
	    (open project))))

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
	     [interface : zodiac:interface^ (drscheme:zodiac-interface@ zodiac mred)]
	     [params : plt:parameters^ (drscheme:parameters@ mred)]
	     [zodiac : zodiac:system^ (zodiac:system@ interface params)]
	     [drscheme : drscheme^
	       (drscheme@ mred mzlib zodiac interface print-convert params)])
       (export (open mred)
	       (open mzlib)
	       (open print-convert)
	       (open drscheme)
	       (open zodiac))))))
