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

(define mred:make-invokable-unit
  (lambda ()
    (unit/sig->unit
     (compound-unit/sig (import ())
       (link [mzlib : mzlib:core^ (mzlib:core@)]
	     [hooks : mzlib:print-convert-hooks^ (mzlib:print-convert-hooks@)]
	     [print-convert : mzlib:print-convert^
               (mzlib:print-convert@ (mzlib string@) (mzlib function@) hooks)]
	     [trigger : mzlib:trigger^ (mzlib:trigger@)]
	     [mred : mred^ (mred@ mzlib trigger (project : mred:application^))]
	     [interface : zodiac:interface^ (drscheme:zodiac-interface@ zodiac mred)]
	     [params : plt:parameters^ (drscheme:parameters@ mred)]
	     [zodiac : zodiac:system^ (zodiac:system@ interface params)]
	     [aries : plt:aries^ (plt:aries@ zodiac interface (spawn : plt:aries:predicates^))]
	     [setup : drscheme:setup^ (drscheme:setup@ mred mzlib)]
	     [tool : drscheme:tool^ 
		   (drscheme:tool@ mred mzlib print-convert zodiac (project : drscheme:export^) params)]
	     [spawn : drscheme:spawn^
		    (drscheme:spawn@ mred mzlib print-convert
				     params aries zodiac
				     interface)]
	     [edit : drscheme:edit^ (drscheme:edit@ mred print-convert spawn)]
	     [frame : drscheme:frame^
		    (drscheme:frame@ mred mzlib setup project tool)]
	     [project : drscheme:project^ (drscheme:project@ mred mzlib frame edit spawn)])
       (export (open mred)
	       (open mzlib)
	       (open print-convert)
	       (open setup)
	       (open tool)
	       (open spawn)
	       (open frame)
	       (open aries)
	       (open project)
	       (open zodiac))))))
