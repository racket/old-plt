(define drscheme:toplevel-tools
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

    (debug:printf 'invoke "drscheme:tool@")

    (define-struct tool (name file))

    (define tools (map (lambda (x) (apply make-tool x))
		       (global-defined-value 'drscheme:toplevel-tools)))

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
	  
(define drscheme:basis@
  (unit/sig drscheme:basis^
    (import [params : plt:parameters^]
            [mred : mred^]
            [zodiac : zodiac:system^])

    (define library-unit #f)

    (define level->number
      (lambda (x)
        (case x
          [(core) 0]
          [(structured) 1]
          [(side-effects) 2]
          [(advanced) 3])))
    (define level-symbols (list 'core 'structured 'side-effects 'advanced))
    (define level-strings (list "Beginner" "Intermediate" "Advanced" "Quasi-R4RS"))

    (mred:set-preference-default 'drscheme:library-file #f)
    (mred:add-preference-callback 
     'drscheme:library-file
     (lambda (p v)
       (with-handlers
        ((void (lambda (x)
                 (wx:message-box (exn-message x) "Invalid Library")
                 #f)))
        (if v
            (let ([new-unit (and (file-exists? v)
                                 (load/cd v))])
              (if ((global-defined-value 'unit/sig?) new-unit)
                  (set! library-unit new-unit)
                  (begin
                    (wx:message-box (format "Invalid Library: ~a" v) "ERROR")
                    #f)))
            (set! library-unit #f)))))

    (define add-basis
      (lambda (n)
        (let* ([plt:userspace@ (global-defined-value 'plt:userspace@)]
               [l@
                (unit/sig ()
                  (import plt:userspace^)
                  (when library-unit
                    (invoke-open-unit/sig library-unit #f plt:userspace^)))]
               [params@ (unit/sig plt:parameters^
                          (import)
                          (define case-sensitive? params:case-sensitive?) 
                          (define allow-set!-on-undefined? 
			    params:allow-set!-on-undefined?)
                          (define allow-improper-lists? 
			    params:allow-improper-lists?)
                          (define unmatched-cond/case-is-error?
			    params:unmatched-cond/case-is-error?) 
                          (define check-syntax-level 
			    params:check-syntax-level))]
               [c@
                (compound-unit/sig
                  (import)
                  (link [params : plt:parameters^ (params@)]
                        [userspace : plt:userspace^ (plt:userspace@ params)]
                        [library : () (l@ userspace)])
                  (export (open userspace)))])
	  (install-unit-with-signature n)
	  (parameterize ([current-namespace n])
	    (invoke-open-unit/sig c@ #f)))))))

(define mred:make-invokable-unit
  (lambda ()
    (unit/sig->unit
     (compound-unit/sig (import ())
       (link [mzlib : mzlib:core^ (mzlib:core@)]
	     [hooks : mzlib:print-convert-hooks^ (mzlib:print-convert-hooks@)]
	     [print-convert : mzlib:print-convert^
               (mzlib:print-convert@ (mzlib string@) (mzlib function@) hooks)]
	     [trigger : mzlib:trigger^ (mzlib:trigger@)]
	     [mred : mred^ (mred@ mzlib trigger (app : mred:application^))]
	     [interface : zodiac:interface^
			(drscheme:zodiac-interface@ zodiac mred)]
	     [basis : drscheme:basis^ (drscheme:basis@ params mred zodiac)]
	     [params : plt:parameters^ (drscheme:parameters@ mred basis)]
	     [zodiac : zodiac:system^ (zodiac:system@ interface params)]
	     [aries : plt:aries^ (plt:aries@ zodiac interface)]
	     [setup : drscheme:setup^ (drscheme:setup@ mred mzlib)]
	     [tool : drscheme:tool^ 
		   (drscheme:tool@ mred mzlib print-convert zodiac
				 (app : drscheme:export^) params)]
	     [rep : drscheme:rep^
		    (drscheme:rep@ mred mzlib print-convert
				     params aries zodiac
				     interface app basis)]
	     [frame : drscheme:frame^
		    (drscheme:frame@ mred mzlib rep basis
				   setup tool compound-unit)]
	     [unit : drscheme:unit^
		    (drscheme:unit@ mred mzlib basis 
				  setup compound-unit tool frame)]
	     [compound-unit : drscheme:compound-unit^
		    (drscheme:compound-unit@ mred mzlib unit frame)]
	     [app : drscheme:app^ (drscheme:application@ mred mzlib)])
       (export (unit mred)
	       (unit app mred)
	       (open mzlib)
	       (open print-convert)
	       (unit setup drscheme:setup)
	       (unit tool drscheme:tool)
	       (unit rep drscheme:rep)
	       (unit unit drscheme:unit)
	       (unit aries drscheme:aries)
	       (unit compound-unit drscheme:compound-unit)
	       (unit zodiac zodiac))))))
