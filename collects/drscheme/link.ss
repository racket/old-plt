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
	    [mred : mred^])

    (rename (user-defined? defined?)
	    (user-macro? macro?)
	    (user-syntax? syntax?)
	    (user-id-macro? id-macro?)
	    (user-expansion-time-value? expansion-time-value?)
	    (user-global-defined-value global-defined-value))

;    (define unit-with-signature->unit (global-defined-value 'unit/sig->unit))

    (define library-unit #f)

    (define level->number
      (lambda (x)
	(case x
	  [(core) 0]
	  [(structured) 1]
	  [(side-effects) 2]
	  [(advanced) 3])))
    (define level-symbols (list 'core 'structured 'side-effects 'advanced))
    (define level-strings (list "Beginner" "Intermediate" "Advanced" "R4RS"))

    (mred:set-preference-default 'drscheme:library-file #f)
    (mred:add-preference-callback 
     'drscheme:library-file
     (lambda (p v)
       (if v
	   (let ([new-unit (and (file-exists? v)
				(with-handlers
				 ((void (lambda (x)
					  (wx:message-box (exn-message x) "Invalid Library")
					  (raise x))))
				 (load/cd v)))])
	     (if ((global-defined-value 'unit/sig?) new-unit)
		 (set! library-unit new-unit)
		 (begin
		   (wx:message-box (format "Invalid Library: ~a" v) "ERROR")
		   #f)))
	   (set! library-unit #f))))

    (define user-defined? 'user-defined?)
    (define user-macro? 'user-macro?)
    (define user-syntax? 'user-syntax?)
    (define user-id-macro? 'user-id-macro?)
    (define user-expansion-time-value? 'expansion-time-value?)
    (define user-global-defined-value 'user-global-defined-value)
    
    (define build-basis
      (lambda (eval)
	(let* ([plt:userspace@ (global-defined-value 'plt:userspace@)]
	       [l@
		(unit/sig ()
		  (import plt:userspace^)
		  (when library-unit
		    (invoke-open-unit/sig library-unit #f plt:userspace^)))]
	       [params@ (unit/sig plt:parameters^
			  (import)
			  (define case-sensitive? params:case-sensitive?) 
			  (define allow-set!-on-undefined? params:allow-set!-on-undefined?)
			  (define allow-improper-lists? params:allow-improper-lists?)
			  (define unmatched-cond/case-is-error? params:unmatched-cond/case-is-error?) 
			  (define check-syntax-level params:check-syntax-level))]
	       [c@
		(compound-unit/sig
		  (import)
		  (link [params : plt:parameters^ (params@)]
			[userspace : plt:userspace^ (plt:userspace@ params)]
			[library : () (l@ userspace)])
		  (export (open userspace)))])
	  ((global-defined-value 'install-unit-with-signature) eval)
	  (let-values
	      ([(d? m? s? id? etv? gdv)
		(eval
		 `(#%begin
		   (#%invoke-open-unit-with-signature ,c@ #f)
		   (#%exit-handler (#%lambda (arg) (,mred:exit)))
		   
		   (#%error-display-handler
		    (#%lambda (msg)
			      (,display msg)
			      (,newline)
			      (wx:message-box
			       (string-append "Internal Error: " msg)
			       "Internal Error!")))
		   (#%print-struct #t)
		   (#%error-print-width 250)
		   (#%break-enabled #t)
		   (#%values defined? macro? syntax?
			     id-macro? expansion-time-value?
			     global-defined-value)))])
	    (set! user-defined? d?)
	    (set! user-macro? m?)
	    (set! user-syntax? s?)
	    (set! user-id-macro? id?)
	    (set! user-expansion-time-value? etv?)
	    (set! user-global-defined-value gdv)))))))

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
	     [basis : drscheme:basis^ (drscheme:basis@ params mred)]
	     [params : plt:parameters^ (drscheme:parameters@ mred basis)]
	     [zodiac : zodiac:system^ (zodiac:system@ interface params)]
	     [aries : plt:aries^ (plt:aries@ zodiac interface (basis : plt:aries:predicates^))]
	     [setup : drscheme:setup^ (drscheme:setup@ mred mzlib)]
	     [tool : drscheme:tool^ 
		   (drscheme:tool@ mred mzlib print-convert zodiac (project : drscheme:export^) params)]
	     [spawn : drscheme:spawn^
		    (drscheme:spawn@ mred mzlib print-convert
				     params aries zodiac
				     interface basis)]
	     [edit : drscheme:edit^ (drscheme:edit@ mred print-convert spawn)]
	     [frame : drscheme:frame^
		    (drscheme:frame@ mred mzlib basis setup project tool)]
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
