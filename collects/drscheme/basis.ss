  (unit/sig drscheme:basis^
    (import [params : plt:parameters^]
	    [drscheme : drscheme:export^]
	    [mred : mred^]
	    [zodiac : zodiac:system^])
    
    (mred:debug:printf 'invoke "drscheme:basis@")

    (define library-unit #f)
    
    (define level->number
      (lambda (x)
	(case x
	  [(core) 0]
	  [(structured) 1]
	  [(side-effecting) 2]
	  [(advanced) 3]
	  [else (error 'level->number "unexpected level: ~a" x)])))
    (define level-symbols (list 'core 'structured 'side-effecting 'advanced))
    (define level-strings (list "Beginner" "Intermediate" "Advanced" "Quasi-R4RS"))
    
    (mred:set-preference-default 'drscheme:library-file #f (lambda (x)
							     (or (not x)
								 (string? x))))
    (mred:add-preference-callback 
     'drscheme:library-file
     (lambda (p v)
       (with-handlers
	   ((void (lambda (x)
		    (mred:message-box (exn-message x) "Invalid Library")
		    #f)))
	 (if v
	     (let ([new-unit (and (file-exists? v)
				  (load/cd v))])
	       (if (unit/sig? new-unit)
		   (set! library-unit new-unit)
		   (begin
		     (mred:message-box (format "Invalid Library: ~a" v) "Error")
		     #f)))
	     (set! library-unit #f)))))
    
    (define add-basis
      (let ([plt:userspace@ (reference-library-unit/sig "gusrspcr.ss" "gusrspce")])
	(lambda (n eventspace custodian)
	  (let* ([l@
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
		  (compound-unit/sig (import [drscheme : drscheme:export^])
				     (link [params : plt:parameters^ (params@)]
					   [userspace : plt:userspace^ (plt:userspace@
									params
									drscheme)]
					   [library : () (l@ userspace)])
				     (export (open userspace)))])
	    (parameterize ([current-namespace n]
			   [current-custodian custodian]
			   [wx:current-eventspace eventspace])
	      (invoke-open-unit/sig c@ #f (drscheme : drscheme:export^))))))))