  (unit/sig drscheme:basis^
    (import [wx : wx^]
	    [drscheme:init : drscheme:init^]
	    [drscheme:language : drscheme:language^]
	    [mred : mred^]
	    [zodiac : drscheme:zodiac^])
    
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
      (lambda (n eventspace custodian)
	(let* ([l@
		(unit/sig ()
		  (import plt:userspace^)
		  (when library-unit
		    (invoke-open-unit/sig library-unit #f plt:userspace^)))]
	       [c@
		(compound-unit/sig (import [drscheme:init : drscheme:init^]
					   [params : plt:userspace:params^])
		  (link [userspace : plt:userspace^ 
				   ((reference-library-unit/sig "gusrspcr.ss" "gusrspce")
				    drscheme:init
				    params)]
			[library : () (l@ userspace)])
		  (export (open userspace)))])
	  (parameterize ([current-namespace n]
			 [current-custodian custodian]
			 [wx:current-eventspace eventspace])
	    (let ([allow-improper-lists zodiac:allow-improper-lists]
		  [eq?-only-compares-symbols drscheme:language:eq?-only-compares-symbols])
	      (load (build-path (collection-path "system") "debug.ss"))
	      (invoke-open-unit/sig c@ #f 
				    (drscheme:init : drscheme:init^)
				    plt:userspace:params^)))))))