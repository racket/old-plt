(unit/sig drscheme:prefs^
  (import [mred : mred^])
  
  (mred:set-preference-default 'drscheme:keep-interactions-history #f
			       (lambda (x)
				 (or (not x)
				     (eq? x #t))))

    (mred:set-preference-default 'drscheme:execute-warning-once
				 #f
				 (lambda (x)
				   (or (eq? x #t)
				       (not x))))

  (mred:add-preference-panel
   "General II"
   (lambda (panel)
     (let* ([main (make-object mred:vertical-panel% panel)]
	    [right-align-in-main
	     (lambda (f)
	       (let ([hp (make-object mred:horizontal-panel% main)])
		 (send hp stretchable-in-y #f)
		 (begin0 (f hp)
			 (make-object mred:horizontal-panel% hp))))]
	    [make-check-box
	     (lambda (pref-sym string)
	       (right-align-in-main
		(lambda (p)
		  (let ([q (make-object mred:check-box% p
					(lambda (checkbox evt)
					  (mred:set-preference 
					   pref-sym 
					   (send evt checked?)))
					string)])
		    (send q set-value (mred:get-preference pref-sym))))))])
       (make-check-box 'drscheme:keep-interactions-history
		       "Keep execution history in interactions window")
       (make-check-box 'drscheme:execute-warning-once
		       "Only warn once when executions and interactions are not synchronized")
       (make-check-box 'drscheme:open-all-files-in-scheme-mode
		       "Open all files as Scheme programs")
       (make-check-box 'drscheme:repl-always-active
		       "Read-eval-print loop always active")
       (make-object mred:vertical-panel% main)
       main))))
       