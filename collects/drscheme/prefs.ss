(unit/sig drscheme:prefs^
  (import [mred : mred^])
  
  (mred:set-preference-default 'drscheme:keep-interactions-history #f)
  (mred:add-preference-panel
   "Interactions"
   (lambda (panel)
     (let* ([main (make-object mred:vertical-panel% panel)]
	    [right-align-in-main
	     (lambda (f)
	       (let ([hp (make-object mred:horizontal-panel% main)])
		 (send hp stretchable-in-y #f)
		 (begin0 (f hp)
			 (make-object mred:horizontal-panel% hp))))]
	    [interaction-history
	     (right-align-in-main
	      (lambda (p)
		(make-object mred:check-box% p
			     (lambda (checkbox evt)
			       (mred:set-preference 
				'drscheme:keep-interactions-history 
				(send evt checked?)))
			     "Keep execution history in interactions window")))])

       (send interaction-history set-value 
	     (mred:get-preference 'drscheme:keep-interactions-history))
       (make-object mred:vertical-panel% main)
       main))))
       