(unit/sig drscheme:prefs^
  (import [mred : mred-interfaces^]
	  [framework : framework^])

  (framework:preferences:set-default 'drscheme:execute-warning-once
				#f
				(lambda (x)
				  (or (eq? x #t)
				      (not x))))

  (framework:preferences:add-font-panel)
  (framework:scheme:add-preferences-panel)
  (framework:preferences:add-general-panel)

  (framework:preferences:add-panel
   "General II"
   (lambda (panel)
     (let* ([main (make-object mred:vertical-panel% panel)]
	    [right-align-in-main
	     (lambda (f)
	       (let ([hp (make-object mred:horizontal-panel% main)])
		 (send hp stretchable-height #f)
		 (begin0 (f hp)
			 (make-object mred:horizontal-panel% hp))))]
	    [make-check-box
	     (lambda (pref-sym string)
	       (right-align-in-main
		(lambda (p)
		  (let ([q (make-object mred:check-box%
			     string
			     p
			     (lambda (checkbox evt)
			       (framework:preferences:set 
				pref-sym 
				(send checkbox get-value))))])
		    (send q set-value (framework:preferences:get pref-sym))))))])
       (make-check-box 'drscheme:execute-warning-once
		       "Only warn once when executions and interactions are not synchronized")
       (make-object mred:vertical-panel% main)
       main))))
