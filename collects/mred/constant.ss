(unit/sig mred:constants^
  (import)

  ;; this cannot be here since the important names aren't defined yet!
  ;(mred:debug:printf 'invoke "mred:constant@")

  (define debug-on (begin-elaboration-time mred:constants:debug-on-backup))
  (define debug-param (begin-elaboration-time mred:constants:debug-param-backup))
  
  (define plt-home-directory
    (let ([x (global-defined-value 'plt:home-directory)])
      (if (string? x)
	  x
	  "/usr/local/lib/plt"))))
