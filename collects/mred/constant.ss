(unit/sig mred:constants^
  (import)

  ;; this cannot be here since the important names aren't defined yet!
  ;(mred:debug:printf 'invoke "mred:constant@")

  (define original-output-port
    (with-handlers ([void (lambda (x) (current-output-port))])
      (global-defined-value 'mred:original-output-port-backup)))

  (define original-input-port
    (with-handlers ([void (lambda (x) (current-input-port))])
      (global-defined-value 'mred:original-input-port-backup)))

  (define debug-on
    (with-handlers ([void (lambda (x) (box null))])
      (global-defined-value 'mred:constants:debug-on-backup)))
  (define debug-param
    (with-handlers ([void (lambda (x) (box null))])
      (global-defined-value 'mred:constants:debug-param-backup)))
  
  (define plt-home-directory
    (let ([x (global-defined-value 'plt:home-directory)])
      (if (string? x)
	  x
	  "/usr/local/lib/plt"))))
