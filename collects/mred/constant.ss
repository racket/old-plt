(unit/sig mred:constants^
  (import)

  (mred:debug:printf 'invoke "mred:constant@")

  (define plt-home-directory
    (let ([x (global-defined-value 'plt:home-directory)])
      (if (string? x)
	  x
	  "/usr/local/lib/plt"))))
