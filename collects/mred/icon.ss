  (unit/sig mred:icon^
    (import [mred:constants : mred:constants^])
	    
    (mred:debug:printf 'invoke "mred:icon@")

    (define (load-icon % name)
      (let ([p (build-path mred:constants:plt-home-directory
			   "icons"
			   (string-append name ".xbm"))])
	(unless (file-exists? p)
	  (printf "WARNING: couldn't find ~a~n" p))
	(make-object % p wx:const-bitmap-type-bmp)))

    (define icon (load-icon wx:icon% "mred"))
    (define autowrap-bitmap (load-icon wx:bitmap% "return"))
    (define paren-highlight-bitmap (load-icon wx:bitmap% "paren"))
    (define reset-console-bitmap (load-icon wx:bitmap% "reset")))
