  (unit/sig mred:icon^
    (import mred:wx^
	    [mred:constants : mred:constants^])
	    
    (mred:debug:printf 'invoke "mred:icon@")

    (define (load-icon % name type)
      (let ([p (build-path mred:constants:plt-home-directory
			   "icons"
			   name)])
	(unless (file-exists? p)
	  (printf "WARNING: couldn't find ~a~n" p))
	(make-object % p type)))


    (define lock-bitmap (load-icon wx:bitmap% "lock.gif" wx:const-bitmap-type-gif))
    (define unlock-bitmap (load-icon wx:bitmap% "unlock.gif" wx:const-bitmap-type-gif))
    (define icon (load-icon wx:icon% "mred.xbm" wx:const-bitmap-type-bmp))
    (define autowrap-bitmap (load-icon wx:bitmap% "return.xbm" wx:const-bitmap-type-bmp))
    (define paren-highlight-bitmap (load-icon wx:bitmap% "paren.xbm" wx:const-bitmap-type-bmp))
    (define reset-console-bitmap (load-icon wx:bitmap% "reset.xbm" wx:const-bitmap-type-bmp)))
