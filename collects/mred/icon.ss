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

    (define anchor-bitmap (load-icon wx:bitmap% "anchor.gif" wx:const-bitmap-type-gif))
    (define lock-bitmap (load-icon wx:bitmap% "lock.gif" wx:const-bitmap-type-gif))
    (define unlock-bitmap (load-icon wx:bitmap% "unlock.gif" wx:const-bitmap-type-gif))
    (define icon (load-icon wx:icon% "mred.xbm" wx:const-bitmap-type-xbm))
    (define autowrap-bitmap (load-icon wx:bitmap% "return.xbm" wx:const-bitmap-type-xbm))
    (define paren-highlight-bitmap (load-icon wx:bitmap% "paren.xbm" wx:const-bitmap-type-xbm))
    (define reset-console-bitmap (load-icon wx:bitmap% "reset.xbm" wx:const-bitmap-type-xbm))

    (define-values (gc-on-dc gc-width gc-height)
      (let ([bitmap (load-icon wx:bitmap% "recycle.gif" wx:const-bitmap-type-gif)]
	    [mdc (make-object wx:memory-dc%)])
	(send mdc select-object bitmap)
	(values mdc
		(send bitmap get-width)
		(send bitmap get-height))))

    (define gc-off-dc 
      (let ([bitmap (make-object wx:bitmap% gc-width gc-height)]
	    [mdc (make-object wx:memory-dc%)])
	(send mdc select-object bitmap)
	(send mdc clear)
	mdc)))
