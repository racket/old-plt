  (unit/sig mred:icon^
    (import mred:wx^
	    [mred:constants : mred:constants^])
	    
    (mred:debug:printf 'invoke "mred:icon@")

    (define (load-icon % name type)
      (let ([p (build-path mred:constants:plt-home-directory
			   "icons"
			   name)]
	    [bitmap #f])
	(unless (file-exists? p)
	  (printf "WARNING: couldn't find ~a~n" p))
	(lambda ()
	  (if bitmap
	      bitmap
	      (begin (set! bitmap (make-object % p type))
		     bitmap)))))

    (define get-anchor-bitmap (load-icon wx:bitmap% "anchor.gif" wx:const-bitmap-type-gif))
    (define get-lock-bitmap (load-icon wx:bitmap% "lock.gif" wx:const-bitmap-type-gif))
    (define get-unlock-bitmap (load-icon wx:bitmap% "unlock.gif" wx:const-bitmap-type-gif))
    (define get-icon (load-icon wx:icon% "mred.xbm" wx:const-bitmap-type-xbm))
    (define get-autowrap-bitmap (load-icon wx:bitmap% "return.xbm" wx:const-bitmap-type-xbm))
    (define get-paren-highlight-bitmap (load-icon wx:bitmap% "paren.xbm" wx:const-bitmap-type-xbm))
    (define get-reset-console-bitmap (load-icon wx:bitmap% "reset.xbm" wx:const-bitmap-type-xbm))

    (define-values (get-gc-on-dc get-gc-width get-gc-height)
      (let* ([get-bitmap (load-icon wx:bitmap% "recycle.gif" wx:const-bitmap-type-gif)]
	     [bitmap #f]
	     [mdc #f]
	     [fetch
	      (lambda ()
		(unless mdc
		  (set! mdc (make-object wx:memory-dc%))
		  (set! bitmap (get-bitmap))))])
	(values (lambda () (fetch) mdc)
		(lambda () (fetch) (send bitmap get-width))
		(lambda () (fetch) (send bitmap get-height)))))

    (define get-gc-off-dc 
      (let ([mdc #f])
	(lambda ()
	  (if mdc
	      mdc
	      (begin
		(set! mdc (make-object wx:memory-dc%))
		(send mdc select-object
		      (make-object wx:bitmap%
			(get-gc-width)
			(get-gc-height)))
		(send mdc clear)
		mdc))))))
