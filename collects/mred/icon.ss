  (unit/sig mred:icon^
    (import mred:wx^
	    [mred:constants : mred:constants^])
	    
    (mred:debug:printf 'invoke "mred:icon@")

    (define icon-path 
      (with-handlers ([void (lambda (x) (collection-path "system"))]) 
	  (collection-path "icons")))

    (define (load-icon % name type)
      (let ([p (build-path icon-path name)]
	    [bitmap #f])
	(unless (file-exists? p)
	  (printf "WARNING: couldn't find ~a~n" p))
	(lambda ()
	  (if bitmap
	      bitmap
	      (begin (set! bitmap (make-object % p type))
		     bitmap)))))
    
    (define (load-bitmap/mdc % name type)
      (let* ([p (build-path icon-path name)]
	     [bitmap #f]
	     [memory-dc #f]
	     [force
	      (lambda ()
		(set! bitmap (make-object % p type))
		(set! memory-dc (make-object wx:memory-dc%))
		(when (send bitmap ok?)
		  (send memory-dc select-object bitmap)))])
	(unless (file-exists? p)
	  (printf "WARNING: couldn't find ~a~n" p))
	(values 
	 (lambda ()
	   (or bitmap
	       (begin (force)
		      bitmap)))
	 (lambda ()
	   (or memory-dc
	       (begin (force)
		      memory-dc))))))

    (define-values (get-anchor-bitmap get-anchor-mdc)
      (load-bitmap/mdc wx:bitmap% "anchor.gif" wx:const-bitmap-type-gif))
    (define-values (get-lock-bitmap get-lock-mdc)
      (load-bitmap/mdc wx:bitmap% "lock.gif" wx:const-bitmap-type-gif))
    (define-values (get-unlock-bitmap get-unlock-mdc)
      (load-bitmap/mdc wx:bitmap% "unlock.gif" wx:const-bitmap-type-gif))

    (define get-autowrap-bitmap (load-icon wx:bitmap% "return.xbm" wx:const-bitmap-type-xbm))
    (define get-paren-highlight-bitmap (load-icon wx:bitmap% "paren.xbm" wx:const-bitmap-type-xbm))
    (define get-reset-console-bitmap (load-icon wx:bitmap% "reset.xbm" wx:const-bitmap-type-xbm))

    (define get-icon 
      (let ([icon #f]
	    [p (build-path icon-path "mred.xbm")])
	(unless (file-exists? p)
	  (printf "WARNING: couldn't find ~a~n" p))
	(lambda ()
	  (or icon
	      (begin
		(set! icon (make-object wx:icon% p
					wx:const-bitmap-type-xbm))
		icon)))))

    (define-values (get-gc-on-dc get-gc-width get-gc-height)
      (let* ([get-bitmap (load-icon wx:bitmap% 
				    "recycle.gif"
				    wx:const-bitmap-type-gif)]
	     [bitmap #f]
	     [mdc #f]
	     [fetch
	      (lambda ()
		(unless mdc
		  (set! mdc (make-object wx:memory-dc%))
		  (set! bitmap (get-bitmap))
		  (send mdc select-object bitmap)))])
	(values (lambda () (fetch) mdc)
		(lambda () (fetch) (if (send bitmap ok?)
				       (send bitmap get-width)
				       10))
		(lambda () (fetch) (if (send bitmap ok?)
				       (send bitmap get-height)
				       10)))))

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
