(define mred:icon@
  (unit/sig mred:icon^
    (import ([unit mred:debug : mred:debug^]))
	    
    (mred:debug:printf 'invoke "mred:icon@")

    ; Load the MrEd icon
    (define icon 
      (let ([dir (global-defined-value 'mred:system-source-directory)])
	(if (eq? wx:platform 'windows)
	    (make-object wx:icon% (build-path dir "mred.bmp") 
			 wx:const-bitmap-type-bmp)
	    (make-object wx:icon% (build-path dir "mred.xbm")
			 wx:const-bitmap-type-xbm))))

    ; Load autowrapping bitmap
    (define autowrap-bitmap 
      (let ([dir (global-defined-value 'mred:system-source-directory)])
	(if (eq? wx:platform 'windows)
	    (make-object wx:bitmap% (build-path dir "return.bmp") 
			 wx:const-bitmap-type-bmp)
	    (make-object wx:bitmap% (build-path dir "return.xbm") 
			 wx:const-bitmap-type-xbm))))
    
    (define paren-highlight-bitmap 
      (let ([dir (global-defined-value 'mred:system-source-directory)])
	(if (eq? wx:platform 'windows)
	    (make-object wx:bitmap% (build-path dir "paren.bmp") 
			 wx:const-bitmap-type-bmp)
	    (make-object wx:bitmap% (build-path dir "paren.xbm") 
			 wx:const-bitmap-type-xbm))))))
