(define mred:icon@
  (unit/sig mred:icon^
    (import [mred:debug : mred:debug^]
	    [mred:constants : mred:constants^])
	    
    (mred:debug:printf 'invoke "mred:icon@")

    ; Load the MrEd icon
    (define icon 
      (make-object wx:icon%
	(build-path mred:constants:system-source-directory "mred.xbm")
	wx:const-bitmap-type-xbm))

    ; Load autowrapping bitmap
    (define autowrap-bitmap 
      (make-object wx:bitmap%
	(build-path mred:constants:system-source-directory "return.xbm") 
	wx:const-bitmap-type-xbm))
    
    (define paren-highlight-bitmap 
      (make-object wx:bitmap%
	(build-path mred:constants:system-source-directory "paren.xbm") 
	wx:const-bitmap-type-bmp))    

    (define reset-console-bitmap 
      (make-object wx:bitmap%
	(build-path mred:constants:system-source-directory "reset.xbm") 
	wx:const-bitmap-type-xbm))))
