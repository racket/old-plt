(unit/sig drscheme:parameters^
  (import [mred : mred^]
	  [drscheme:unit : drscheme:unit^]
	  [drscheme:frame : drscheme:frame^]
	  [drscheme:rep : drscheme:rep^])

  (define make-parameter
    (lambda (x g)
      (case-lambda
       [() x]
       [(n) (if (g n)
		(set! x n)
		(error 'parameter "value: ~a failed guard" n))])))

  (define current-interactions-canvas%
    (make-parameter drscheme:unit:interactions-canvas%
		    (lambda (x)
		      (if (subclass? x wx:media-canvas%)
			  x
			  (error 'current-interactions-canvas%
				 "expected a subclass of wx:media-canvas%, got: ~a"
				 x)))))

  (define current-definitions-canvas%
    (make-parameter drscheme:unit:definitions-canvas%
		    (lambda (x)
		      (if (subclass? x wx:media-canvas%)
			  x
			  (error 'current-definitions-canvas%
				 "expected a subclass of wx:media-canvas%, got: ~a"
				 x)))))  

  (define current-frame%
    (make-parameter 
     drscheme:unit:frame%
     (lambda (x)
       (if (subclass? x wx:frame%)
	   x
	   (error 'current-frame%
		  "expected a subclass of wx:frame%, got: ~a"
		  x)))))

  (define current-interactions-edit%
    (make-parameter 
     drscheme:rep:edit%
     (lambda (x)
       (if (subclass? x wx:media-edit%)
	   x
	   (error 'current-interactions-edit% 
		  "expected a subclass of wx:edit%, got: ~a"
		  x)))))

  (define current-definitions-edit%
    (make-parameter 
     drscheme:unit:definitions-edit%
     (lambda (x)
       (if (subclass? x wx:media-edit%)
	   x
	   (error 'current-definitions-edit% 
		  "expected a subclass of wx:edit%, got: ~a"
		  x))))))