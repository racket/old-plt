(unit/sig drscheme:get/extend^
  (import [mred : mred-interfaces^]
	  [drscheme:unit : drscheme:unit^]
	  [drscheme:frame : drscheme:frame^]
	  [drscheme:rep : drscheme:rep^]
	  [mzlib : mzlib:core^])


  (define make-extender
    (lambda (base%)
      (let ([extensions (lambda (x) x)]
	    [built-yet? #f]
	    [built #f]
	    [verify
	     (lambda (f)
	       (lambda (%)
		 (let ([new% (f %)])
		   (if (and (class? new%)
			    (subclass? new% %))
		       new%
		       (error 'extend-% "expected output of extension to create a subclass of its input, got: ~a"
			      new%)))))])
	(values
	 (lambda (extension)
	   (when built-yet?
	     (error 'extender "cannot build a new extension of ~a after initialization"
		    base%))
	   (set! extensions (mzlib:function:compose 
			     (verify extension)
			     extensions)))
	 (lambda ()
	   (unless built-yet?
	     (set! built-yet? #t)
	     (set! built (extensions base%)))
	   built)))))

  (define-values (extend-interactions-canvas% get-interactions-canvas%)
    (make-extender drscheme:unit:interactions-canvas%))

  (define-values (extend-definitions-canvas% get-definitions-canvas%)
    (make-extender drscheme:unit:definitions-canvas%))  

  (define-values (extend-unit-frame% get-unit-frame%)
    (make-extender drscheme:unit:frame%))

  (define-values (extend-interactions-text% get-interactions-text%)
    (make-extender drscheme:rep:text%))
	     
  (define-values (extend-definitions-text% get-definitions-text%)
    (make-extender drscheme:unit:definitions-text%)))