(unit/sig drscheme:main^
  (import [I : mred:application-imports^]
	  [mred : mred^]
	  [print-convert : mzlib:print-convert^]
	  [drscheme:compound-unit : drscheme:compound-unit^]
	  [drscheme:parameters : drscheme:parameters^])
  
  '(mred:add-version-spec 'd 3)
  
  (print-convert:current-print-convert-hook
   (lambda (expr basic-convert sub-convert)
     (if (is-a? expr wx:image-snip%)
	 expr
	 (basic-convert expr))))

  '(define (make-basic)
    (make-object drscheme:compound-unit:frame% #f #f))

  (define (make-basic)
    (send (make-object (drscheme:parameters:current-frame%) #f #f) show #t))

  (let ([files-to-open (reverse (vector->list I:argv))])
    (if (null? files-to-open)
	(make-basic)
	(for-each mred:edit-file files-to-open))))