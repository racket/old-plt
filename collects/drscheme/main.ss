(unit/sig drscheme:main^
  (import [I : mred:application-imports^]
	  [mred : mred^]
	  [print-convert : mzlib:print-convert^]
	  [drscheme:compound-unit : drscheme:compound-unit^]
	  [drscheme:parameters : drscheme:parameters^])
  
  (mred:add-version-spec 'd 1)
  
  (print-convert:current-print-convert-hook
   (lambda (expr basic-convert sub-convert)
     (if (is-a? expr wx:image-snip%)
	 expr
	 (basic-convert expr))))

  '(define (make-basic)
    (make-object drscheme:compound-unit:frame% #f #f))

  (define (make-basic)
    (let ([frame (make-object (drscheme:parameters:current-frame%) #f #f)])
      (unless (mred:get-preference 'drscheme:repl-always-active)
	(let* ([interactions-edit (ivar frame interactions-edit)]
	       [definitions-edit (ivar frame interactions-edit)]
	       [filename (send definitions-edit get-filename)])
	  (when (null? filename)
	    (send interactions-edit reset-console)
	    (send interactions-edit enable-autoprompt)
	    (send interactions-edit insert-prompt)
	    (send (ivar frame show-menu) check (ivar frame interactions-id) #t)
	    (send frame update-shown)
	    (send (ivar frame interactions-canvas) set-focus))))
      (send frame show #t)))

  (let ([files-to-open (reverse (vector->list I:argv))])
    (if (null? files-to-open)
	(make-basic)
	(for-each mred:edit-file files-to-open))))