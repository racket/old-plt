(unit/sig drscheme:main^
  (import [I : mred:application-imports^]
	  [mred : mred^]
	  [print-convert : mzlib:print-convert^]
	  [drscheme:unit : drscheme:unit^]
	  [drscheme:compound-unit : drscheme:compound-unit^]
	  [drscheme:get/extend : drscheme:get/extend^])
  
  (mred:debug:printf 'invoke "drscheme:main@")

  (mred:current-app-name "DrScheme")
  (mred:add-version-spec 'd 4)
  
  ;; no more extension after this point
  (drscheme:get/extend:get-interactions-canvas%)
  (drscheme:get/extend:get-definitions-canvas%)
  (drscheme:get/extend:get-unit-frame%)
  (drscheme:get/extend:get-interactions-edit%)
  (drscheme:get/extend:get-definitions-edit%)
  
  (print-convert:current-print-convert-hook
   (lambda (expr basic-convert sub-convert)
     (if (is-a? expr wx:image-snip%)
	 expr
	 (basic-convert expr))))

  '(define (make-basic)
    (send (drscheme:compound-unit:make-compound-unit #f)
	  create-frame))

  ;; the initial window doesn't set the 
  ;; unit object's state correctly, yet.
  (define (make-basic)
    (let* ([unit (drscheme:unit:make-unit #f)]
	   [_ (send unit create-frame #f)]
	   [frame (send unit get-frame)])
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
