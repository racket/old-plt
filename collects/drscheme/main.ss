(unit/sig drscheme:main^
  (import [I : (program argv)]
	  [fw : framework^]
	  [pretty-print : mzlib:pretty-print^]
	  [print-convert : mzlib:print-convert^]
	  [drscheme:unit : drscheme:unit^]
	  [drscheme:compound-unit : drscheme:compound-unit^]
	  [drscheme:get/extend : drscheme:get/extend^]
	  [basis : userspace:basis^])
  

  ;; add the new settings
  (basis:add-setting 'MrEd (basis:copy-setting (basis:find-setting-named 'MzScheme)))
  (basis:add-setting '|MrEd Debug| (basis:copy-setting (basis:find-setting-named '|MzScheme Debug|)))

  (fw:application:current-app-name "DrScheme")
  (fw:version:add-spec 'd 1)
  
  ;; no more extension after this point
  (drscheme:get/extend:get-interactions-canvas%)
  (drscheme:get/extend:get-definitions-canvas%)
  (drscheme:get/extend:get-unit-frame%)
  (drscheme:get/extend:get-interactions-edit%)
  (drscheme:get/extend:get-definitions-edit%)
  
  '(define (make-basic)
    (send (drscheme:compound-unit:make-compound-unit #f)
	  create-frame))

  ;; the initial window doesn't set the 
  ;; unit object's state correctly, yet.
  (define (make-basic)
    (let* ([unit (drscheme:unit:make-unit #f)]
	   [_ (send unit create-frame #f)]
	   [frame (send unit get-frame)])

      (unless (fw:preferences:get 'drscheme:repl-always-active)
	(let* ([interactions-edit (ivar frame interactions-edit)]
	       [definitions-edit (ivar frame interactions-edit)]
	       [filename (send definitions-edit get-filename)])
	  (unless filename
	    (send interactions-edit reset-console)
	    (send interactions-edit insert-prompt)
	    (send frame update-shown)
	    (send (ivar frame interactions-canvas) focus))))
      (send frame show #t)))

  (let ([files-to-open (reverse (vector->list I:argv))])
    (if (null? files-to-open)
	(make-basic)
	(for-each (lambda (x)
		    (send (drscheme:unit:make-unit x) create-frame))
		  files-to-open))))
