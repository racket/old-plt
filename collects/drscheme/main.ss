(unit/sig drscheme:main^
  (import [i : (program argv)]
	  [fw : framework^]
	  [pretty-print : mzlib:pretty-print^]
	  [print-convert : mzlib:print-convert^]
	  [drscheme:unit : drscheme:unit^]
	  [drscheme:get/extend : drscheme:get/extend^]
	  [basis : userspace:basis^]
	  mzlib:function^)

  (fw:finder:default-extension "scm")

  ;; add the new settings
  (basis:add-setting 
   (let ([s (basis:copy-setting (basis:find-setting-named "MzScheme"))])
     (basis:set-setting-name! s "MrEd")
     s))
  (basis:add-setting 
   (let ([s (basis:copy-setting (basis:find-setting-named "MzScheme Debug"))])
     (basis:set-setting-name! s "MrEd Debug")
     (basis:set-setting-vocabulary-symbol! s 'mred-debug)
     s))

  (fw:application:current-app-name "DrScheme")
  (fw:version:add-spec 'd 10)
  
  
  ;; add preferences
  (fw:preferences:set-default 'drscheme:settings
			       (basis:get-default-setting)
			       basis:setting?)
  (fw:preferences:set-un/marshall 'drscheme:settings
				  (compose cdr vector->list struct->vector)
				  (lambda (x) 
				    (if (and (list? x)
					     (equal? (arity basis:make-setting) (length x)))
					(apply basis:make-setting x)
					(basis:get-default-setting))))


  ;; no more extension after this point
  (drscheme:get/extend:get-interactions-canvas%)
  (drscheme:get/extend:get-definitions-canvas%)
  (drscheme:get/extend:get-unit-frame%)
  (drscheme:get/extend:get-interactions-edit%)
  (drscheme:get/extend:get-definitions-edit%)

  ;; the initial window doesn't set the 
  ;; unit object's state correctly, yet.
  (define (make-basic)
    (let* ([frame (drscheme:unit:open-drscheme-window)])

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

  (let ([files-to-open (reverse (vector->list i:argv))])
    (if (null? files-to-open)
	(make-basic)
	(for-each drscheme:unit:open-drscheme-window files-to-open))))
