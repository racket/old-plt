(unit/sig drscheme:main^
  (import [i : (program argv)]
	  [fw : framework^]
	  [pretty-print : mzlib:pretty-print^]
	  [print-convert : mzlib:print-convert^]
	  [drscheme:app : drscheme:app^]
	  [drscheme:unit : drscheme:unit^]
	  [drscheme:get/extend : drscheme:get/extend^]
	  [basis : plt:basis^]
	  mzlib:function^)


  (fw:finder:default-extension "scm")

  ;; add the graphical settings
  (basis:add-setting 
   (let ([s (basis:copy-setting (basis:find-setting-named
				 "Textual Full Scheme without Debugging (MzScheme)"))])
     (basis:set-setting-name! s "Graphical Full Scheme without Debugging (MrEd)")
     (basis:set-setting-vocabulary-symbol! s 'mred)
     s)
   3)
  (basis:add-setting 
   (let ([s (basis:copy-setting (basis:find-setting-named
				 "Textual Full Scheme (MzScheme)"))])
     (basis:set-setting-name! s "Graphical Full Scheme (MrEd)")
     (basis:set-setting-vocabulary-symbol! s 'mred-debug)
     s)
   3)

  (fw:application:current-app-name "DrScheme")
  (fw:version:add-spec 'd 9)
  
  
  ;; no more extension after this point
  (drscheme:get/extend:get-interactions-canvas%)
  (drscheme:get/extend:get-definitions-canvas%)
  (drscheme:get/extend:get-unit-frame%)
  (drscheme:get/extend:get-interactions-text%)
  (drscheme:get/extend:get-definitions-text%)

  ;; the initial window doesn't set the 
  ;; unit object's state correctly, yet.
  (define (make-basic)
    (let* ([frame (drscheme:unit:open-drscheme-window)])

      (let* ([interactions-edit (ivar frame interactions-text)]
	     [definitions-edit (ivar frame interactions-text)]
	     [filename (send definitions-edit get-filename)])
	(unless filename
	  (send interactions-edit reset-console)
	  (send interactions-edit insert-prompt)
	  (send frame update-shown)
	  (send (ivar frame interactions-canvas) focus)))
      (send frame show #t)))

  (let ([files-to-open (reverse (vector->list i:argv))])
    (if (null? files-to-open)
	(make-basic)
	(for-each drscheme:unit:open-drscheme-window files-to-open)))


  ;;
  ;; Show about box when version changes
  ;; 

  (fw:preferences:set-default 'drscheme:last-version #f
			      (lambda (x)
				(or (string? x)
				    (not x))))
  (drscheme:app:check-new-version))
