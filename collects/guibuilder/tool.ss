
(module tool mzscheme
  (require (lib "tool.ss" "drscheme")
           (lib "mred.ss" "mred")
           (lib "unitsig.ss")
           (lib "class.ss")
	   "top-level.ss"
	   "toolbar.ss"
	   "readable.ss")

  (provide tool@)

  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      
      (define (phase1) (void))
      (define (phase2) 
	(drscheme:get/extend:extend-unit-frame 
	 (lambda (drs:frame%)
	   (class drs:frame%
	     (inherit get-special-menu get-edit-target-object)
	     (rename [super-get-definitions/interactions-panel-parent
		      get-definitions/interactions-panel-parent])

	     (define popup (new popup-menu%))
	     (define toolbar #f)

	     (define/override (get-definitions/interactions-panel-parent)
	       (let ([p (super-get-definitions/interactions-panel-parent)])
		 (set! toolbar (new toolbar% [parent p]))
		 (add-tools toolbar popup 
			    (lambda (c%) 
			      (let ([e (get-edit-target-object)])
				(when (e . is-a? . gb:edit%)
				  (send e insert-element c%)))))
		 (new vertical-panel% (parent p))))

	     (super-new)

	     (make-object menu-item% "Insert GUI" (get-special-menu)
			  (lambda (b e)
			    (let ([e (get-edit-target-object)])
			      (when e
				(let ([gb (make-object gb:edit%)])
				  (send gb create-main-panel)
				  (send e insert (make-object gui-code-snip% gb))))))))))))))
