(compound-unit/sig 
  (import [framework : framework^])
  (link
   [url : mred:url^ ((require-unit/sig "url.ss") wx 
		     (minimal constants) (core function@) gui-utils
		     exn (core file@))]
   [hyper-dialog : mred:hyper-dialog^ 
		 ((require-unit/sig "hyprdial.ss") wx 
		  (minimal constants) hyper-edit gui-utils (core file@))]
   [hyper-edit : mred:hyper-edit^ 
	       ((require-unit/sig "hypredit.ss") wx 
		(minimal constants) edit hyper-dialog html url gui-utils
		(core file@) (core string@))]
   [hyper-frame : mred:hyper-frame^ 
		((require-unit/sig "hyprfram.ss") wx 
		 (minimal constants)
		 hyper-edit hyper-dialog (minimal container)
		 frame canvas group find-string
		 preferences handler)]
   [html : mred:html^ ((require-unit/sig "html.ss") wx 
		       (minimal constants)
		       url (core file@)
		       (core string@))])
  (export))
