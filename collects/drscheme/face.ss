(unit/sig drscheme:face^
  (import [mred : mred^])
  
  (define unitI (interface ()
		  get-filename
		  get-collections

		  get-name
		  set-name

		  get-snips
		  get-frame
		  create-snip
		  create-frame
		  frame-closed
		  remove-snip
		  
		  get-imports
		  get-exports
		  add-import
		  add-export
		  remove-import
		  remove-export))

  
  (define snipInfoI (interface ()
		      get-name))

  (define compound-unitI (interface (unitI)))
  
  (define unit-displayI (interface ()
			  after-change-name
			  after-remove-import
			  after-remove-export
			  after-add-import
			  after-add-export
			  get-unit))

  (define unit-frameI (interface (unit-displayI)))
  (define unit-snipI (interface (unit-displayI)))
  (define compound-unit-frameI (interface (unit-frameI)))
  (define compound-unit-snipI (interface (unit-snipI))))
				