(unit/sig drscheme:interfaces^
  (import)
  
  (define unitI (interface ()
		  get-snips
		  create-snip
		  get-frame
		  create-frame
		  
		  get-filename

		  add-child
		  add-parent
		  remove-child
		  remove-parent
		  get-children
		  get-parents
		  
		  get-name
		  set-name))
  
  (define compound-unitI (interface (unitI)
			   get-sub-units))
  
  (define unit-displayI (interface ()
			  after-change-name
			  after-remove-import
			  after-remove-export
			  after-add-import
			  after-add-export
			  get-unit))

  (define unit-frameI (interface (unit-displayI)))
  (define unit-snipI (interface (unit-snipI)))
  (define compound-unit-frameI (interface (unit-displayI)))
  (define compound-unit-snipI (interface (unit-snipI))))
				