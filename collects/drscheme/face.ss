(unit/sig drscheme:face^
  (import [mred : mred-interfaces^])
  
  (define unit<%> (interface ()
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

  
  (define snip-info<%> (interface ()
			 get-name))

  (define compound-unit<%> (interface (unit<%>)))
  
  (define unit-display<%> (interface ()
			    after-change-name
			    after-remove-import
			    after-remove-export
			    after-add-import
			    after-add-export
			    get-unit))

  (define unit-frame<%> (interface (unit-display<%>)))
  (define unit-snip<%> (interface (unit-display<%>)))
  (define compound-unit-frame<%> (interface (unit-frame<%>)))
  (define compound-unit-snip<%> (interface (unit-snip<%>))))
