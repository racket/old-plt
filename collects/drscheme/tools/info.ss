(letrec ([tools-info
	  (lambda (request failure)
	    (case request
	      [(name) "DrScheme"]
              [(doc-sub-collections)
               (directory-list (collection-path "drscheme" "tools"))]
	      [else (failure)]))])
  tools-info)
