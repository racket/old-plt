(lambda (request failure-thunk)
  (case request
    [(name) "readline"]
    [(install-collection)
     (lambda (path)
       (parameterize ([current-namespace (make-namespace)]
		      [current-directory (build-path path "collects" "readline")])
	 (global-defined-value 'argv #())
	 (load "mzmake.ss")))]
    [(blurb)
     `("The readline collection provides glue for using GNU's readline library"
       " with the MzScheme read-eval-print-loop.")]
    [else (failure-thunk)]))
