;; info.ss for mysterx collection

(lambda (request failure-thunk)
  (case request
    [(name) "MysterX"]
    [(compile-prefix) 
     `(begin
	(current-require-relative-collection (list "mysterx"))
	(require-library "macro.ss")
	(require-library "cores.ss")
	(require-relative-library "mysterxu.ss")
	(unless (eq? (system-type) 'windows)
		(begin
		  (fprintf (current-error-port) 
			   "Error: can't install MysterX on non-Windows machine~n")
		  (,failure-thunk)))
	(system "REGSVR32 /s compiled/native/myspage.dll")
	(system "REGSVR32 /s compiled/native/myssink.dll"))]
    [else (failure-thunk)]))
