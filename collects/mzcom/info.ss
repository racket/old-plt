;; info.ss for mzcom collection

(lambda (request failure-thunk)
  (case request
    [(name) "MzCOM"]
    [(compile-prefix) 
     (if (not (eq? (system-type) 'windows))
	 (begin
	   (fprintf (current-error-port) 
		    "Error: can't install MzCOM on non-Windows machine~n")
	   (failure-thunk))
	 `(system ".\\mzcom.exe /RegServer"))]
    [else (failure-thunk)]))
